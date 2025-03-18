#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#ORIGINAL AUTHORS: Grant Adams and Alberto Rovelinni
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# Script for ROMS data collection
# https://github.com/GOA-CLIM/ROMS_to_Index/blob/main/ROMS%20index%20generation%20example.qmd 
#------------------------------------------------------------------------------#

# Load packages ####

library(kableExtra)
library(stringr)
library(mgcv)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(here)
source("code/Delta_correction.R")

# Read variable names (unchanged)
nep_vars <- read.csv("data/NEP_variable_names.csv")
kable(nep_vars)



# Define regions 
regions <- c("WGOA", "EGOA")
# Define the depth values you want to process
depth_list <- c(300, 1000)

for (region in regions) {
  for (depth in depth_list) {
    
    #### Set area based on region and depth ####
    if (region == "WGOA") {
      if (depth == 300) {
        area_df <- data.frame(
          NMFS_AREA = c("610", "620", "630"),
          area_m2   = c(57225003746, 62597059226, 98582220025)
        )
      } else if (depth == 1000) {
        area_df <- data.frame(
          NMFS_AREA = c("610", "620", "630"),
          area_m2   = c(63986698621, 69583703140, 105918077937)
        )
      }
      region_areas <- c("610", "620", "630")
      out_folder <- "WGOA_source_data/ROMSOutputWGOA"
    } else if (region == "EGOA") {
      if (depth == 300) {
        area_df <- data.frame(
          NMFS_AREA = c("640", "650"),
          area_m2   = c(32560976631, 36726651409)
        )
      } else if (depth == 1000) {
        area_df <- data.frame(
          NMFS_AREA = c("640", "650"),
          area_m2   = c(37270389681, 43952466109)
        )
      }
      region_areas <- c("640", "650")
      out_folder <- "EGOA_source_data/ROMSOutputEGOA"
    }
    
    
  #### Load data (sum) integrated across depth and strata ####
  hind_path <- paste0("Data/NEP_10k_revised_indices/nep_sum_hind_",depth,".csv")
  hist_path <- paste0("Data/NEP_10k_revised_indices/nep_sum_wb_hist_",depth,".csv")
  ssp126_path <- paste0("Data/NEP_10k_revised_indices/nep_sum_wb_ssp126_",depth, ".csv")
  ssp245_path <- paste0("Data/NEP_10k_revised_indices/nep_sum_wb_ssp245_",depth,".csv")
  ssp585_path <- paste0("Data/NEP_10k_revised_indices/nep_sum_wb_ssp585_",depth,".csv")
  
  nep_hind <- read.csv(hind_path) %>%
    filter(!str_detect(date, '1990'))
  nep_hind$simulation <- "hindcast"
  
  nep_hist <- read.csv(hist_path)
  nep_hist$simulation <- "historical"
  
  nep_ssp126 <- read.csv(ssp126_path)
  nep_ssp126$simulation <- "ssp126"
  
  nep_ssp245 <- read.csv(ssp245_path)
  nep_ssp245$simulation <- "ssp245"
  
  nep_585 <- read.csv(ssp585_path)
  nep_585$simulation <- "ssp585"
  
  # Combine data
  roms_sum_data <- do.call(rbind,
                           list(nep_hind, nep_hist, nep_ssp126, nep_ssp245, nep_585))
  
  # Add time and date information
  roms_sum_data <- roms_sum_data %>%
    mutate(date = as_date(date),
           month = month(date),
           year = year(date))
  
  #### Run bias correction for all variables ####
  
  # SSP126
  ssp126_sum_biascorrected <- delta_correction(
    hindcast = roms_sum_data %>% filter(simulation == "hindcast"),
    historical = roms_sum_data %>% filter(simulation == "historical"),
    projection = roms_sum_data %>% filter(simulation == "ssp126"),
    ref_yrs = 1991:2014,
    lognormal = FALSE,
    use_sd = TRUE,
    include_hindcast = TRUE
  )
  
  # SSP245
  ssp245_sum_biascorrected <- delta_correction(
    hindcast = roms_sum_data %>% filter(simulation == "hindcast"),
    historical = roms_sum_data %>% filter(simulation == "historical"),
    projection = roms_sum_data %>% filter(simulation == "ssp245"),
    ref_yrs = 1991:2014,
    lognormal = FALSE,
    use_sd = TRUE,
    include_hindcast = TRUE
  )
  
  # SSP585
  ssp585_sum_biascorrected <- delta_correction(
    hindcast = roms_sum_data %>% filter(simulation == "hindcast"),
    historical = roms_sum_data %>% filter(simulation == "historical"),
    projection = roms_sum_data %>% filter(simulation == "ssp585"),
    ref_yrs = 1991:2014,
    lognormal = FALSE,
    use_sd = TRUE,
    include_hindcast = TRUE
  )
  
  # Combine bias-corrected data for both SSP126 and SSP585
  sum_biascorrected <- bind_rows(
    ssp126_sum_biascorrected %>% mutate(simulation = "ssp126"),
    ssp245_sum_biascorrected %>% mutate(simulation = "ssp245"),
    ssp585_sum_biascorrected %>% mutate(simulation = "ssp585")
  )
  

  
  # Biomass ####
  
  # For biomass, filter data for depthclass "All" and NMFS_AREA 640,650 only
  
  biomass_data <- sum_biascorrected %>%
    filter(depthclass == "All",
           NMFS_AREA %in% region_areas,
           !grepl("prod_", varname)) %>%
    left_join(area_df, by = "NMFS_AREA") %>%
    # Join appropriate area data based on the chosen depth
    left_join(area_df, by = "NMFS_AREA") %>%
    mutate(
      B_ww_mg = case_when(
        varname == "Cop" ~ 10^((log10(value_dc) - (-0.93)) / 0.95),
        varname == "NCa" ~ 10^((log10(value_dc) - (-0.93)) / 0.95),
        varname == "Eup" ~ 10^((log10(value_dc) - (-1.02)) / 1.01),
        varname == "MZL" ~ 10^((log10(value_dc) - (-1.37)) / 0.88),
        varname == "MZS" ~ 10^((log10(value_dc) - (-1.37)) / 0.88),
        varname == "PhL" ~ value_dc * 9,
        varname == "PhS" ~ value_dc * 9
      )
    ) %>%
    drop_na(B_ww_mg) %>%
    mutate(biomass_tonnes = B_ww_mg * 1e-3)
  
  # Summarize biomass per varname, simulation, year, month
  biomass_summary <- biomass_data %>%
    group_by(varname, simulation, year, month) %>%
    summarise(biomass_tonnes = sum(biomass_tonnes),
              .groups = 'drop') %>%
    # Calculate mean annual biomass per varname and simulation
    group_by(varname, simulation, year) %>%
    summarise(mean_biomass = mean(biomass_tonnes),
              .groups = 'drop')
  
#  # Write biomass output
#  write.csv(
#    biomass_summary,
#    paste0(
#      out_folder,
#      "/Long_",
#      region,
#      "_B_summary_",
#      depth,
#      "_v2_corrected.csv"
#    ),
#    row.names = FALSE
#  )
  
  # Production ####
  
  # Filter data for varnames containing 'prod_Ph' (phytoplankton production only), depthclass 'All', and NMFS_AREA 640 and 650
  prod_data <- sum_biascorrected %>%
    filter(depthclass == "All",
           NMFS_AREA %in% region_areas,
           grepl("prod_Ph", varname)) %>%
    mutate(date = as_date(date),
           month = month(date),
           year = year(date)) %>%
    # Apply unit conversions to compute monthly productivity
    mutate(monthly_value = value_dc * 1e-3 * 30.44) %>%  # production data is in C mt*km^-2^month^-1
    group_by(varname, simulation, year, month) %>%
    summarise(prod_biom_month = sum(monthly_value, na.rm = TRUE),
              .groups = 'drop')
  
#  write.csv(
#    prod_data,
#    paste0(out_folder, "/Long_", region, "_prod_", depth, ".csv"),
#    row.names = FALSE
#  )
  
  # Temperature ####
  
  # Load Data and Combine for the given depth
  hind_avg_path <- paste0("Data/NEP_10k_revised_indices/nep_avg_hind_",depth,".csv")
  hist_avg_path <- paste0("Data/NEP_10k_revised_indices/nep_avg_wb_hist_",depth,".csv")
  ssp126_avg_path <- paste0("Data/NEP_10k_revised_indices/nep_avg_wb_ssp126_",depth,".csv")
  ssp245_avg_path <- paste0("Data/NEP_10k_revised_indices/nep_avg_wb_ssp245_",depth,".csv")
  ssp585_avg_path <- paste0("Data/NEP_10k_revised_indices/nep_avg_wb_ssp585_",depth,".csv")
  
  nep_hind_avg <- read.csv(hind_avg_path) %>%
    filter(!str_detect(date, '1990'))
  nep_hind_avg$simulation <- "hindcast"
  
  nep_hist_avg <- read.csv(hist_avg_path)
  nep_hist_avg$simulation <- "historical"
  
  nep_ssp126_avg <- read.csv(ssp126_avg_path)
  nep_ssp126_avg$simulation <- "ssp126"
  
  nep_ssp245_avg <- read.csv(ssp245_avg_path)
  nep_ssp245_avg$simulation <- "ssp245"
  
  nep_ssp585_avg <- read.csv(ssp585_avg_path)
  nep_ssp585_avg$simulation <- "ssp585"
  
  # Combine all datasets into one data frame
  roms_avg_data <- bind_rows(nep_hind_avg,
                             nep_hist_avg,
                             nep_ssp126_avg,
                             nep_ssp245_avg,
                             nep_ssp585_avg) %>%
    mutate(date = as_date(date),
           month = month(date),
           year = year(date))
  
  # Run bias correction for temperature
  ssp126_biascorrected <- delta_correction(
    hindcast = roms_avg_data %>% filter(simulation == "hindcast"),
    historical = roms_avg_data %>% filter(simulation == "historical"),
    projection = roms_avg_data %>% filter(simulation == "ssp126"),
    ref_yrs = 1991:2014,
    lognormal = FALSE,
    use_sd = TRUE,
    include_hindcast = TRUE
  )
  
  
  ssp245_biascorrected <- delta_correction(
    hindcast = roms_avg_data %>% filter(simulation == "hindcast"),
    historical = roms_avg_data %>% filter(simulation == "historical"),
    projection = roms_avg_data %>% filter(simulation == "ssp245"),
    ref_yrs = 1991:2014,
    lognormal = FALSE,
    use_sd = TRUE,
    include_hindcast = TRUE
  )
  
  
  ssp585_biascorrected <- delta_correction(
    hindcast = roms_avg_data %>% filter(simulation == "hindcast"),
    historical = roms_avg_data %>% filter(simulation == "historical"),
    projection = roms_avg_data %>% filter(simulation == "ssp585"),
    ref_yrs = 1991:2014,
    lognormal = FALSE,
    use_sd = TRUE,
    include_hindcast = TRUE
  )
  
  temp_biascorrected <- bind_rows(
    ssp126_biascorrected %>% mutate(simulation = "ssp126"),
    ssp245_biascorrected %>% mutate(simulation = "ssp245"),
    ssp585_biascorrected %>% mutate(simulation = "ssp585")
  )
  
  
  # Filter and compute area-weighted means for temperature data (Surface and Bottom)
  temp_data <- temp_biascorrected %>%
    filter(
      varname == "temp",
      depthclass %in% c("Surface", "Bottom"),
      NMFS_AREA %in% region_areas
    ) %>%
    left_join(area_df, by = "NMFS_AREA") %>%
    mutate(date = as_date(date),
           month = month(date),
           year = year(date))
  
  temp_weighted <- temp_data %>%
    group_by(simulation, depthclass, date, year, month) %>%
    summarise(
      area_weighted_temp = sum(value_dc * area_m2) / sum(area_m2),
      .groups = 'drop'
    )
  
  # Compute annual averages
  temp_annual <- temp_weighted %>%
    group_by(simulation, depthclass, year) %>%
    summarise(annual_avg_temp = mean(area_weighted_temp, na.rm = TRUE),
              .groups = 'drop')
  
  # Save the temperature weighted and annual files
 # write.csv(temp_weighted,
 #           paste0(out_folder, "/Long_", region, "_temp_monthly_", depth, ".csv"),
 #           row.names = FALSE)
 # write.csv(temp_annual,
 #           paste0(out_folder, "/Long_", region, "_temp_annual_", depth, ".csv"),
 #           row.names = FALSE)
 # 
  cat("Processing complete for region:", region, "at depth:", depth, "m\n")
  }
}


# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #
# Plot ####
# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #
for (depth in depth_list) {
  
  # Read in the data needed for plotting
  temp_weighted <- read.csv(paste0("WGOA_source_data/ROMSOutputWGOA/Long_WGOA_temp_monthly_", depth, ".csv"))
  temp_annual <- read.csv(paste0("WGOA_source_data/ROMSOutputWGOA/Long_WGOA_temp_annual_", depth, ".csv"))
  
  # Convert date if needed
  temp_weighted$date <- as.Date(temp_weighted$date)
  
  # Plot the time-series of area-weighted temperature
  p_temp_time <- ggplot(temp_weighted,
                        aes(x = date, y = area_weighted_temp, colour = simulation)) +
    geom_line() +
    facet_wrap(~ depthclass, ncol = 1, scales = "free_y") +
    ylab("Temperature (°C)") +
    xlab("Date") +
    theme_minimal() +
    ggtitle(paste("WGOA Area-Weighted Temperature over Time for Surface and Bottom Depths", depth))
  print(p_temp_time)
  
 # ggsave(paste0("WGOA_source_data/ROMSOutputWGOA/WGOA_temp_time_", depth, ".png"), p_temp_time, width = 10, height = 6)
  
  # Plot the annual average temperature
  p_temp_annual <- ggplot(temp_annual,
                          aes(x = year, y = annual_avg_temp, colour = simulation)) +
    geom_line() +
    facet_wrap(~ depthclass, ncol = 1, scales = "free_y") +
    ylab("Annual Average Temperature (°C)") +
    xlab("Year") +
    theme_minimal() +
    ggtitle(paste("WGOA Annual Average Area-Weighted Temperature for Surface and Bottom Depths", depth))
  
  print(p_temp_annual)
  
 # ggsave(paste0("WGOA_source_data/ROMSOutputWGOA/WGOA_temp_annual_", depth, ".png"), p_temp_annual, width = 10, height = 6)
  
 # cat("Plots saved for depth:", depth, "m\n")
}
