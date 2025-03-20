#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#ORIGINAL FILE AND AUTHORS: REEM_guilds repo R/EBS_guild_temp.R Kerim Aydin (Sept 2021) and Andy Whitehouse (Feb 2023)
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
#R SCRIPT FOR GENERATING GUILD DATA FROM GROUNDFISH SURVEY BIOMASS
#generate biomass-weighted average surface and average bottom temperatures
#from the BT survey
#
# WARNING: this script will not run without the googledrive subfolders.
#------------------------------------------------------------------------------#

# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).
library("tidyverse")
library("janitor")
library("lubridate")
library("here")
library("googledrive")
#googledrive::drive_auth(email = "enter your email here!") #RUN THIS with your e-mail
#READ https://lter.github.io/scicomp/tutorial_googledrive-pkg.html connect googledrive to R

####################################################################

source("code/REEM_fooddata_functions.R")


# Load all Race data and perform some preliminary cleanups/calculations
REEM.loadclean.RACE.googledrive("https://drive.google.com/drive/folders/1nIXG6ydT52bcy-UoEgFSjKYWUXvtx-JA")

# Load strata table (linking models to strata)
REEM.loadclean.strata(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                      stratum_bin_column    = "strat_groups")

# Load lookup file for mapping RACE codes to biomass. Later need to specify
# which column to use for group mapping (varies by ecosystem)
#race_lookup_base <- read.clean.csv("lookups/race_lookup_combined.csv")
race_lookup_base <- read.clean.csv("lookups/race_lookup_base_v2.csv")

race_lookup_col <- c(
  "EBS" = "ebs_ecopath",
  "AI" = "ai_ecopath",
  "WGOA" = "wgoa_ecopath",
  "EGOA" = "egoa_ecopath"
)



##############################################################################
#  get_cpue_all() mirrors the RACE get_cpue function (returning by haul), except it gets
# biomasses for all groups at once, binned by the race_lookup names (race_group column)
# haul_stratum_summary() gets the total stations and area for each model domain.
#
# Note: BS Survey filter (for slope stations) moved to core loading code

#race_lookup_older <- read.clean.csv("lookups/goa_race_lookup_apr_04_2023.csv") %>%
#  select(species_code,final_wgoa)
#
#race_lookup_combined <- race_lookup_base %>%
#  left_join(race_lookup_older,by="species_code")
#
#write.csv(race_lookup_combined,"lookups/race_lookup_combined.csv",row.names=F)


# WGOA GUILDS ------------------------------------------------------------------
this.model  <- "WGOA"
race_lookup      <- race_lookup_base %>% mutate(race_group  = .data[["final_wgoa"]])
q_table          <- read.clean.csv("lookups/GroupQ_2021_GOA.csv")
domains_included <-  c(
  "Chirikof_shelf",
  "Chirikof_gully",
  "Chirikof_slope",
  "Kodiak_shelf",
  "Kodiak_gully",
  "Kodiak_slope",
  "Shumagin_shelf",
  "Shumagin_gully",
  "Shumagin_slope"
)

tot_model_area <- sum(strat_areas$area[strat_areas$model == this.model &
                                         strat_areas$stratum_bin %in% domains_included])
cpue_dat  <- get_cpue_all(model = this.model)
check_RACE_codes(cpue_dat)
cpue_dt <- na.omit(cpue_dat) %>%
  filter(number_fish > 0)



library(Hmisc) 

station_summary <- cpue_dt %>%
  group_by(race_group, stationid) %>%
  summarise(
    station_avg_st = mean(Surface_temp, na.rm = TRUE),
    station_avg_bt  = mean(Bottom_temp, na.rm = TRUE),
    station_catch    = sum(catch_kg, na.rm = TRUE),
    .groups = "drop"
  )

thermal_envelopes <- station_summary %>%
  group_by(race_group) %>%
  summarise(
    # Surface temperature weighted average:
    weighted_avg_surf = sum(station_avg_st * station_catch, na.rm = TRUE) / sum(station_catch, na.rm = TRUE),
    # For min and max, weighted quantiles at 0% and 100% are essentially the extreme values:
    weighted_minA_surf = as.numeric(wtd.quantile(station_avg_st, weights = station_catch, probs = 0, na.rm = TRUE)),
    weighted_maxA_surf = as.numeric(wtd.quantile(station_avg_st, weights = station_catch, probs = 1, na.rm = TRUE)),
    # Weighted 10th and 90th percentiles:
    weighted_minPref_surf = as.numeric(wtd.quantile(station_avg_st, weights = station_catch, probs = 0.1, na.rm = TRUE)),
    weighted_maxPref_surf = as.numeric(wtd.quantile(station_avg_st, weights = station_catch, probs = 0.9, na.rm = TRUE)),
    
    # Bottom temperature statistics:
    weighted_avg_bot = sum(station_avg_bt * station_catch, na.rm = TRUE) / sum(station_catch, na.rm = TRUE),
    weighted_minA_bot = as.numeric(wtd.quantile(station_avg_bt, weights = station_catch, probs = 0, na.rm = TRUE)),
    weighted_maxA_bot = as.numeric(wtd.quantile(station_avg_bt, weights = station_catch, probs = 1, na.rm = TRUE)),
    weighted_minPref_bot = as.numeric(wtd.quantile(station_avg_bt, weights = station_catch, probs = 0.1, na.rm = TRUE)),
    weighted_maxPref_bot = as.numeric(wtd.quantile(station_avg_bt, weights = station_catch, probs = 0.9, na.rm = TRUE)),
    
    # Also record the number of stations and total catch for reference:
    total_stations = n(),
    total_catch = sum(station_catch, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(thermal_envelopes, "WGOA_source_data/species_weighted_thermal_envelopes_WGOA.csv")

#-------------------------------------------------------------------------------




weighted_temps <- cpue_dt %>%
  group_by(race_group, year, stratum_bin) %>%
  summarise(
    weighted_surf_temp = sum(Surface_temp * catch_kg, na.rm = TRUE) / sum(catch_kg, na.rm = TRUE),
    weighted_bot_temp  = sum(Bottom_temp * catch_kg, na.rm = TRUE) / sum(catch_kg, na.rm = TRUE),
    total_catch        = sum(catch_kg, na.rm = TRUE),
    .groups = 'drop'
  )

species_weighted_avg <- weighted_temps %>%
  group_by(race_group, year) %>%
  summarise(
    agg_weighted_surf_temp = sum(weighted_surf_temp * total_catch, na.rm = TRUE) / sum(total_catch, na.rm = TRUE),
    agg_weighted_bot_temp  = sum(weighted_bot_temp * total_catch, na.rm = TRUE) / sum(total_catch, na.rm = TRUE),
    total_catch_year       = sum(total_catch, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  select(year,
         race_group,
         agg_weighted_surf_temp,
         agg_weighted_bot_temp,
         total_catch_year) %>%
  rename(total_catch_year_kg = total_catch_year)

write.csv(species_weighted_avg, "WGOA_source_data/species_weighted_temp_WGOA.csv")




# EGOA GUILDS -----------------------------------------------
this.model.egoa  <- "EGOA"
race_lookup_egoa      <- race_lookup_base %>% mutate(race_group_egoa  = .data[["final_egoa"]])
q_table         <- read.clean.csv("lookups/GroupQ_2021_GOA.csv")
domains_included_egoa <-  c(
  "Southeastern_shelf",
  "Southeastern_slope",
  "Southeastern_gully",
  "Yakutat_shelf",
  "Yakutat_gully",
  "Yakutat_slope"
)

tot_model_area_egoa <- sum(strat_areas$area[strat_areas$model == this.model.egoa &
                                              strat_areas$stratum_bin %in% domains_included_egoa])
cpue_dat_egoa  <- get_cpue_all(model = this.model.egoa)
check_RACE_codes(cpue_dat_egoa)
cpue_dt_egoa <- na.omit(cpue_dat_egoa)

station_summary_egoa <- cpue_dt_egoa %>%
  group_by(race_group, stationid) %>%
  summarise(
    station_avg_st = mean(Surface_temp, na.rm = TRUE),
    station_avg_bt  = mean(Bottom_temp, na.rm = TRUE),
    station_catch    = sum(catch_kg, na.rm = TRUE),
    .groups = "drop"
  )

thermal_envelopes_egoa <- station_summary_egoa %>%
  group_by(race_group) %>%
  summarise(
    # Surface temperature weighted average:
    weighted_avg_surf = sum(station_avg_st * station_catch, na.rm = TRUE) / sum(station_catch, na.rm = TRUE),
    # For min and max, weighted quantiles at 0% and 100% are essentially the extreme values:
    weighted_minA_surf = as.numeric(wtd.quantile(station_avg_st, weights = station_catch, probs = 0, na.rm = TRUE)),
    weighted_maxA_surf = as.numeric(wtd.quantile(station_avg_st, weights = station_catch, probs = 1, na.rm = TRUE)),
    # Weighted 10th and 90th percentiles:
    weighted_minPref_surf = as.numeric(wtd.quantile(station_avg_st, weights = station_catch, probs = 0.1, na.rm = TRUE)),
    weighted_maxPref_surf = as.numeric(wtd.quantile(station_avg_st, weights = station_catch, probs = 0.9, na.rm = TRUE)),
    
    # Bottom temperature statistics:
    weighted_avg_bot = sum(station_avg_bt * station_catch, na.rm = TRUE) / sum(station_catch, na.rm = TRUE),
    weighted_minA_bot = as.numeric(wtd.quantile(station_avg_bt, weights = station_catch, probs = 0, na.rm = TRUE)),
    weighted_maxA_bot = as.numeric(wtd.quantile(station_avg_bt, weights = station_catch, probs = 1, na.rm = TRUE)),
    weighted_minPref_bot = as.numeric(wtd.quantile(station_avg_bt, weights = station_catch, probs = 0.1, na.rm = TRUE)),
    weighted_maxPref_bot = as.numeric(wtd.quantile(station_avg_bt, weights = station_catch, probs = 0.9, na.rm = TRUE)),
    
    # Also record the number of stations and total catch for reference:
    total_stations = n(),
    total_catch = sum(station_catch, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(thermal_envelopes_egoa, "EGOA_source_data/species_weighted_thermal_envelopes_EGOA.csv")

#-------------------------------------------------------------------------------


weighted_temps_egoa <- cpue_dt_egoa %>%
  group_by(race_group, year, stratum_bin) %>%
  summarise(
    weighted_surf_temp = sum(Surface_temp * catch_kg, na.rm = TRUE) / sum(catch_kg, na.rm = TRUE),
    weighted_bot_temp  = sum(Bottom_temp * catch_kg, na.rm = TRUE) / sum(catch_kg, na.rm = TRUE),
    total_catch        = sum(catch_kg, na.rm = TRUE),
    .groups = 'drop'
  )

species_weighted_avg_egoa <- weighted_temps_egoa %>%
  group_by(race_group, year) %>%
  summarise(
    agg_weighted_surf_temp = sum(weighted_surf_temp * total_catch, na.rm = TRUE) / sum(total_catch, na.rm = TRUE),
    agg_weighted_bot_temp  = sum(weighted_bot_temp * total_catch, na.rm = TRUE) / sum(total_catch, na.rm = TRUE),
    total_catch_year       = sum(total_catch, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  select(year,
         race_group,
         agg_weighted_surf_temp,
         agg_weighted_bot_temp,
         total_catch_year) %>%
  rename(total_catch_year_kg = total_catch_year)

write.csv(species_weighted_avg_egoa, "EGOA_source_data/species_weighted_temp_EGOA.csv")


