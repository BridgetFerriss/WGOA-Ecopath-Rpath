#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
#Quick anomaly generation code for primary produuction
#------------------------------------------------------------------------------#



# Load necessary library
library(dplyr)

# Load the dataset (assuming the dataset is in a CSV file)
pp <- read.csv("WGOA_source_data/ROMSOutputWGOA/Long_WGOA_prod_300.csv")

# Calculate the mean of CGOA_prod_Ph
mean_prod_wgoa <- mean(pp$prod_biom_month, na.rm = TRUE)

# Transform CGOA_prod_Ph to be centered at 1 by subtracting the mean and then scaling
pp_trans <- pp %>%
  mutate(wgoa_prod_centered_1 = ((prod_biom_month - mean_prod_wgoa) / mean_prod_wgoa) + 1) 
  
write.csv(pp_trans, "WGOA_source_data/ROMSOutputWGOA/Long_WGOA_prod_300_anommaly.csv")


pp_trans_ssp1 <- pp_trans %>% 
  filter(simulation == "ssp126") %>% 
  select(c(year, month,varname, prod_biom_month)) %>% 
  pivot_wider(names_from =varname, values_from = prod_biom_month) 
pp_trans_ssp1$tstep <- 1:1440
pp_trans_ssp1 <- pp_trans_ssp1 %>% select(tstep, year, month, prod_PhL, prod_PhS)
write.csv(pp_trans_ssp1, "WGOA_source_data/ROMSOutputWGOA/ssp126_wide_WGOA_prod_300_anommaly.csv")

pp_trans_ssp2 <- pp_trans %>% 
  filter(simulation == "ssp245") %>% 
  select(c(year, month,varname, prod_biom_month)) %>% 
  pivot_wider(names_from =varname, values_from = prod_biom_month) 
pp_trans_ssp2$tstep <- 1:1440
pp_trans_ssp2 <- pp_trans_ssp2 %>% select(tstep, year, month, prod_PhL, prod_PhS)
write.csv(pp_trans_ssp2, "WGOA_source_data/ROMSOutputWGOA/ssp245_wide_WGOA_prod_300_anommaly.csv")

pp_trans_ssp5 <- pp_trans %>% 
  filter(simulation == "ssp585") %>% 
  select(c(year, month,varname, prod_biom_month)) %>% 
  pivot_wider(names_from =varname, values_from = prod_biom_month) 
pp_trans_ssp5$tstep <- 1:1440
pp_trans_ssp5 <- pp_trans_ssp5 %>% select(tstep, year, month, prod_PhL, prod_PhS)
write.csv(pp_trans_ssp2, "WGOA_source_data/ROMSOutputWGOA/ssp585_wide_WGOA_prod_300_anommaly.csv")

#Temperature

# Load the dataset (assuming the dataset is in a CSV file)
temp_roms <- read.csv("WGOA_source_data/ROMSOutputWGOA/Long_WGOA_temp_monthly_300.csv") %>% 
  
  
temp_roms_ssp1 <-   temp_roms %>% 
  filter(simulation == "ssp126") %>% 
  select(c(year, month, depthclass, area_weighted_temp)) %>% 
  pivot_wider(names_from =depthclass, values_from = area_weighted_temp) 
temp_roms_ssp1$tstep <- 1:1440
temp_roms_ssp1 <- temp_roms_ssp1 %>% 
  rename(btemp=Bottom, stemp= Surface) %>% 
  select(tstep, year, month, btemp, stemp)
write.csv(temp_roms_ssp1, "WGOA_source_data/ROMSOutputWGOA/ssp126_wide_WGOA_temp_300.csv")

temp_roms_ssp2 <-   temp_roms %>% 
  filter(simulation == "ssp245") %>% 
  select(c(year, month, depthclass, area_weighted_temp)) %>% 
  pivot_wider(names_from =depthclass, values_from = area_weighted_temp) 
temp_roms_ssp2$tstep <- 1:1440
temp_roms_ssp2 <- temp_roms_ssp2 %>% 
  rename(btemp=Bottom, stemp= Surface) %>% 
  select(tstep, year, month, btemp, stemp)
write.csv(temp_roms_ssp1, "WGOA_source_data/ROMSOutputWGOA/ssp245_wide_WGOA_temp_300.csv")

temp_roms_ssp5 <-   temp_roms %>% 
  filter(simulation == "ssp585") %>% 
  select(c(year, month, depthclass, area_weighted_temp)) %>% 
  pivot_wider(names_from =depthclass, values_from = area_weighted_temp) 
temp_roms_ssp5$tstep <- 1:1440
temp_roms_ssp5 <- temp_roms_ssp5 %>%
  rename(btemp=Bottom, stemp= Surface) %>% 
  select(tstep, year, month, btemp, stemp)
write.csv(temp_roms_ssp1, "WGOA_source_data/ROMSOutputWGOA/ssp585_wide_WGOA_temp_300.csv")
