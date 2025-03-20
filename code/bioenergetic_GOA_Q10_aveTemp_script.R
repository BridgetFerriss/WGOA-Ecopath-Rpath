#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
#Script to update the groups names of the bioenergetic_EBS_ACLIM2_bioen.csv file
#to match WGOA names. 
#------------------------------------------------------------------------------#

library(tidyverse)
library(janitor)
library(here)

bioen <- read.csv("data/bioenergetic_EBS_ACLIM2_bioen.csv")
survey_temps <- read.csv("WGOA_source_data/species_weighted_temp_WGOA.csv") %>% 
  filter(year==1990) %>% 
  select(race_group:total_catch_year_kg) %>% 
  rename(Species=race_group, mean_st1990=agg_weighted_surf_temp, mean_bt1990= agg_weighted_bot_temp)

bioen_v2 <- bioen %>%
  mutate(
    Species = case_when(
      Species == "arrowtooth_juv" ~ "Arrowtooth flounder juvenile" ,
      Species == "arrowtooth_adu" ~ "Arrowtooth flounder adult" ,
      Species == "atka"           ~ "Atka mackerel" ,
      Species == "fh_sole"        ~ "Flathead sole adult" ,
      Species == "gr_turbot_juv"  ~ "gr_turbot_juv" ,
      Species == "gr_turbot_adu"  ~ "gr_turbot_adu" ,
      Species == "kamchatka"      ~ "kamchatka" ,
      Species == "north_rockfish" ~ "north_rockfish" ,
      Species == "octopus"        ~ "Octopus" ,
      Species == "oth_flatfish"   ~ "oth_flatfish" ,
      Species == "oth_rockfish"   ~ "oth_rockfish" ,
      Species == "pcod_juv"       ~ "Pacific cod juvenile" ,
      Species == "pcod_adu"       ~ "Pacific cod adult" ,
      Species == "ak_plaice"      ~ "ak_plaice" ,
      Species == "pac_ocean_perch"~ "Pacific ocean perch adult" ,
      Species == "pollock_juv"    ~ "Walleye pollock juvenile" ,
      Species == "pollock_adu"    ~ "Walleye pollock adult" ,
      Species == "nr_sole"        ~ "nr_sole" ,
      Species == "rougheye_rock"  ~ "rougheye_rock" ,
      Species == "sablefish"      ~ "Sablefish adult" ,
      Species == "sharks"         ~ "sharks" ,
      Species == "shortraker_rock"~ "shortraker_rock" ,
      Species == "skates"         ~ "skates" ,
      Species == "yf_sole"        ~ "yf_sole" ,
      Species == "herring"        ~ "Pacific herring adult" ,
      Species == "halibut_juv"    ~ "Pacific halibut juvenile" ,
      Species == "halibut_adu"    ~ "Pacific halibut adult" ,
      Species == "lg_sculpins"    ~ "lg_sculpins" ,
      Species == "squids"         ~ "Squid" ,
      Species == "capelin"        ~ "Pacific capelin" ,
      Species == "sandlance"      ~ "Pacific sandlance"
    )
  ) %>% 
  select(Species, Tmax, Topt, Q10) 
#%>% 
#  full_join(survey_temps, by= join_by(Species)) # not joining because we don't need the mean_bt1990 here 
  

write.csv(bioen_v2, "WGOA_source_data/WGOA_bioen.csv", row.names=FALSE)
                               