#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# script to format pollock acoustic survey data
#------------------------------------------------------------------------------#

library(tidyverse)
library(janitor)
library(here)

raw <- read_csv("data/pollock_acoustic_survey_winter_shelikof.csv",
                col_names = TRUE, 
                col_types = cols(.default = col_character()))
pollock_shelikof <- raw %>%
  clean_names() %>%
  drop_na()


pollock_shelikof[,"Type"] <- NA
pollock_shelikof[,"Scale1"] <- 1
pollock_shelikof[,"Scale2"] <- 1/234769.3 # WGOA area in km2
pollock_shelikof[,"Group"]<- "walleye_pollock_adult"
pollock_shelikof[,"Species"] <- ""
pollock_shelikof[,"SE"] <- NA
pollock_shelikof[,"Stdev"] <- NA
pollock_shelikof[,"Loc"] <- ""
pollock_shelikof[,"n"] <- ""
pollock_shelikof[,"Source"] <- "shelikof_winter_acoustic_survey"


pollock_shelikof_v2 <-  pollock_shelikof %>% 
  ungroup() %>% 
  select(c(year, Group, Type, Stdev, SE, winter_shelikof_strait_acoustic_survey_pollock_biomass_t, Scale2, cv, Species, Loc, n, Source)) 
 

colnames(pollock_shelikof_v2) <- c("Year", "Group", "Type", "Stdev", "SE",
                                   "Value", "Scale", "CV",  "Species", 
                                   "Loc", "n", "Source") 

write.csv(pollock_shelikof_v2, file="wgoa_data_rpath_fitting/wgoa_pollock_shelikof_v2_biomass_ts_fitting_index_v2_tons_ka.csv", row.names=FALSE)
#------------------------------------------------------------------------------#