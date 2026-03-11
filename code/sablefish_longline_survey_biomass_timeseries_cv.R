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

raw <- read_csv("data/sablefish_longline_survey_WCGOA.csv",
                col_names = TRUE)
sablefish_ll <- raw %>%
  clean_names()
  #filter(area == c("Central Gulf of Alaska", "Western Gulf of Alaska")) %>% 
  #select(year, area, rpw_sablefish_ll, cv) %>%
  #pivot_wider(names_from = area, values_from = c(rpw_sablefish_ll, cv))

#sablefish_ll$sablefish_longline <- rowSums(sablefish_ll[,c("rpw_sablefish_ll_Central Gulf of Alaska", 
#                                                          "rpw_sablefish_ll_Western Gulf of Alaska")], 
#                                          na.rm=TRUE) 

#colnames(sablefish_ll)[which(names(sablefish_ll) == "cv_Central Gulf of Alaska")] <- "cv"

#sablefish_ll <- sablefish_ll %>% select(c(year, sablefish_longline, cv))
  
            
          


sablefish_ll[,"Type"] <- NA
sablefish_ll[,"Scale1"] <- 1
sablefish_ll[,"Scale2"] <- 1/234769.3 # WGOA area in km2
sablefish_ll[,"Group"]<- "sablefish_adult"
sablefish_ll[,"Species"] <- ""
sablefish_ll[,"SE"] <- NA
sablefish_ll[,"Stdev"] <- NA
sablefish_ll[,"Loc"] <- ""
sablefish_ll[,"n"] <- ""
sablefish_ll[,"Source"] <- "Longline_survey" #Longline_survey_WGOA_CG_RPW


sablefish_ll_v2 <-  sablefish_ll %>% 
  ungroup() %>% 
  select(c(year, Group, Type, Stdev, SE, sable_ll_wcgoa , Scale2, cv, Species, Loc, n, Source)) 
 

colnames(sablefish_ll_v2) <- c("Year", "Group", "Type", "Stdev", "SE",
                                   "Value", "Scale", "CV",  "Species", 
                                   "Loc", "n", "Source") 

write.csv(sablefish_ll_v2, file="wgoa_data_rpath_fitting/wgoa_sablefish_ll_v2_biomass_ts_fitting_index_v3.csv", row.names=FALSE)
#------------------------------------------------------------------------------#