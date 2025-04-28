#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# script to convert pedigree formats from Rpath to EwE
#------------------------------------------------------------------------------#


library(tidyverse)
library(here)

# Pedigree file updated on 4/28/2025
GOA_ped<- read.csv("data/wgoa_rpath_pedigree.csv")

GOA_ped_v2 <- GOA_ped %>%
  mutate(Biomass = case_when(Biomass =="8"~ "1",
                             Biomass =="7"~ "2",
                             Biomass =="6"~ "2",
                             Biomass =="5"~ "3",
                             Biomass =="4"~ "4",
                             Biomass =="3"~ "4",
                             Biomass =="2"~ "5",
                             Biomass =="1"~ "6")) %>% 
  mutate(Diet =    case_when(Diet =="8"~ "1",
                             Diet =="7"~ "2",
                             Diet =="6"~ "2",
                             Diet =="5"~ "3",
                             Diet =="4"~ "4",
                             Diet =="3"~ "4",
                             Diet =="2"~ "5",
                             Diet =="1"~ "6")) %>% 
  mutate(Catch =   case_when(Catch =="8"~ "1",
                             Catch =="7"~ "2",
                             Catch =="6"~ "2",
                             Catch =="5"~ "3",
                             Catch =="4"~ "4",
                             Catch =="3"~ "4",
                             Catch =="2"~ "5",
                             Catch =="1"~ "6")) %>% 
  mutate(Production_biomass=
                   case_when(Production_biomass  =="8"~ "1",
                             Production_biomass  =="7"~ "2",
                             Production_biomass  =="6"~ "3",
                             Production_biomass  =="5"~ "4",
                             Production_biomass  =="4"~ "5",
                             Production_biomass  =="3"~ "6",
                             Production_biomass  =="2"~ "7",
                             Production_biomass  =="1"~ "8")) %>% 
  mutate(Consumption_biomass=
           case_when(        Consumption_biomass =="8"~ "1",
                             Consumption_biomass =="7"~ "2",
                             Consumption_biomass =="6"~ "3",
                             Consumption_biomass =="5"~ "4",
                             Consumption_biomass =="4"~ "5",
                             Consumption_biomass =="3"~ "6",
                             Consumption_biomass =="2"~ "7",
                             Consumption_biomass =="1"~ "8"))
write.csv(GOA_ped_v2,"WGOA_source_data/wgoa_ewe_pedigree.csv") 
# Pedigree file updated on 4/28/2025