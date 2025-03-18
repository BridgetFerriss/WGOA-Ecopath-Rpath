#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# script to generate Multistanzas maturity proportion
#------------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(janitor)


goa_size_comps <- read.csv("data/GOA_BTS-Size-Composition-by-Stratum.csv") %>% 
  clean_names()


#multi_groups_ewe_names <- c("Arrowtooth flounder", "Flathead sole", "Pacific ocean perch", 
#                  "Pacific cod", "Walleye pollock", "Rex sole", "Pacific halibut", 
#                  "Sablefish") # No "Pacific herring" in the data set

goa_size_comps_v2 <- goa_size_comps %>% 
  mutate(common_name= case_when(common_name=="arrowtooth flounder" ~"Arrowtooth flounder",
                                common_name=="flathead sole"       ~"Flathead sole",
                                common_name=="Pacific ocean perch" ~"Pacific ocean perch",
                                common_name=="Pacific cod"         ~"Pacific cod",
                                common_name=="walleye pollock"     ~"Walleye pollock",
                                common_name=="rex sole"            ~"Rex sole",
                                common_name=="Pacific halibut"     ~"Pacific halibut",
                                common_name=="sablefish"           ~"Sablefish")) %>% 
  drop_na(common_name) %>% 
  select(survey, year, common_name, stratum_regulatory_area_name, length_mm, 
         number_of_males, number_of_females, number_unsexed, total_number)

goa_size_comps_v2 <- goa_size_comps_v2 %>%
  filter(
    case_when(
      common_name == "Arrowtooth flounder" ~ length_mm >= 200,
      common_name == "Flathead sole" ~ length_mm >= 200,
      common_name == "Pacific ocean perch" ~ length_mm >= 200,
      common_name == "Pacific cod" ~ length_mm >= 350,
      common_name == "Walleye pollock" ~ length_mm >= 300,
      common_name == "Rex sole" ~ length_mm >= 200,
      common_name == "Pacific halibut" ~ length_mm >= 300,
      common_name == "Sablefish" ~ length_mm >= 400
    )
  )



goa_size_comps_v3 <- goa_size_comps_v2 %>% 
  mutate(number_of_female_mat = case_when(
    common_name == "Arrowtooth flounder" & length_mm < 470 ~ number_of_females * 0,
    common_name == "Arrowtooth flounder" & length_mm >= 470 ~ number_of_females * 1, #from https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/1997/953/zimmermann.pdf
       
    common_name == "Flathead sole" & length_mm < 320 ~ number_of_females * 0,
    common_name == "Flathead sole" & length_mm >= 320 ~ number_of_females * 1, #from https://apps-afsc.fisheries.noaa.gov/refm/reem/lhweb/LifeHistory50Maturity.php?SpeciesID=69
    
    common_name == "Pacific ocean perch" & length_mm < 290 ~ number_of_females * 0,
    common_name == "Pacific ocean perch" & length_mm >= 290 ~ number_of_females * 1, #from https://apps-afsc.fisheries.noaa.gov/refm/reem/lhweb/LifeHistory50Maturity.php?SpeciesID=131 
    
    common_name == "Pacific cod" & length_mm < 510 ~ number_of_females * 0,
    common_name == "Pacific cod" & length_mm >= 600 ~ number_of_females * 1,
    common_name == "Pacific cod" & length_mm >= 470 & length_mm < 600 ~ number_of_females * 0.5, #from https://apps-afsc.fisheries.noaa.gov/
    common_name == "Pacific cod" & length_mm >= 600 & length_mm < 700 ~ number_of_females * 0.9, 
    
    common_name == "Walleye pollock" & length_mm < 400 ~ number_of_females * 0,
    common_name == "Walleye pollock" & length_mm >= 550 ~ number_of_females * 1,
    common_name == "Walleye pollock" & length_mm >= 400 & length_mm < 550 ~ number_of_females * 0.5,
    
    common_name == "Rex sole" & length_mm < 450 ~ number_of_females * 0,
    common_name == "Rex sole" & length_mm >= 500 ~ number_of_females * 1,
    common_name == "Rex sole" & length_mm >= 350 & length_mm < 450 ~ number_of_females * 0.5,
    common_name == "Rex sole" & length_mm >= 450 & length_mm < 500 ~ number_of_females * 0.95,
    
    common_name == "Pacific halibut" & length_mm < 900 ~ number_of_females * 0,
    common_name == "Pacific halibut" & length_mm >= 1400 ~ number_of_females * 1,
    common_name == "Pacific halibut" & length_mm >= 900 & length_mm < 1400 ~ number_of_females * 0.5,
    
    common_name == "Sablefish" & length_mm < 600 ~ number_of_females * 0,
    common_name == "Sablefish" & length_mm >= 800 ~ number_of_females * 1,
    common_name == "Sablefish" & length_mm >= 600 & length_mm < 700 ~ number_of_females * 0.5,
    common_name == "Sablefish" & length_mm >= 700 & length_mm < 800 ~ number_of_females * 0.95)) %>% 
  
  mutate(number_of_male_mat = case_when(
    common_name == "Arrowtooth flounder" & length_mm < 420 ~ number_of_males * 0,
    common_name == "Arrowtooth flounder" & length_mm >= 500 ~ number_of_males * 1, #from https://apps-afsc.fisheries.noaa.gov/refm/reem/lhweb/LifeHistory50Maturity.php?SpeciesID=13
    common_name == "Arrowtooth flounder" & length_mm >= 420 & length_mm < 500 ~ number_of_males * 0.5,
    
    common_name == "Flathead sole" & length_mm < 230 ~ number_of_males * 0,
    common_name == "Flathead sole" & length_mm >= 250 ~ number_of_males * 1,
    common_name == "Flathead sole" & length_mm >= 230 & length_mm < 250 ~ number_of_males * 0.5,
    
    common_name == "Pacific ocean perch" & length_mm < 300 ~ number_of_males * 0,
    common_name == "Pacific ocean perch" & length_mm >= 300 ~ number_of_males * 1,
    
    common_name == "Pacific cod" & length_mm < 420 ~ number_of_males * 0,
    common_name == "Pacific cod" & length_mm >= 500 ~ number_of_males * 1,
    common_name == "Pacific cod" & length_mm >= 420 & length_mm < 500 ~ number_of_males * 0.5, #from https://apps-afsc.fisheries.noaa.gov/
    
    common_name == "Walleye pollock" & length_mm < 35 ~ number_of_males * 0,
    common_name == "Walleye pollock" & length_mm >= 450 ~ number_of_males * 1,
    common_name == "Walleye pollock" & length_mm >= 350 & length_mm < 450 ~ number_of_males * 0.5,
    
    common_name == "Rex sole" & length_mm < 260 ~ number_of_males * 0,
    common_name == "Rex sole" & length_mm >= 300 ~ number_of_males * 1,
    common_name == "Rex sole" & length_mm >= 260 & length_mm < 300 ~ number_of_males * 0.5,
    
    common_name == "Pacific halibut" & length_mm < 700 ~ number_of_males * 0,
    common_name == "Pacific halibut" & length_mm >= 850~ number_of_males * 1,
    common_name == "Pacific halibut" & length_mm >= 700 & length_mm < 850 ~ number_of_males * 0.5,
    
    common_name == "Sablefish" & length_mm < 550 ~ number_of_males * 0,
    common_name == "Sablefish" & length_mm >= 650 ~ number_of_males * 1,
    common_name == "Sablefish" & length_mm >= 550 & length_mm < 650 ~ number_of_males * 0.5)) %>% 
  
  #I STOPPED HERE ####
  mutate(number_of_male_mat = case_when(
    common_name == "Arrowtooth flounder" & length_mm < 420 ~ number_of_males * 0,
    common_name == "Arrowtooth flounder" & length_mm >= 500 ~ number_of_males * 1,
    common_name == "Arrowtooth flounder" & length_mm >= 420 & length_mm < 500 ~ number_of_males * 0.5,
    
    common_name == "Flathead sole" & length_mm < 230 ~ number_of_males * 0,
    common_name == "Flathead sole" & length_mm >= 250 ~ number_of_males * 1,
    common_name == "Flathead sole" & length_mm >= 230 & length_mm < 250 ~ number_of_males * 0.5,
    
    common_name == "Pacific ocean perch" & length_mm < 300 ~ number_of_males * 0,
    common_name == "Pacific ocean perch" & length_mm >= 300 ~ number_of_males * 1,
    
    common_name == "Pacific cod" & length_mm < 450 ~ number_of_males * 0,
    common_name == "Pacific cod" & length_mm >= 500 ~ number_of_males * 1,
    common_name == "Pacific cod" & length_mm >= 450 & length_mm < 500 ~ number_of_males * 0.5,
    
    common_name == "Walleye pollock" & length_mm < 35 ~ number_of_males * 0,
    common_name == "Walleye pollock" & length_mm >= 450 ~ number_of_males * 1,
    common_name == "Walleye pollock" & length_mm >= 350 & length_mm < 450 ~ number_of_males * 0.5,
    
    common_name == "Rex sole" & length_mm < 260 ~ number_of_males * 0,
    common_name == "Rex sole" & length_mm >= 300 ~ number_of_males * 1,
    common_name == "Rex sole" & length_mm >= 260 & length_mm < 300 ~ number_of_males * 0.5,
    
    common_name == "Pacific halibut" & length_mm < 700 ~ number_of_males * 0,
    common_name == "Pacific halibut" & length_mm >= 850~ number_of_males * 1,
    common_name == "Pacific halibut" & length_mm >= 700 & length_mm < 850 ~ number_of_males * 0.5,
    
    common_name == "Sablefish" & length_mm < 550 ~ number_of_males * 0,
    common_name == "Sablefish" & length_mm >= 650 ~ number_of_males * 1,
    common_name == "Sablefish" & length_mm >= 550 & length_mm < 650 ~ number_of_males * 0.5))



