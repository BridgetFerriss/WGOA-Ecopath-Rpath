#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# script to format non-race groups data. The source data and references are found
#in  METADA_WGOA_MASTERDATA_with_time_series.xlsx
#------------------------------------------------------------------------------#

library(tidyverse)
library(janitor)
library(here)

raw <- read_csv("data/mammals_bird_salmon_time_series_raw_v2.csv",
                col_names = FALSE, 
                col_types = cols(.default = col_character()))

# grabbing the first three rows to use as metadata
sources <- raw %>% slice(1) %>% select(-1) %>% unlist(use.names = TRUE)
group_names <- raw %>% slice(2) %>% select(-1) %>% unlist(use.names = TRUE)
nodes <- raw %>% slice(3) %>% select(-1) %>% unlist(use.names = TRUE)

# droping the first 3 rows and renaming the first column to "year"
data <- raw %>%
  slice(-c(1,2,3)) %>%
  rename(year = X1)

# pivot long
df_long <- data %>%
  pivot_longer(
    cols = -year,
    names_to = "col_id",
    values_to = "value"
  ) %>%
  transmute(
    year = parse_integer(year),
    source = sources[col_id],
    group_name = group_names[col_id] %>% 
      make_clean_names() %>% 
      str_remove("_\\d+$"),
    node = parse_integer(nodes[col_id]),
    value = parse_double(value)
  )


#creating the new columns to match with race biomass timeseries
df_long[,"Type"] <- NA
df_long[,"Scale1"] <- 1
df_long[,"Species"] <- ""
df_long[,"Stdev"] <- NA
df_long[,"SE"] <- NA
df_long[,"cv"] <- NA
df_long[,"Loc"] <- ""
df_long[,"n"] <- ""


bio_summary_non_race <-  df_long %>% 
  ungroup() %>% 
  select(c(year, group_name, Type, Stdev, SE, value, Scale1, cv, Species, Loc, n, source))

#bio_summary_v2$race_group<-make_clean_names(bio_summary_v2$race_group, allow_dupes = TRUE)

colnames(bio_summary_non_race) <- c("Year", "Group", "Type", "Stdev", "SE",
                              "Value", "Scale", "CV",  "Species", 
                              "Loc", "n", "Source") 

# Pedigree ####
# Here we are adding the biomass pedigree to the file. 
# Pedigrees are translated into CVs
ped <- read_csv("wgoa_data_rpath_fitting/wgoa_ewe_pedigree_cv.csv",
                col_names = TRUE) %>% 
  rename(Group = Group_name, CV= Biomass) %>% 
  select(Group, CV)
ped$Group<-make_clean_names(ped$Group, allow_dupes = TRUE)

bio_summary_non_race_ped <- bio_summary_non_race %>% 
  select(-CV) %>% 
  left_join(ped %>% select(Group, CV), 
            by = "Group") %>% 
  select(c(Year, Group, Type, Stdev, SE,
           Value, Scale, CV,  Species, 
           Loc, n, Source)) 



# Write the file ####

write.csv(bio_summary_non_race, 
          file="wgoa_data_rpath_fitting/wgoa_nonrace_biomass_ts_fitting_index.csv", row.names=FALSE)
