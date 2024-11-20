library(tidyverse)
library(here)
wgoacatchsp <- read.csv("data/2023/wgoa_catch_year_group.csv")
catch_data <- read.csv("data/2023/wgoa_catch_year_group_spec_ret_gear.csv")

Species_nodes <- c(
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "10",
  "11",
  "12",
  "13",
  "14",
  "15",
  "16",
  "17",
  "18",
  "19",
  "20",
  "21",
  "21",
  "22",
  "23",
  "24",
  "25",
  "26",
  "27",
  "28",
  "29",
  "30",
  "31",
  "32",
  "33",
  "33",
  "34",
  "35",
  "35",
  "36",
  "37",
  "37",
  "38",
  "39",
  "39",
  "40",
  "41",
  "42",
  "43",
  "43",
  "44",
  "45",
  "46",
  "47",
  "48",
  "49",
  "49",
  "50",
  "51",
  "51",
  "52",
  "53",
  "53",
  "54",
  "55",
  "56",
  "57",
  "58",
  "59",
  "60",
  "61",
  "62",
  "63",
  "64",
  "65",
  "66",
  "67",
  "68",
  "69",
  "70",
  "71",
  "72",
  "73",
  "74",
  "75",
  "76",
  "77",
  "78",
  "79",
  "80",
  "81",
  "82",
  "83",
  "84",
  "85",
  "86",
  "87"
)
Species_names <- c(
  "Transient killer whales",
  "Resident killer whales",
  "Offshore killer whales",
  "Other toothed whales",
  "Humpback whale",
  "Other baleen whales",
  "Dolphins & porpoises",
  "Northern elephant seal",
  "N. fur seal adult",
  "N.fur seal juv",
  "Steller sea lion",
  "Harbor seal",
  "Sea otter",
  "Piscivorous surface birds",
  "Planktivorous surface birds",
  "Piscivorous diving birds",
  "Planktivorous diving birds",
  "Salmon returning",
  "Salmon outgoing",
  "Pacific hake",
  "Pacific herring",
  "Pacific herring adult",
  "Pacific herring juv",
  "Pacific sandlance",
  "Pacific capelin",
  "Shelf forage fish",
  "Slope forage fish",
  "Pacific dogfish",
  "Salmon shark",
  "Pacific sleeper shark",
  "Big skate",
  "Longnose skate",
  "Other skates",
  "Pacific halibut",
  "Pacific halibut adult",
  "Pacific halibut juv",
  "Arrowtooth flounder",
  "Arrowtooth flounder adult",
  "Arrowtooth flounder juv",
  "Rex sole ",
  "Rex sole adult",
  "Rex sole juv",
  "Flathead sole",
  "Flathead sole adult",
  "Flathead sole juv",
  "Shallow water flatfish",
  "Deep water flatfish",
  "Pacific ocean perch",
  "Pacific ocean perch adult",
  "Pacific ocean perch juv",
  "Slope rockfish",
  "Demersal shelf rockfish",
  "Pelagic shelf rockfish",
  "Thornyheads",
  "Sablefish",
  "Sablefish adult",
  "Sablefish juv",
  "Pacific cod",
  "Pacific cod adult",
  "Pacific cod juv",
  "Walleye pollock",
  "Walleye pollock adult",
  "Walleye pollock juv",
  "Large sculpins",
  "Lingcod",
  "Atka mackerel",
  "Shelf demersal fish",
  "Slope demersal fish",
  "Miscellaneous deep-sea fish",
  "Squid",
  "Octopus",
  "Tanner crab",
  "King crab",
  "Pandalid shrimp",
  "Nonpandalid shrimp",
  "Motile epifauna",
  "Benthic zooplankton",
  "Sessile epifauna",
  "Infauna",
  "Gelatinous carnivores",
  "Other gelatinous zooplankton",
  "Mysids",
  "Pelagic amphipods",
  "Euphausiids",
  "Large copepods",
  "Small copepods",
  "Large microzooplankton",
  "Small microzooplankton",
  "Large phytoplankton",
  "Small phytoplankton",
  "Macroalgae",
  "Benthic microbes",
  "Discards",
  "Offal",
  "Pelagic detritus",
  "Benthic detritus"
)

lookout_data <- data.frame(node = Species_nodes,names = Species_names) %>% 
  filter(!str_detect(names, c('adult|juv|Other'))) 

#write.csv(lookout_data, "data/WGOAfg_lookout.csv")

# match data frames ####

# Ensure columns are character type for matching
catch_data$species_group_name <- as.character(catch_data$species_group_name)
lookout_data$names <- as.character(lookout_data$names)

library(dplyr)
library(stringr)
library(purrr)


# Ensure columns are character type
catch_data <- catch_data %>%
  mutate(species_group_name = as.character(species_group_name))

lookout_data <- lookout_data %>%
  mutate(names = as.character(names))

# Define a helper function for partial matching with NA handling
find_best_match <- function(group_name, lookup_names) {
  if (is.na(group_name)) {
    return(NA_character_) # Return NA if the group name is NA
  }
  matches <- str_detect(lookup_names, fixed(group_name, ignore_case = TRUE))
  if (any(matches, na.rm = TRUE)) {
    return(lookup_names[which(matches)[1]]) # Return the first match
  } else {
    return(NA_character_)
  }
}

# Add a column to `catch_data` for matched names
catch_data <- catch_data %>%
  mutate(
    matched_name = map_chr(species_group_name, ~ find_best_match(.x, lookout_data$names))
  )

# Perform a left join to add data from `lookout_data`
wgoa_catch_ts <- catch_data %>%
  left_join(lookout_data, by = c("matched_name" = "names")) %>% 
  na.omit()
write.csv(wgoa_catch_ts,"wgoa_catch_ts_long.csv" )

# Inspect the resulting dataframe
head(wgoa_catch_ts)


# Transform the data to wide format
wgoa_catch_ts_w <- wgoa_catch_ts %>%
  select(year, node, matched_name,retained_or_discarded, agency_gear_code, catch_mt) %>% 
  pivot_wider(
    names_from = c(matched_name,node, agency_gear_code, retained_or_discarded),
    values_from = c(catch_mt))

wgoa_land_ts_w <- wgoa_catch_ts %>%
  group_by(year, node, matched_name, retained_or_discarded, agency_gear_code) %>%
  summarise(catch_mt = sum(catch_mt, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = c(matched_name, node, agency_gear_code, retained_or_discarded),
    values_from = catch_mt,
    values_fill = 0 # Fill missing values with 0
  )


write.csv(wgoa_land_ts_w, "wgoa_land_ts_wide.csv")


wgoa_catch_ts_w <- wgoa_catch_ts %>%
  group_by(year, node, matched_name, agency_gear_code) %>%
  summarise(catch_mt = sum(catch_mt, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = c(matched_name, node, agency_gear_code),
    values_from = catch_mt,
    values_fill = 0 # Fill missing values with 0
  )
write.csv(wgoa_catch_ts_w, "wgoa_catch_ts_wide.csv")
