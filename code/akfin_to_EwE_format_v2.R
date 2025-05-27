#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# script to format AKFIN data into EwE and Rpath formats
#------------------------------------------------------------------------------#


library(tidyverse)
library(janitor)
library(here)


#wgoacatchsp <- as_tibble(read.csv("data/2023/wgoa_catch_year_group.csv"))


#LOOKUP ####

# loading lookup file and making it ready for the case match
lookup_tbl <- read_csv("lookups/model_species_for_ft_cas_akfin.csv",
                       show_col_types = FALSE) %>%
  drop_na(common_name) %>%
  mutate(
    # strip off life‐stage suffi, here we only have adults (from the lookup), but I decided to include juvenile in the code as well
    wgoa_group_name = str_remove(wgoa_group_name, regex(" (adult|juv)$", ignore_case = TRUE)),
    # normalized strings
    grp_norm = wgoa_group_name %>%
      str_replace_all("[[:punct:]]+", " ") %>%
      str_to_lower() %>% str_squish(),
    cmn_norm = common_name %>%
      str_replace_all("[[:punct:]]+", " ") %>%
      str_to_lower() %>% str_squish()
  )

#FEDERAL ####
catch_data <- as_tibble(read.csv("data/2023/wgoa_catch_year_group_spec_ret_gear.csv")) %>%
  filter(conf_flag == "0")
# preparing the catch_data, here I am taking the punctuation (stringr::) from akfin's query final df
df <- catch_data %>%
  mutate(
    grp_low = species_group_name %>%
      str_replace_all("[[:punct:]]+", " ") %>%
      str_to_lower() %>%
      str_squish(),
    sp_low  = species_name %>%
      str_replace_all("[[:punct:]]+", " ") %>%
      str_to_lower() %>%
      str_squish(),
    assigned_group = NA_character_
  )

n <- nrow(df)

#group‑level matching with singular fallback
for (i in seq_len(nrow(lookup_tbl))) {
  pat       <- lookup_tbl$grp_norm[i]
  pat_sing  <- str_remove(pat, "s$")      # drop trailing "s" if present
  # vector mask: full or singular match
  mask_full <- str_detect(df$grp_low, fixed(pat, ignore_case = TRUE))
  mask_sing <- if (pat_sing != pat) {
    str_detect(df$grp_low, fixed(pat_sing, ignore_case = TRUE))
  } else {
    rep(FALSE, n)
  }
  mask <- (mask_full | mask_sing) & is.na(df$assigned_group)
  df$assigned_group[mask] <- lookup_tbl$wgoa_group_name[i]
}

#creating a token list with all the combination of works (e.g. 'flounder', 'arrowtooth' can be searched in in and out of order)
sp_tokens <- str_split(df$sp_low, "\\s+")

for (i in seq_len(nrow(lookup_tbl))) {
  pat_words <- str_split(lookup_tbl$cmn_norm[i], "\\s+")[[1]]
  mask <- vapply(sp_tokens, function(tok)
    all(pat_words %in% tok), logical(1)) & is.na(df$assigned_group)
  df$assigned_group[mask] <- lookup_tbl$wgoa_group_name[i]
}

# getting rid of my Rfriendly columns
wgoa_catch_ts <- df %>% select(-grp_low, -sp_low) %>%
  na.omit()


# extracting unique values for the lookup_tbl
lookup_unique <- lookup_tbl %>%
  select(c(node, wgoa_group_name)) %>%
  distinct(node, .keep_all = TRUE)


wgoa_catch_ts_v2 <- wgoa_catch_ts %>%
  left_join(lookup_unique, by = c("assigned_group" = "wgoa_group_name")) %>%
  mutate(
    agency_gear_code = case_when(
      agency_gear_code == "NPT" ~ "TRW",
      agency_gear_code == "BTR" ~ "TRW",
      agency_gear_code == "PTR" ~ "TRW",
      agency_gear_code == "HAL" ~ "HAL",
      agency_gear_code == "POT" ~ "POT",
      agency_gear_code == "TRW" ~ "TRW",
      agency_gear_code == "JIG" ~ "JIG"
    )
  ) %>%
  group_by(year,
           assigned_group,
           retained_or_discarded,
           agency_gear_code,
           node) %>%
  summarise(catch_mt = sum(catch_mt))

write.csv(wgoa_catch_ts, "WGOA_source_data/wgoa_catch_ts_long_v2.csv", row.names = FALSE)
#Group, Years, Value, Scale=1, Stdev= Value*0.1
#Value=R+D

# Transform the data to wide format
wgoa_catch_ts_w_mt <- wgoa_catch_ts_v2 %>%
  select(year,
         node,
         assigned_group,
         retained_or_discarded,
         agency_gear_code,
         catch_mt) %>%
  pivot_wider(
    names_from = c(assigned_group, node, retained_or_discarded, agency_gear_code),
    values_from = c(catch_mt)
  )
#write.csv(wgoa_catch_ts_w_mt, "WGOA_source_data/wgoa_fed_catch_ts_wide_mt.csv", row.names = FALSE)

wgoa_catch_ts_w_mtkm2 <- wgoa_catch_ts_v2 %>%
  group_by(year,
           assigned_group,
           retained_or_discarded,
           agency_gear_code,
           node) %>%
  summarise(catch_mtkm2 = catch_mt / 234769, .groups = "drop") %>%
  select(year,
         node,
         assigned_group,
         retained_or_discarded,
         agency_gear_code,
         catch_mtkm2) %>%
  pivot_wider(
    names_from = c(assigned_group, node, retained_or_discarded, agency_gear_code),
    values_from = c(catch_mtkm2),
    names_sort = TRUE
  )

#write.csv(wgoa_catch_ts_w_mtkm2, "WGOA_source_data/wgoa_fed_catch_ts_wide_mtkm2.csv")





# STATE ####
# Lingcod, non-ground, Halibut, salmon, herring, 
catch_data_state <- as_tibble(read.csv("data/2023/wgoa_catch_year_group_spec_ret_gear_state.csv")) %>% 
  filter(harvest_description %in% c("State managed fishery","State managed groundfish",
                                    "Federally managed (Groundfish) "
                                    )) %>%
  filter(conf_flag == "0") %>%  na.omit() %>%
  filter(species_name %in% c("Blk hagfish",
                              "clam, Pacific little-neck",          
                              "crab, Dungeness",
                              "crab, golden (brown) king",
                              "crab, king (general)" ,            
                              "crab, Tanner (general)",
                              "crab, Tanner, bairdi"    ,
                              "crab, Tanner, grooved (tanneri)",  
                              "eels or eel-like fish" ,
                              "flatfish, deep water"  ,
                              "flatfish, shallow water",          
                              "flounder, Alaska plaice",
                              "flounder, general",                
                              "flounder, starry",
                              "greenling, atka mackerel",
                              "grenadier (rattail)",              
                              "groundfish, general" ,
                              "halibut, Pacific",
                              "herring, food" ,                   
                              "herring, Pacific",
                              "jellyfish",
                              "lingcod",                          
                              "octopus, North Pacific",                
                              "prowfish",
                              "rockfish, black",
                              "rockfish, canary",                 
                              "rockfish, china" ,
                              "rockfish, copper",
                              "rockfish, dark",                   
                              "rockfish, dusky",
                              "rockfish, northern",
                              "rockfish, other",                  
                              "rockfish, quillback" ,
                              "rockfish, red" ,
                              "rockfish, redbanded",              
                              "rockfish, redstripe",
                              "rockfish, rosethorn",
                              "rockfish, rougheye",               
                              "rockfish, sharpchin",
                              "rockfish, shortraker" ,
                              "rockfish, silvergray", 
                              "rockfish, tiger",
                              "rockfish, unspecified demersal",   
                              "rockfish, unspecified pelagic",
                              "rockfish, unspecified slope",
                              "rockfish, widow",                  
                              "rockfish, yelloweye (red snapper)",
                              "rockfish, yellowtail" , 
                              "salmon, chinook",
                              "salmon, chum" ,                              
                              "salmon, coho" ,
                              "salmon, groundfish bycatch",
                              "salmon, pink" ,                              
                              "salmon, sockeye",
                              "scallop, weathervane",
                              "sculpin, coastrange",
                              "sculpin, general",
                              "sea cucumber",
                              "sea urchin" ,
                              "shark, other",
                              "shark, spiny dogfish",
                              "smelt, eulachon",
                              "smelt, general",                  
                              "sole, butter" ,
                              "sole, dover"  ,
                              "sole, English", 
                              "sole, rock",                      
                              "sole, sand",
                              "sole, yellowfin",
                              "squid, majestic",                 
                              "turbot, Greenland"))


# Pre-split each lookup pattern into its word‐token sto remove plural
common_tokens <- map(lookup_tbl$cmn_norm, ~ str_split(.x, "\\s+")[[1]])
group_tokens  <- map(lookup_tbl$grp_norm, ~ str_split(.x, "\\s+")[[1]])

#similar than above
df2 <- catch_data_state %>%
  mutate(
    sp_low   = species_name %>%
      str_replace_all("[[:punct:]]+", " ") %>%
      str_to_lower() %>%
      str_squish(),
    # we'll build this next:
    wgoa_group_name = NA_character_,
    # token‐list for each row
    sp_tokens       = str_split(sp_low, "\\s+")
  )

# function for the state (since it has only one column to be matched it required a little more work).
assign_one <- function(tokens) {
  #trying every species‐level (common_name) pattern first
  for (i in seq_along(common_tokens)) {
    if (all(common_tokens[[i]] %in% tokens)) {
      return(lookup_tbl$wgoa_group_name[i])
    }
  }
  #trying every group‐level pattern
  for (i in seq_along(group_tokens)) {
    pat_tok     <- group_tokens[[i]]
    # a) exact group words
    if (all(pat_tok %in% tokens)) {
      return(lookup_tbl$wgoa_group_name[i])
    }
    #singular fallback: drop trailing "s" from each lookup word
    pat_sing_tok <- sub("s$", "", pat_tok)
    if (all(pat_sing_tok %in% tokens)) {
      return(lookup_tbl$wgoa_group_name[i])
    }
  }
  NA_character_
}

wgoa_catch_state_ts <- df2 %>%
  mutate(wgoa_group_name = map_chr(sp_tokens, assign_one)) %>%
  select(-sp_low, -sp_tokens) %>%
  na.omit()

wgoa_catch_state_ts_v2 <- wgoa_catch_state_ts %>%
  left_join(lookup_unique, by = c("wgoa_group_name" = "wgoa_group_name")) %>%
  mutate(harvest_description = case_when(harvest_description == "State managed fishery" ~
                                           "state_ft",
                                         harvest_description == "State managed groundfish" ~
                                           "state_ft",
                                         harvest_description == "Federally managed (Groundfish) "~
                                           "fed_ft")) 
  

colnames(wgoa_catch_state_ts_v2) <- c(
  "year",
  "species_name",
  "agency_gear_code",
  "harvest_description",
  "conf_flag",
  "catch_lb",
  "catch_mt",
  "assigned_group",
  "node"
)

wgoa_catch_state_ts_w_mtkm2 <- wgoa_catch_state_ts_v2 %>%
  group_by(year,
           assigned_group,
           harvest_description,
           agency_gear_code,
           node) %>%
  filter(!assigned_group %in% c("Shallow-water flatfish", "Demersal shelf rockfish", 
                                "Slope rockfish", "Deep-water flatfish", "Pelagic shelf rockfish")) %>% 
  summarise(catch_mt = sum(catch_mt)) %>%
  mutate(catch_mtkm2 = catch_mt / 234769) %>%
  select(year,
         node,
         assigned_group,
         harvest_description,
         agency_gear_code,
         catch_mtkm2) %>%
  pivot_wider(
    names_from = c(assigned_group, node, harvest_description, agency_gear_code),
    values_from = c(catch_mtkm2),
    names_sort = TRUE
  )
#write.csv(wgoa_catch_state_ts_w_mtkm2, "WGOA_source_data/wgoa_state_catch_ts_wide_mtkm2.csv", row.names = FALSE)



# COMBINE FEDERAL AND STATE ####
wgoa_catch_ts_w_mtkm2 <- read.csv("WGOA_source_data/wgoa_fed_catch_ts_wide_mtkm2.csv") %>%
  select(-X) %>%
  mutate(year = as.numeric(year))

wgoa_catch_state_ts_w_mtkm2 <- read.csv("WGOA_source_data/wgoa_state_catch_ts_wide_mtkm2.csv") %>%
  mutate(year = as.numeric(year))

wgoa_catch_ts_w_mtkm2_combined <- wgoa_catch_ts_w_mtkm2 %>%
  full_join(wgoa_catch_state_ts_w_mtkm2, by = c("year")) %>%
  mutate(year = as.numeric(year)) %>%
  select(year, everything()) %>%
  rename_all( ~ str_replace(., "X", "")) %>%
  rename_all( ~ str_replace(., " ", "_")) %>%
  rename_all( ~ str_replace(., "\\.", "_")) %>%
  rename_all( ~ str_replace(., "__", "_")) %>%
  rename_all( ~ str_replace(., "_$", "")) %>%
  rename_all( ~ str_replace(., "^_", ""))
  
wgoa_catches_ft_cas <- wgoa_catch_ts_w_mtkm2_combined %>% 
  select(year, sort(tidyselect::peek_vars()))

#write.csv(wgoa_catches_ft_cas, "WGOA_source_data/wgoa_catches_ft_cas_final_mtkm2.csv", row.names = FALSE)

#dt_1990 <- wgoa_catches_ft_cas %>% 
#  filter(year <= 1993) 
#write.csv(dt_1990, "WGOA_source_data/wgoa_catches_ft_cas_1990_mean.csv", row.names = FALSE)


#Long format for Rpath

wgoa_catch_ts_long <- wgoa_catches_ft_cas %>%
  pivot_longer(
    cols = -year,
    names_to = c("species_name", "node", "harvest_description", "agency_gear_code"),
    names_pattern = "^(.*)_(\\d+)_(.*)_(.*)$",
    values_to  = "catch_mtkm2"
  ) %>%
  # if you want node as an integer:
  mutate(node = as.integer(node)) %>%
  select(year, species_name, node, harvest_description, agency_gear_code, catch_mtkm2) %>% 
  filter(!harvest_description %in% "fed_ft") %>%
  group_by(year,
           species_name,
           node) %>%
  summarise(catch_mtkm2 = sum(catch_mtkm2, na.rm = TRUE)) %>%
  mutate(catch_mtkm2 = na_if(catch_mtkm2, 0))

wgoa_catch_ts_long[,"Stdev"] <- NA
wgoa_catch_ts_long[,"Scale"] <- 1

colnames(wgoa_catch_ts_long) <- c("Year", "Group", "Node", "Value", "Stdev", "Scale")
wgoa_catch_ts_long$Group<-make_clean_names(wgoa_catch_ts_long$Group, allow_dupes = TRUE)

#Values are in t_km2
wgoa_catch_ts_long_v2 <-wgoa_catch_ts_long %>% 
  mutate(Stdev = Value * 0.1) %>% 
  select(Group, Year, Value,  Scale, Stdev, Node) %>% 
  mutate(Group=case_when(Group=="arrowtooth_flounder"~ "arrowtooth_flounder_adult",
                              Group=="rex_sole"~"rex_sole_adult", 
                              Group=="flathead_sole"~ "flathead_sole_adult",
                              Group=="pacific_ocean_perch"~ "pacific_ocean_perch_adult",
                              Group=="sablefish"~ "sablefish_adult",
                              Group=="pacific_cod"~"pacific_cod_adult",
                              Group=="walleye_pollock"~ "walleye_pollock_adult",
                              Group=="pacific_halibut"~ "pacific_halibut_adult",
                              Group=="pacific_herring"~ "pacific_herring_adult",
                              TRUE ~ Group))

write.csv(wgoa_catch_ts_long_v2, "wgoa_data_rpath_fitting/wgoa_catches_ft_cas_long.csv", row.names = FALSE)
