#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#ORIGINAL AUTHORS: Kerim Aydin (Sept 2021) and Andy Whitehouse (Feb 2023)
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
#R SCRIPT FOR GENERATING GUILD DATA FROM GROUNDFISH SURVEY BIOMASS
#generate biomass-weighted average surface and average bottom temperatures 
#from the BT survey
#------------------------------------------------------------------------------#

# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).
library("tidyverse")
library("janitor")
library("lubridate")
library("here")

####################################################################

source("R/REEM_fooddata_functions.R")

# Load all Race data and perform some preliminary cleanups/calculations
REEM.loadclean.RACE(path = "C:/Users/biadias/Documents/data_reemdiets/local_racebase")

# Load strata table (linking models to strata)
REEM.loadclean.strata(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                      stratum_bin_column    = "strat_groups")

# Load lookup file for mapping RACE codes to biomass. Later need to specify
# which column to use for group mapping (varies by ecosystem)
#race_lookup_base <- read.clean.csv("lookups/race_lookup_combined.csv")
race_lookup_base <- read.clean.csv("lookups/race_lookup_base_v2.csv")

race_lookup_col <- c("EBS" = "ebs_ecopath",
                     "AI" = "ai_ecopath",
                     "WGOA" = "wgoa_ecopath",
                     "EGOA" = "egoa_ecopath")



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


# WGOA GUILDS -----------------------------------------------
this.model  <- "WGOA"
race_lookup      <- race_lookup_base %>% mutate(race_group  = .data[["final_wgoa"]])
q_table          <- read.clean.csv("apps/ESR_guilds/GroupQ_2021_GOA.csv")
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

#stratsum_q <- get_stratsum_q(cpue_dat, q_table)
#domain_sum_q <- get_domain_sum_q(cpue_dat, q_table)
# Code to do means and sds
domain_stats <- haul_domain_summary(this.model)


