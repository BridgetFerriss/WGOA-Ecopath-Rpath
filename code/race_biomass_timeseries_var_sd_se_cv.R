#------------------------------------------------------------------------------#
#AUTHORS: Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# script to download race Biomass from the Bottom Trawl Survey (BTS) data
# Original code from reem_diets repo
# Location of original file: reem_diets/apps/ESR_guilds/ESR_guilds_GOA_EwE.R
#
# WARNING: this script will not run without the googledrive subfolders.
#------------------------------------------------------------------------------#

# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).
library("tidyverse")
library("janitor")
library("lubridate")
library("here")
library("googledrive")

#googledrive::drive_auth(email = "enter your email here!") #RUN THIS with your e-mail
#READ https://lter.github.io/scicomp/tutorial_googledrive-pkg.html connect googledrive to R

source("code/REEM_fooddata_functions.R")


# Load all Race data and perform some preliminary cleanups/calculations
# this takes a long time to run, but we only need to run once
REEM.loadclean.RACE.googledrive("https://drive.google.com/drive/folders/1nIXG6ydT52bcy-UoEgFSjKYWUXvtx-JA")

# Load strata table (linking models to strata)
REEM.loadclean.strata.by.stratum(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                                 stratum_bin_column    = "stratum") # here we are using the individual stratum instead stratum groups


# Load lookup file for mapping RACE codes to biomass. Later need to specify
# which column to use for group mapping (varies by ecosystem)
#race_lookup_base <- read.clean.csv("lookups/race_lookup_combined.csv")
race_lookup_base <- read.clean.csv("lookups/race_lookup_base_v2.csv")

race_lookup_col <- c("EBS" = "ebs_ecopath",
                     "AI" = "ai_ecopath",
                     "WGOA" = "wgoa_ecopath",
                     "EGOA" = "egoa_ecopath")


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
q_table          <- read.clean.csv("lookups/GroupQ_2021_GOA.csv")
#domains_included <-  c(
#  "Chirikof_shelf",
#  "Chirikof_gully",
#  "Chirikof_slope",
#  "Kodiak_shelf",
#  "Kodiak_gully",
#  "Kodiak_slope",
#  "Shumagin_shelf",
#  "Shumagin_gully",
#  "Shumagin_slope"
#)

#Explanation of the domains included: Here we are using the stratum bins instead of the full domain, since with have higher confidence in the data for doing so.
#However, for temperature guilds and diet we are going with the main domains. 

#domains_included <-  c("20", "21", "22", "121", "122",    #  "Chirikof_shelf" 
#                       "120", "220",                      #  "Chirikof_gully"
#                       "221", "320", "420", "520",        #  "Chirikof_slope"
#                       "130", "230", "232",               #  "Kodiak_shelf"
#                       "30", "31","32", "33", "35","131", "132", "133", "134", #  "Kodiak_gully"
#                       "231", "330", "430", "530",        #  "Kodiak_slope"
#                       "10", "11", "12", "13","111",      #  "Shumagin_shelf"
#                       "110", "112",                      #  "Shumagin_gully"
#                       "210", "310", "410", "510"         #  "Shumagin_slope"
#)

#n_stratum <- length(domains_included)

tot_model_area <- sum(strat_areas$area[strat_areas$model == this.model &
                                         strat_areas$stratum_bin %in% domains_included])
#cpue_dat  <- get_cpue_all(model = this.model)
#check_RACE_codes(cpue_dat)


# Build stratum‐level biomass
stratsum <- get_cpue_all(model = this.model) %>%
  group_by(year, model, race_group, stratum, hauljoin) %>%
  summarize(
    haul_wgtcpue = sum(wgtcpue),
    haul_numcpue = sum(numcpue),
    .groups = "keep"
  )%>%
  group_by(year, model, race_group, stratum) %>%
  summarize(
    tot_wtcpue  = sum(haul_wgtcpue),
    tot_wtcpue2 = sum(haul_wgtcpue * haul_wgtcpue),
    #squared needed for var calculation
    .groups = "keep"
  )  %>%
  left_join(haul_stratum_summary(this.model),
            by = c("year","model","stratum")) %>%
  mutate(
    mean_wtcpue   = tot_wtcpue / stations,
    var_wtcpue    = tot_wtcpue2 / stations - mean_wtcpue * mean_wtcpue,
    bio_t_km2     = mean_wtcpue / 1000,
    #WHY the cpue to mt/km2 conversion is 1:1000? 
    #units are kg/hectare and are transformed by the GAP_get_cpue.R function. 
    #Here we are doing the final transformation from kg to mt
    bio_tons      = bio_t_km2 * area,
    var_bio_t_km2 = var_wtcpue / (1000 * 1000),
    var_bio_tons  = var_wtcpue * (area / 1000) * (area / 1000)
  )

predlist <- read.clean.csv("lookups/Alaska_Multistanza_GOA_vonb_2025_04_30_v2.csv")
#race_lookup <- read.clean.csv("lookups/race_lookup_base_v2.csv") %>% 
#mutate(race_group  = .data[["final_wgoa"]])

preds      <- predlist %>% filter(model==this.model)
pred_names <- unique(preds$predator)
pred_params=list()
for (p in pred_names){
  pdat <- as.list(preds[preds$predator==p,])
  pred_params[[p]] <- pdat
  pred_params[[p]]$LCLASS <- sort(unique(c(0,pdat$juv_cm, pdat$adu_1, pdat$adu_2, pdat$adu_3,9999)))
  pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
  pred_params[[p]]$lw_b   <- pdat$b_l_mm_g
  pred_params[[p]]$lw_a   <- pdat$a_l_mm_g*(10^pdat$b_l_mm_g)  
  pred_params[[p]]$bioen  <- list(CA=pdat$ca, CB=pdat$cb, C_TM=pdat$c_tm, C_T0=pdat$c_t0, C_Q=pdat$c_q)
  pred_params[[p]]$vonb <- list(h= pdat$vb_k, Linf  = pdat$vb_linf_mm, t0= pdat$vb_t0, rec_len=pdat$von_b_rec_len_cm)
}

model_area <- sum(strata_lookup$area[strata_lookup$model==this.model])
bio_totals <- stratsum %>%
  group_by(year, model, race_group) %>%
  summarize(bio_tons = sum(bio_tons),
            bio_tkm2 = bio_tons/model_area,
            var_bio_tons = sum(var_bio_tons),
            var_bio_t_km2= sum(var_bio_t_km2),
            .groups="keep")

# Juvenile and Adult bio proportions using Kerim's get_stratum_length_cons
juv_combined <- NULL
for (p in pred_names){
  #p <- pred_names[1]
  bio_pred <- bio_totals %>%
    filter(race_group==p)
  
  juv_adu_lencons  <- get_stratum_length_cons(predator=p, model=this.model) %>%
    group_by(year, model, species_name, stratum, lbin) %>%
    summarize(strat_bio_sum = sum(tot_wlcpue_t_km2), .groups="keep") %>%
    left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
    mutate(bio_t_km2 = strat_bio_sum/stations,
           bio_tons  = bio_t_km2 * area, #area for each stratum/bins
           jcat      = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu")) 
  
  
  juv_proportions <- juv_adu_lencons %>%
    group_by(year,model,species_name,jcat) %>%
    summarize(bio_tons = sum(bio_tons), .groups="keep") %>%
    pivot_wider(names_from=jcat, values_from=bio_tons) %>%
    mutate(juv_bio_prop = juv/(juv+adu))

  juv_combined <- bind_rows(juv_combined, juv_proportions)
}
  

juv_props <- juv_combined %>%
  rename(race_group = species_name)

# Attach the juv‐prop to every stratum, compute stratum‐level adu/juv bio
stratsum_jad <- stratsum %>%
  left_join(juv_props, by = c("year","model","race_group")) %>%
  # wherever a species had no juvs, we’ll get NA; that’s okay
  mutate(
    juv_bio_prop = coalesce(juv_bio_prop, 0),
    juv_bio_tkm2 = bio_t_km2 * juv_bio_prop,
    adu_bio_tkm2 = bio_t_km2 * (1 - juv_bio_prop),
    juv_bio_tons = bio_tons * juv_bio_prop,
    adu_bio_tons = bio_tons * (1 - juv_bio_prop)
  )
#write.csv(stratsum_jad, file="WGOA_source_data/stratsum_jad.csv", row.names=FALSE)

strata_long <- stratsum_jad %>%
  select(-bio_tons) %>% 
  pivot_longer(  cols       = c(juv_bio_tkm2, adu_bio_tkm2,
                                juv_bio_tons, adu_bio_tons),
                 names_to   = c("stanza", ".value"),
                 names_pattern = "(juv|adu)_(bio_tkm2|bio_tons)"
  )  %>%
  filter(!bio_tkm2==0) %>% 
  select(-juv, -adu) %>% 
  filter(!race_group %in% c("ZERO", "MISC_NA", "MISC_SHELLS")) %>%
  mutate(race_group=case_when((race_group=="Pacific halibut" & stanza== "juv")~ 
                                "Pacific halibut juvenile",
                              (race_group=="Arrowtooth flounder" & stanza== "juv")~ 
                                "Arrowtooth flounder juvenile",
                              (race_group=="Rex sole" & stanza== "juv")~ 
                                "Rex sole juvenile", 
                              (race_group=="Flathead sole" & stanza== "juv")~ 
                                "Flathead sole juvenile",
                              (race_group=="Pacific ocean perch" & stanza== "juv")~ 
                                "Pacific ocean perch juvenile",
                              (race_group=="Sablefish" & stanza== "juv")~ 
                                "Sablefish juvenile",
                              (race_group=="Pacific cod" & stanza== "juv")~ 
                                "Pacific cod juvenile",
                              (race_group=="Walleye pollock" & stanza== "juv")~ 
                                "Walleye pollock juvenile",
                              (race_group=="Pacific halibut" & stanza== "adu")~ 
                                "Pacific halibut adult",
                              (race_group=="Arrowtooth flounder" & stanza== "adu")~ 
                                "Arrowtooth flounder adult",
                              (race_group=="Rex sole" & stanza== "adu")~ 
                                "Rex sole adult", 
                              (race_group=="Flathead sole" & stanza== "adu")~ 
                                "Flathead sole adult",
                              (race_group=="Pacific ocean perch" & stanza== "adu")~ 
                                "Pacific ocean perch adult",
                              (race_group=="Sablefish" & stanza== "adu")~ 
                                "Sablefish adult",
                              (race_group=="Pacific cod" & stanza== "adu")~ 
                                "Pacific cod adult",
                              (race_group=="Walleye pollock" & stanza== "adu")~ 
                                "Walleye pollock adult",
                              TRUE ~ race_group)) %>% 
  select(c(-stanza,-bio_t_km2,-bottom_temp_mean,-surface_temp_mean))

#write.csv(strata_long, file="WGOA_source_data/strata_long.csv", row.names=FALSE)

#QUESTION: what to do with years that have only one station? #####
#BIA YOU ARE HERE ####
# You have to FIX the bio_mt and bio_mt_km2 calculations
bio_summary2 <- strata_long %>%
  group_by(year, model, race_group) %>%
  summarise(
    n_stations   = n(),
    total_area   = first(model_area),             # grab the one model_area per group
    bio_mt       = sum(bio_tons),
    bio_mt_km2   = bio_mt / total_area,
    var_mt_km2  = sum((area/total_area)^2 * var_bio_t_km2, na.rm = TRUE),
    sd_mt_km2   = sqrt(var_mt_km2),
    cv_mt_km2   = sd_mt_km2 / bio_mt_km2,
    
    # 2) total‐biomass variance & CV
    #var_tons     = sum(var_bio_t_km2 * area^2, na.rm = TRUE),
    #sd_tons      = sqrt(var_tons),
    #cv_tons      = sd_tons / bio_mt,
    .groups = "drop"
  ) %>%
  select(year, model, race_group,total_area,
         bio_mt, bio_mt_km2,
         var_mt_km2, sd_mt_km2, cv_mt_km2)


#write.csv(bio_summary, file="WGOA_source_data/wgoa_race_biomass_ts.csv", row.names=FALSE)

bio_summary2[,"Type"] <- NA
bio_summary2[,"Scale"] <- 1
bio_summary2[,"Species"] <- ""
bio_summary2[,"Loc"] <- ""
bio_summary2[,"n"] <- ""
bio_summary2[,"Source"] <- "race_wgoa"
bio_summary2[,"SE"] <- NA
  
bio_summary_v2 <-  bio_summary2 %>% 
  select(c(year, race_group, Type, sd_mt_km2, SE, bio_mt_km2, Scale, cv_mt_km2, Species, Loc, n, Source)) %>% 
  mutate(race_group=case_when(race_group=="Pacific herring"~ 
                                "Pacific herring adult", TRUE~race_group))

bio_summary_v2$race_group<-make_clean_names(bio_summary_v2$race_group, allow_dupes = TRUE)

 colnames(bio_summary_v2) <- c("Year", "Group", "Type", "Stdev", "SE", 
           "Value", "Scale", "CV",  "Species", 
            "Loc", "n", "Source") 
write.csv(bio_summary_v2, file="wgoa_data_rpath_fitting/wgoa_race_biomass_ts_fitting_index_v2.csv", row.names=FALSE)
