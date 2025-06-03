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


# get_cpue_all() mirrors the RACE get_cpue function (returning by haul), except it gets
# biomasses for all groups at once, binned by the race_lookup names (race_group column)
# haul_stratum_summary() gets the total stations and area for each model domain.
#
# Note: BS Survey filter (for slope stations) moved to core loading code

# WGOA  -----------------------------------------------
this.model  <- "WGOA"
race_lookup      <- race_lookup_base %>% mutate(race_group  = .data[["final_wgoa"]]) #WGOA specific ####
q_table          <- read.clean.csv("lookups/GroupQ_2021_WGOA.csv")

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
    .groups = "keep"
  )  %>%
  left_join(haul_stratum_summary(this.model), 
            by = c("year","model","stratum")) %>%
  mutate(
    # stations comes from the haul_stratum_summary function so includes
    # stations with 0 biomass.
      n_stations= sum(stations),
      mean_wtcpue   = tot_wtcpue / n_stations,
    # CHANGED below line, was missing a division by n-1 same as used in the GAP method###
      var_wtcpue    = (tot_wtcpue2 - ((tot_wtcpue * tot_wtcpue) / n_stations))/(n_stations-1),
    # Convert to variance of mean estimate by dividing by n_stations,
    # replace varest with 0 at this stage if it would be NA (due to 1 station only) 
      varest_wtcpue = ifelse(is.na(var_wtcpue), 0, var_wtcpue/n_stations),
    # Units of wtcpue as returned by get_cpue_all() are kg/km2, convert to t/km2
    # by dividing by 1000, then scale up to tons by multiplying by stratum area.
    # Scale up to tons by stratum area
      bio_tons      = mean_wtcpue   * area / 1000,
      varest_tons   = varest_wtcpue * area * area / (1000*1000)
  )

# Summing these means and variances should match outputs from GAP method
# prior to applying juvenile/adult calculations
model_area <- sum(strata_lookup$area[strata_lookup$model==this.model])

bio_totals <- stratsum %>%
  group_by(year, model, race_group) %>%
  summarize( tot_bio_tons    = sum(bio_tons),
             tot_varest_tons = sum(varest_tons),
             .groups="keep"
           ) %>%
  # This mutation and select function is to rename variables in bio_totals to
  # match the remainder of the code below.
  mutate(bio_tons      = tot_bio_tons,
         bio_tkm2      = bio_tons/model_area,
         se_tkm2       = sqrt(tot_varest_tons)/model_area,
         se_tons       = sqrt(tot_varest_tons),
         var_bio_tons  = tot_varest_tons,
         var_bio_t_km2 = tot_varest_tons/(model_area*model_area),
         cv            = se_tons/bio_tons
         ) %>%
  select(-tot_bio_tons,-tot_varest_tons)

## KYA 5/29/25 Tested above code against GAP surveyindex - bio_totals 
## bio_tons and se_tons match GAP results (to 3 decimal places) for
## Arrowtooth, Big skate, Atka mackerel, and pollock.  Yay!!!


predlist <- read.clean.csv("lookups/Alaska_Multistanza_GOA_vonb_2025_04_30_v2.csv") #WGOA specific ####

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

# KYA 30-May-25 AN ATTEMPT AT STRAIGHT Juv/Adu biomass and se estimation
# from length data 

# Loop through predators and get all the length data, add juv/adu categories
  len_dat <- NULL
  for(p in pred_names){
    len_dat=rbind(len_dat,
                get_stratum_length_cons(predator=p, model=this.model) %>%
                  mutate(jcat=ifelse(lbin==pred_params[[p]]$jsize,"juv","adu"))
    )
  }

# Sum length classes to juv/adu by-haul biomass (biomass from l/w regressions)
 juv_adu_lencons  <- len_dat %>%
   group_by(year, model, species_name, stratum, hauljoin, jcat) %>%
   summarize(haul_wlcpue_tkm2 = sum(tot_wlcpue_t_km2), .groups="keep")

# Go to stratum level calculating variance of estimate per stratum
 juv_adu_strat <- juv_adu_lencons %>%
   group_by(year, model, species_name, stratum, jcat) %>%
   summarize(
     tot_wtlcpue_tkm2  = sum(haul_wlcpue_tkm2),
     tot_wtlcpue2_tkm2 = sum(haul_wlcpue_tkm2 * haul_wlcpue_tkm2),
     .groups = "keep"
   )  %>%
   left_join(haul_stratum_summary(this.model), 
             by = c("year","model","stratum")) %>%
   mutate(
     # stations comes from the haul_stratum_summary function so includes
     # stations with 0 biomass.
     n_stations= sum(stations),
     mean_wtlcpue_tkm2   = tot_wtlcpue_tkm2 / n_stations,
     # CHANGED below line, was missing a division by n-1 ###
     var_wtlcpue_tkm2    = (tot_wtlcpue2_tkm2 - ((tot_wtlcpue_tkm2 * tot_wtlcpue_tkm2) / n_stations))/(n_stations-1),  
     # Convert to variance of mean estimate by dividing by n_stations,
     # replace varest with 0 at this stage if it would be NA (due to 1 station only) 
     varest_wtlcpue_tkm2 = ifelse(is.na(var_wtlcpue_tkm2), 0, var_wtlcpue_tkm2/n_stations),
     # Units of wtcpue as returned by get_cpue_all() are kg/km2, convert to t/km2
     # by dividing by 1000, then scale up to tons by multiplying by stratum area.
     # Scale up to tons by stratum area
     bio_l_tons      = mean_wtlcpue_tkm2   * area,
     varest_l_tons   = varest_wtlcpue_tkm2 * area * area 
   ) 

# sum to ecosystem level
 juvadu_totals <- juv_adu_strat %>%
   group_by(year, model, species_name, jcat) %>%
   summarize( bio_tons    = sum(bio_l_tons),
              varest_tons = sum(varest_l_tons),
              se_tons     = sqrt(varest_tons),
              bio_tkm2    = bio_tons/model_area,
              se_tkm2     = se_tons/model_area,
              cv          = se_tons/bio_tons,
              .groups="keep"
   ) %>% 
   rename(race_group=species_name)
   

 
## AT THIS POINT, ALL CALCULATIONS SHOULD BE DONE, Everything else is
## renaming and formatting.  
## We want to use bio_tons for Value and se_tons for StDev read into Rpath
## (or if preferred bio_tkm2 and se_tkm2, but keeping it in tons is easier to
## compare to other data sources.
## These two files contain the non-stanzas and stanza results before any formatting.
# write.csv(bio_totals,    file="wgoa_data_rpath_fitting/wgoa_race_bio_totals.csv", row.names=FALSE) 
# write.csv(juvadu_totals, file="wgoa_data_rpath_fitting/wgoa_race_juvadu_totals.csv", row.names=FALSE)
 
 
# Now we need to merge the juvenile/adult totals with the stratum totals 
stratsum_jad <- bio_totals %>% 
  left_join(juvadu_totals, by = c("year","model","race_group"))  
#write.csv(stratsum_jad, file="WGOA_source_data/stratsum_jad.csv", row.names=FALSE)
  
## Attach the juv‐prop to every stratum, compute stratum‐level adu/juv bio
  strata_long <- stratsum_jad %>%
    mutate(
      race_group = case_when(
        !is.na(jcat) & jcat == "juv" ~ paste0(race_group, "_juvenile"),
        !is.na(jcat) & jcat == "adu" ~ paste0(race_group, "_adult"),
        TRUE                        ~ race_group
      ),
      bio_tons      = if_else(!is.na(jcat), bio_tons.y,   bio_tons.x),
      se_tons       = if_else(!is.na(jcat), se_tons.y,    se_tons.x),
      bio_tkm2      = if_else(!is.na(jcat), bio_tkm2.y,   bio_tkm2.x),
      se_tkm2       = if_else(!is.na(jcat), se_tkm2.y,    se_tkm2.x),
      cv            = if_else(!is.na(jcat), cv.y,         cv.x),
      var_bio_tons  = if_else(!is.na(jcat), varest_tons,  var_bio_tons)
    ) %>%
    # 2) Drop all the old “.x”/“.y” helper columns, plus varest_tons and jcat
    select(
      -ends_with(".x"),
      -ends_with(".y"),
      -varest_tons,
      -jcat
    ) %>% 
  filter(!race_group %in% c("ZERO", "MISC_NA", "MISC_SHELLS"))

#write.csv(strata_long, file="WGOA_source_data/strata_long.csv", row.names=FALSE)

bio_summary2 <- strata_long %>%
  #group_by(year, model, race_group) %>%
  mutate(total_area = model_area) %>%
  select(
    year,
    model,
    total_area,
    race_group,
    bio_tons,
    var_bio_tons,
    se_tons ,
    cv,
    bio_tkm2,
    var_bio_t_km2,
    se_tkm2,
  )
 
#write.csv(bio_summary2, file="WGOA_source_data/wgoa_race_biomass_ts.csv", row.names=FALSE)

bio_summary2[,"Type"] <- NA
bio_summary2[,"Scale1"] <- 1
bio_summary2[,"Scale2"] <- model_area
bio_summary2[,"Species"] <- ""
bio_summary2[,"SE"] <- NA
bio_summary2[,"Loc"] <- ""
bio_summary2[,"n"] <- ""
bio_summary2[,"Source"] <- "race_wgoa"
#bio_summary2[,"SE"] <- NA
  
bio_summary_v2 <-  bio_summary2 %>% 
  ungroup() %>% 
  select(c(year, race_group, Type, se_tkm2, SE, bio_tkm2, Scale1, cv, Species, Loc, n, Source)) %>% 
  mutate(race_group=case_when(race_group=="Pacific herring"~ 
                                "Pacific herring adult", TRUE~race_group)) #WGOA specific ####

bio_summary_v2$race_group<-make_clean_names(bio_summary_v2$race_group, allow_dupes = TRUE)

 colnames(bio_summary_v2) <- c("Year", "Group", "Type", "Stdev", "SE",
           "Value", "Scale", "CV",  "Species", 
            "Loc", "n", "Source") 
write.csv(bio_summary_v2, file="wgoa_data_rpath_fitting/wgoa_race_biomass_ts_fitting_index_v2_ka.csv", row.names=FALSE)


bio_summary_v2_tons <-  bio_summary2 %>% 
  ungroup() %>% 
  select(c(year, race_group, Type, se_tons, SE, bio_tons, Scale2, cv, Species, Loc, n, Source)) %>% 
  mutate(race_group=case_when(race_group=="Pacific herring"~ 
                                "Pacific herring adult", TRUE~race_group)) #WGOA specific ####

bio_summary_v2_tons$race_group <-make_clean_names(bio_summary_v2_tons$race_group, allow_dupes = TRUE)

colnames(bio_summary_v2_tons) <- c("Year", "Group", "Type", "Stdev", "SE",
                              "Value", "Scale", "CV",  "Species", 
                              "Loc", "n", "Source") 
write.csv(bio_summary_v2_tons, file="wgoa_data_rpath_fitting/wgoa_race_biomass_ts_fitting_index_v2_tons_ka.csv", row.names=FALSE)
