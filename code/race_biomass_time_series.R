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

domains_included <-  c("20", "21", "22", "121", "122",    #  "Chirikof_shelf" 
                       "120", "220",                      #  "Chirikof_gully"
                       "221", "320", "420", "520",        #  "Chirikof_slope"
                       "130", "230", "232",               #  "Kodiak_shelf"
                       "30", "31","32", "33", "35","131", "132", "133", "134", #  "Kodiak_gully"
                       "231", "330", "430", "530",        #  "Kodiak_slope"
                       "10", "11", "12", "13","111",      #  "Shumagin_shelf"
                       "110", "112",                      #  "Shumagin_gully"
                       "210", "310", "410", "510"         #  "Shumagin_slope"
)


tot_model_area <- sum(strat_areas$area[strat_areas$model == this.model &
                                         strat_areas$stratum_bin %in% domains_included])
cpue_dat  <- get_cpue_all(model = this.model)
check_RACE_codes(cpue_dat)

#stratsum_q <- get_stratsum_q(cpue_dat, q_table)
#domain_sum_q <- get_domain_sum_q(cpue_dat, q_table)
# Code to do means and sds
domain_stats <- haul_domain_summary(this.model)

haul_sum <- cpue_dat %>%
  group_by(year, model, race_group, stratum_bin, hauljoin) %>%
  summarize(
    haul_wgtcpue = sum(wgtcpue),
    haul_numcpue = sum(numcpue),
    .groups = "keep"
  )

domain_sum <- haul_sum %>%
  group_by(year, model, race_group, stratum_bin) %>%
  summarize(
    tot_wtcpue  = sum(haul_wgtcpue),
    tot_wtcpue2 = sum(haul_wgtcpue * haul_wgtcpue),
    #squared needed for var calculation
    .groups = "keep"
  ) %>%
  left_join(domain_stats, by = c("year", "model", "stratum_bin")) %>%
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

model_sum <- domain_sum %>%
  group_by(year, model, race_group) %>%
  summarize(
    bio_tons     = sum(bio_tons),
    var_bio_tons = sum(var_bio_tons),
    .groups = "keep"
  ) %>%
  mutate(
    sd_bio_tons      = sqrt(var_bio_tons),
    cv_bio           = sd_bio_tons / bio_tons,
    model_area       = tot_model_area,
    bio_tons_km2     = bio_tons / model_area,
    var_bio_tons_km2 = var_bio_tons / (model_area * model_area),
    sd_bio_tons_km2  = sqrt(var_bio_tons_km2),
    cv_bio_tons_km2  = sd_bio_tons_km2 / bio_tons_km2,
  ) %>% 
  filter(!race_group %in% c("ZERO", "MISC_NA", "MISC_SHELLS")) %>% 
  ungroup() %>%
  # expand to all years 1990–2023 for each model × race_group
  complete(
    year       = 1990:2023,
    model,
    race_group
  ) %>%
  group_by(model, race_group) %>%
  fill(model_area, .direction = "down") %>%
  ungroup()



predlist <- read.clean.csv("lookups/Alaska_Multistanza_GOA_vonb_2025_04_30_v2.csv")
#race_lookup <- read.clean.csv("lookups/race_lookup_base_v2.csv") %>% 
#  mutate(race_guild  = .data[["final_wgoa"]])

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




#Getting biomass-at-length and converting to split pool biomass
# Loop through each predator.  the juv_adu_lencons splits out
# length samples based on the length class divisions in the
# predator lookup, and converts to % by weight using the a and b LW
# regression.  This % by weight is applied to the total biomass
# for the predator calculated above, to calculate biomass in
# each length class.  It is output into bio_combined as two biomass groupings
# (juv and adu) where adu is the sum of all the adu size classes.


bio_totals <- domain_sum %>%
  group_by(year, model, race_group) %>%
  summarize(bio_tons = sum(bio_tons),
            bio_tkm2 = bio_tons/tot_model_area, .groups="keep")

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
           bio_tons  = bio_t_km2 * area,
           jcat      = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu")) 
  
  juv_proportions <- juv_adu_lencons %>%
    group_by(year,model,species_name,jcat) %>%
    summarize(bio_tons = sum(bio_tons), .groups="keep") %>%
    pivot_wider(names_from=jcat, values_from=bio_tons) %>%
    mutate(juv_bio_prop = juv/(juv+adu))
  
  juv_combined <- rbind(juv_combined,juv_proportions)
} # end of pred_names loop

bio_with_juvs <- bio_totals %>%
  left_join(juv_combined,by=c("year","model","race_group"="species_name")) %>%
  select(-c(juv,adu)) %>%
  mutate(juv_bio_prop  = replace_na(juv_bio_prop,0.0),
         adu_bio_tkm2  = (1.0-juv_bio_prop) * bio_tkm2,
         juv_bio_tkm2  = juv_bio_prop       * bio_tkm2)

bio_combined <- NULL
bio_combined <- rbind(bio_combined,bio_with_juvs)


write.csv(bio_combined,
          "WGOA_source_data/WGOA_groundfish_bio_juv_adu.csv",
          row.names = F)

#BIA YOU ARE HERE ####


#wide format
wgoa_race_b <- model_sum %>% 
  select(year,model,race_group,bio_tons_km2,cv_bio_tons_km2) %>%
  rename(mtkm2 = bio_tons_km2,
         cv_mtkm2 = cv_bio_tons_km2) %>%
  pivot_wider(names_from = race_group,
              values_from = c(mtkm2,cv_mtkm2),
              names_sep = "_", 
              names_glue = "{race_group}_{.value}",
              names_vary = 'slowest') #tidyr thing. 
  



# Following code does means but not sds
#  domain_sum <- cpue_dat %>%
#    group_by(year, model, race_group, stratum_bin) %>%
#    summarize(wgtcpue = sum(wgtcpue),
#              numcpue = sum(numcpue),.groups="keep") %>%
#    left_join(haul_domain_summary(this.model), by=c("year","model","stratum_bin")) %>%
#    mutate(bio_t_km2 = wgtcpue/1000/stations,
#           bio_tons  = bio_t_km2 * area)
#
#  model_sum <- domain_sum %>%
#    group_by(year, model, race_group) %>%
#    summarize(tot_bio_tons = sum(bio_tons),
#              tot_bio_tkm2 = sum(bio_tons)/tot_model_area,
#      .groups="keep")

write.csv(wgoa_race_b,
          "WGOA_source_data/wgoa_groundfish_bio_mt_km2_wide.csv",
          row.names = F)


#cpue_test <- cpue_dat %>%
#  filter(year==1982 & stratum==10 & race_group =="ak_plaice")
# These two lines then sum stratsum to the total biomass and biomass density
# for the entire model area
#model_area <- sum(strata_lookup$area[strata_lookup$model==this.model])
#bio_totals <- stratsum %>%
#  group_by(year, model, race_group) %>%
#  summarize(bio_tons = sum(bio_tons),
#            bio_tkm2 = bio_tons/model_area, .groups="keep")

# IF Looping through multiple ecosystems, need to save or rbind bio_totals
# at the end of each loop here.
#}

#write.csv(bio_totals,"apps/ESR_guilds/bio_totals_2024test.csv",row.names=F)


# EGOA GUILDS -----------------------------------------------
this.model.e  <- "EGOA"
race_lookup      <- race_lookup_base %>% mutate(race_group  = .data[["final_egoa"]])
q_table          <- read.clean.csv("lookups/GroupQ_2021_GOA.csv")
domains_included_e <-  c(
  "Southeastern_shelf",
  "Southeastern_slope",
  "Southeastern_gully",
  "Yakutat_shelf",
  "Yakutat_gully",
  "Yakutat_slope"
)

tot_model_area_e <- sum(strat_areas$area[strat_areas$model == this.model.e &
                                           strat_areas$stratum_bin %in% domains_included_e])
cpue_dat_e  <- get_cpue_all(model = this.model.e) %>% 
  drop_na(race_group)

check_RACE_codes(cpue_dat_e)

#stratsum_q <- get_stratsum_q(cpue_dat, q_table)
#domain_sum_q <- get_domain_sum_q(cpue_dat, q_table)
# Code to do means and sds
domain_stats_e <- haul_domain_summary(this.model.e)

haul_sum_e <- cpue_dat_e %>%
  group_by(year, model, race_group, stratum_bin, hauljoin) %>%
  summarize(
    haul_wgtcpue = sum(wgtcpue),
    haul_numcpue = sum(numcpue),
    .groups = "keep"
  )

domain_sum_e <- haul_sum_e %>%
  group_by(year, model, race_group, stratum_bin) %>%
  summarize(
    tot_wtcpue  = sum(haul_wgtcpue),
    tot_wtcpue2 = sum(haul_wgtcpue * haul_wgtcpue),
    #squared needed for var calculation
    .groups = "keep"
  ) %>%
  left_join(domain_stats_e, by = c("year", "model", "stratum_bin")) %>%
  mutate(
    mean_wtcpue   = tot_wtcpue / stations,
    var_wtcpue    = tot_wtcpue2 / stations - mean_wtcpue * mean_wtcpue,
    bio_t_km2     = mean_wtcpue / 1000,
    bio_tons      = bio_t_km2 * area,
    var_bio_t_km2 = var_wtcpue / (1000 * 1000),
    var_bio_tons  = var_wtcpue * (area / 1000) * (area / 1000)
  )

model_sum_e <- domain_sum_e %>%
  group_by(year, model, race_group) %>%
  summarize(
    bio_tons     = sum(bio_tons),
    var_bio_tons = sum(var_bio_tons),
    .groups = "keep"
  ) %>%
    mutate(
      sd_bio_tons      = sqrt(var_bio_tons),
      cv_bio           = sd_bio_tons / bio_tons,
      model_area       = tot_model_area_e,
      bio_tons_km2     = bio_tons / model_area,
      var_bio_tons_km2 = var_bio_tons / (model_area * model_area),
      sd_bio_tons_km2  = sqrt(var_bio_tons_km2),
      cv_bio_tons_km2  = sd_bio_tons_km2 / bio_tons_km2,
  ) %>% 
    filter(!race_group %in% c("ZERO", "MISC_NA", "MISC_SHELLS")) %>% 
  ungroup() %>%
  # expand to all years 1990–2023 for each model × race_group
  complete(
    year       = 1990:2023,
    model,
    race_group
  ) %>%
  group_by(model, race_group) %>%
  fill(model_area, .direction = "down") %>%
  ungroup()

write.csv(model_sum_e,
          "EGOA_source_data/EGOA_groundfish_bio.csv",
          row.names = F)


#wide format
wgoa_race_b_e <- model_sum_e %>% 
  select(year,model,race_group,bio_tons_km2,cv_bio_tons_km2) %>%
  rename(mtkm2 = bio_tons_km2,
         cv_mtkm2 = cv_bio_tons_km2) %>%
  pivot_wider(names_from = race_group,
              values_from = c(mtkm2,cv_mtkm2),
              names_sep = "_", 
              names_glue = "{race_group}_{.value}",
              names_vary = 'slowest') #tidyr thing. 


write.csv(wgoa_race_b_e,
          "EGOA_source_data/egoa_groundfish_bio_mt_km2_wide.csv",
          row.names = F)
