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

#this provides the summary for stratum, and save the file for a sanity check####
#stratbio_combined <- NULL
#stratsum <- cpue_dat %>%
#  group_by(year, model, race_group, stratum) %>%
#  summarize(wgtcpue = sum(wgtcpue),
#            numcpue = sum(numcpue),.groups="keep") %>%
#  left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
#  mutate(bio_t_km2 = wgtcpue/1000/stations,
#         bio_tons  = bio_t_km2 * area)
#
#stratbio_combined <- rbind(stratbio_combined,stratsum)  
#write.csv(stratbio_combined, "WGOA_source_data/goa_bio_bystrat_2025_5_13.csv",row.names=F) 

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

# BIA YOU ARE HERE ####
bio_totals2 <- domain_sum %>%
  group_by(year, model, race_group) %>%
  summarise(
    # total and sum‐of‐squares of your per‐station density
    sum_bio_station  = sum(bio_t_km2,      na.rm = TRUE),
    sum_sq_bio_station = sum(bio_t_km2^2,    na.rm = TRUE),
    n = sum(stations),           #  how many “observations” went into those sums
    area = first(tot_model_area),     # the total area over which you want to scale up
    mean_bio = sum_bio / n, #the mean density across all stations
    var_t2km2 = ifelse(n > 1, #the sample variance of those station densities: Var = [Σ x² − (Σ x)²/n] / (n − 1)
                     (sum_sq_bio_station - sum_bio_station^2 / n) / (n - 1),NA_real_), #sample variance
    sd_bio = sqrt(var_t2km2),
    se_bio = sd_bio / sqrt(n),
    cv_bio = ifelse(mean_bio > 0, sd_bio / mean_bio, NA_real_),
    bio_tons = sum(bio_tons),
    bio_tkm2 = bio_tons/tot_model_area, 
    var_bio_tons2 = var_t2km2 * tot_model_area^2, #variance in total tons
    .groups = "keep")

# BIA YOU ARE HERE ####

# Old code, I think the variance here is wrong, because assumes that all stations are independent.
#bio_totals <- domain_sum %>%
#  group_by(year, model, race_group) %>%
#  summarize(bio_tons = sum(bio_tons),
#            bio_tkm2 = bio_tons/tot_model_area, 
#            var_bio_tons = sum(var_bio_tons), #FLAG  ####
#            .groups="keep")
#

# NEW CODE ####
juv_combined2 <- NULL
for (p in pred_names) {
#get your haul‐by‐haul densities + stratum summary
  juv_adu_lencons <-
    get_stratum_length_cons(predator = p, model = this.model) %>%
    left_join(haul_stratum_summary(this.model),
              by = c("year", "model", "stratum")) %>%
#collapse to one row per year–model–species–stratum–lbin,
#computing sums & sums of squares by hand
    group_by(year, model, species_name, stratum, lbin) %>%
    summarise(sum_den = sum(tot_wlcpue_t_km2, na.rm = TRUE),#total “density” summed over all stations
              sum_sq_den = sum(tot_wlcpue_t_km2^2, na.rm = TRUE),#sum of squares
              n = first(stations),#station count
              area = first(area),#stratum area
              mean_den = sum_den / n,# mean density
              var_den = ifelse(n > 1, 
                               (sum_sq_den - sum_den^2 / n) / (n - 1), NA_real_), # sample variance (n–1 denominator)
              sd_den = sqrt(var_den),# sample SD
              se_den = sd_den / sqrt(n), # standard error of the mean density
              cv_den = ifelse(mean_den > 0, sd_den / mean_den, NA_real_),# coefficient of variation of the density
              .groups = "keep") %>%
    mutate(bio_t_km2 = mean_den,# mean biomass density (t/km2)
      var_tkm2 = var_den,# variance of biomass density (just var_den still in (t/km2)^2)
      sd_tkm2 = sd_den,
      se_tkm2 = se_den,
      cv_tkm2 = cv_den,
      bio_tons = bio_t_km2 * area,# now multiply by area to get total biomass (tons) in that stratum–lbin
      var_tons = var_tkm2 * area^2,
      sd_tons = sqrt(var_tons),
      se_tons = sd_tons / sqrt(n),
      cv_tons = ifelse(bio_tons > 0, sd_tons / bio_tons, NA_real_),
      jcat = ifelse(lbin == pred_params[[p]]$jsize, "juv", "adu") # juvenile, adult id
    )


  juv_proportions <- juv_adu_lencons %>%
    group_by(year, model, species_name, jcat) %>%
    summarise(
      bio_tons = sum(bio_tons, na.rm = TRUE),
      var_tons = sum(var_tons, na.rm = TRUE),
      n_obs    = sum(n, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    complete(year,model,species_name,jcat = c("juv", "adu"),
             fill = list(bio_tons= NA_real_, var_tons= NA_real_, n_obs= NA_integer_)) %>%
    pivot_wider(names_from = jcat, values_from = c(bio_tons, var_tons, n_obs),
                names_sep   = "_") %>%
#compute SD, SE, CV for each stanza **after** pivoting
    mutate(sd_juv = sqrt(var_tons_juv),
           sd_adu = sqrt(var_tons_adu),
           se_juv = sd_juv / sqrt(n_obs_juv),
           se_adu = sd_adu / sqrt(n_obs_adu),
           cv_juv = if_else(bio_tons_juv > 0, sd_juv / bio_tons_juv, NA_real_),
           cv_adu = if_else(bio_tons_adu > 0, sd_adu / bio_tons_adu, NA_real_),
           juv_bio_prop = bio_tons_juv / (bio_tons_juv + bio_tons_adu))
  
  juv_combined2 <- bind_rows(juv_combined2, juv_proportions)
}


bio_with_juvs2 <- bio_totals %>%
  left_join(juv_combined2,
            by = c("year", "model", "race_group" = "species_name")) %>%
  mutate(juv_bio_prop = coalesce(juv_bio_prop, NA_real_)) %>%  # fill missing prop → NA
  # compute juvenile & adult densities
  mutate(
    adu_bio_tkm2 = (1 - juv_bio_prop) * bio_tkm2,
    juv_bio_tkm2 = juv_bio_prop * bio_tkm2) %>%
  mutate(var_bio_tkm2 = var_bio_tons / (tot_model_area^2)) %>%
  mutate(adu_var_tkm2 = (1 - juv_bio_prop)^2 * var_bio_tkm2,
    juv_var_tkm2 = juv_bio_prop^2 * var_bio_tkm2) %>%
  mutate(
    adu_sd_tkm2 = sqrt(adu_var_tkm2),
    juv_sd_tkm2 = sqrt(juv_var_tkm2)) %>%
  mutate(
    adu_cv_tkm2 = if_else(adu_bio_tkm2 > 0,adu_sd_tkm2 / adu_bio_tkm2, NA_real_),
    juv_cv_tkm2 = if_else(juv_bio_tkm2 > 0,juv_sd_tkm2 / juv_bio_tkm2, NA_real_)
    ) %>% 
  select(-matches("_(adu|juv)$"))


bio_with_juvs3 <- bio_totals %>%
  left_join(juv_combined2,
            by = c("year","model","race_group"="species_name")) %>%
  # 1) fill missing juv‐prop (if really you want NA→0 use 0 instead of NA_real_)
  mutate(juv_bio_prop = coalesce(juv_bio_prop, NA_real_)) %>%
  
  # 2) original density + variance steps
  mutate(
    adu_bio_tkm2 = (1 - juv_bio_prop) * bio_tkm2,
    juv_bio_tkm2 = juv_bio_prop       * bio_tkm2,
    var_bio_tkm2 = var_bio_tons / tot_model_area^2,
    adu_var_tkm2 = (1 - juv_bio_prop)^2 * var_bio_tkm2,
    juv_var_tkm2 = juv_bio_prop^2      * var_bio_tkm2
  ) %>%
  
  # 3) **fill** any ADU gaps from the TOTALs
  mutate(
    adu_bio_tkm2 = coalesce(adu_bio_tkm2, bio_tkm2),
    adu_var_tkm2 = coalesce(adu_var_tkm2, var_bio_tkm2)
  ) %>%
  
  # 4) **re**–compute SD / SE / CV for adults (and juveniles if you like)
  mutate(
    adu_sd_tkm2 = sqrt(adu_var_tkm2),
    adu_se_tkm2 = adu_sd_tkm2 / sqrt(n_obs_adu),
    adu_cv_tkm2 = if_else(adu_bio_tkm2 > 0,
                          adu_sd_tkm2 / adu_bio_tkm2,
                          NA_real_),
    
    juv_sd_tkm2 = sqrt(juv_var_tkm2),
    juv_se_tkm2 = juv_sd_tkm2 / sqrt(n_obs_juv),
    juv_cv_tkm2 = if_else(juv_bio_tkm2 > 0,
                          juv_sd_tkm2 / juv_bio_tkm2,
                          NA_real_)
  ) %>%
  
  # 5) optionally drop any *_adu / *_juv columns you no longer need:
  select(-matches("_(bio|var)_tkm2$"))


##OLD CODE ####
#juv_combined <- NULL
#for (p in pred_names){
#  #p <- pred_names[1]
#  bio_pred <- bio_totals %>%
#    filter(race_group==p)
#  
#  juv_adu_lencons  <- get_stratum_length_cons(predator=p, model=this.model) %>%
#    group_by(year, model, species_name, stratum, lbin) %>%
#    summarize(strat_bio_sum = sum(tot_wlcpue_t_km2), .groups="keep") %>%
#    left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
#    mutate(bio_t_km2 = strat_bio_sum/stations,
#           bio_tons  = bio_t_km2 * area, #area for each stratum/bins
#           jcat      = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu")) 
#  
#  
#  juv_proportions <- juv_adu_lencons %>%
#    group_by(year,model,species_name,jcat) %>%
#    summarize(bio_tons = sum(bio_tons), .groups="keep") %>%
#    pivot_wider(names_from=jcat, values_from=bio_tons) %>%
#    mutate(juv_bio_prop = juv/(juv+adu))
#  
#  juv_combined <- rbind(juv_combined,juv_proportions)
#} # end of pred_names loop


#bio_with_juvs <- bio_totals %>%
#  left_join(juv_combined,by=c("year","model","race_group"="species_name")) %>%
#  select(-c(juv,adu)) %>%
#  mutate(juv_bio_prop  = replace_na(juv_bio_prop,0.0),
#         adu_bio_tkm2  = (1.0-juv_bio_prop) * bio_tkm2,
#         juv_bio_tkm2  = juv_bio_prop       * bio_tkm2,
#         var_bio_tkm2  = var_bio_tons / (tot_model_area^2), #FLAG ####
#         # propagate variance to each stanza #
#         adu_var_tkm2  = (1 - juv_bio_prop)^2 * var_bio_tkm2,
#         juv_var_tkm2  = juv_bio_prop^2      * var_bio_tkm2,
#         adu_sd_tkm2  = sqrt(adu_var_tkm2),
#         juv_sd_tkm2  = sqrt(juv_var_tkm2),
#         adu_cv_tkm2  = adu_sd_tkm2 / adu_bio_tkm2,
#         juv_cv_tkm2  = juv_sd_tkm2 / juv_bio_tkm2
#         )
#         
         
bio_combined <- NULL
bio_combined <- rbind(bio_combined,bio_with_juvs2)


#write.csv(bio_combined,
#          "WGOA_source_data/WGOA_groundfish_bio_juv_adu.csv",
#          row.names = F)

model_sum_comb <- bio_combined %>% 
  select(c(year, model, race_group, adu_bio_tkm2, juv_bio_tkm2, adu_cv_tkm2, juv_cv_tkm2)) %>%
  pivot_longer(!c(year, model, race_group), names_to= c("stanza", ".value"), names_sep = "_") %>% 
  rename(bio_tkm2 = bio,
         cv_tkm2 = cv) %>%
  filter(bio_tkm2 != 0.000000e+00 & !is.na(cv_tkm2)) %>% 
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
  select(c(year, model, race_group, bio_tkm2,sd_tkm2 ,cv_tkm2))

#write.csv(model_sum_comb,
#          "WGOA_source_data/wgoa_groundfish_bio_mt_km2_long.csv",
#          row.names = F)


# Now we need to calculate the means and cvs for the combined data
wgoa_race_bio <- model_sum_comb %>%
  group_by(model, race_group) %>%
  # expand to all years 1990–2023 for each model × race_group
  complete(
    year= 1990:2023, fill = list(bio_tkm2=NA, cv_tkm2=NA)
  )  %>%
  pivot_wider(names_from = race_group,
              values_from = c(bio_tkm2,cv_tkm2),
              names_sep = "_", 
              names_glue = "{race_group}_{.value}",
              names_vary = 'slowest') #tidyr thing. 
  
write.csv(wgoa_race_bio,
          "WGOA_source_data/wgoa_groundfish_bio_mt_km2_wide.csv",
          row.names = F)





# EGOA GUILDS -----------------------------------------------
this.model  <- "EGOA"
race_lookup      <- race_lookup_base %>% mutate(race_group  = .data[["final_egoa"]])
#q_table          <- read.clean.csv("lookups/GroupQ_2021_GOA.csv")
#domains_included_e <-  c(
#  "Southeastern_shelf",
#  "Southeastern_slope",
#  "Southeastern_gully",
#  "Yakutat_shelf",
#  "Yakutat_gully",
#  "Yakutat_slope"
#)

domains_included_e <-  c("50", "150", "151",                     #  "Southeastern_shelf" 
                       "250", "251","351","450","550",         #  "Southeastern_slope"
                       "350",                                  #  "Southeastern_gully"
                       "40", "41", "140","141", "142", "143",  #  "Yakutat_shelf"
                       "240", "340",                           #  "Yakutat_gully"
                       "241", "341", "440", "540"              #  "Yakutat_slope"
                       )

tot_model_area_e <- sum(strat_areas$area[strat_areas$model == this.model &
                                           strat_areas$stratum_bin %in% domains_included_e])
cpue_dat_e  <- get_cpue_all(model = this.model) %>% 
  drop_na(race_group)

check_RACE_codes(cpue_dat_e)

# Code to do means and sds
domain_stats_e <- haul_domain_summary(this.model)

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


predlist <- read.clean.csv("lookups/Alaska_Multistanza_GOA_vonb_2025_04_30_v2.csv")

preds_e <- predlist %>% filter(model==this.model)
pred_names_e <- unique(preds_e$predator)
pred_params=list()
for (p in pred_names_e){
  pdat_e <- as.list(preds_e[preds_e$predator==p,])
  pred_params[[p]] <- pdat_e
  pred_params[[p]]$LCLASS <- sort(unique(c(0,pdat_e$juv_cm, pdat_e$adu_1, pdat_e$adu_2, pdat_e$adu_3,9999)))
  pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
  pred_params[[p]]$lw_b   <- pdat_e$b_l_mm_g
  pred_params[[p]]$lw_a   <- pdat_e$a_l_mm_g*(10^pdat_e$b_l_mm_g)  
  pred_params[[p]]$bioen  <- list(CA=pdat_e$ca, CB=pdat_e$cb, C_TM=pdat_e$c_tm, C_T0=pdat_e$c_t0, C_Q=pdat_e$c_q)
  pred_params[[p]]$vonb <- list(h= pdat_e$vb_k, Linf  = pdat_e$vb_linf_mm, t0= pdat_e$vb_t0, rec_len=pdat_e$von_b_rec_len_cm)
}


#Getting biomass-at-length and converting to split pool biomass
# Loop through each predator.  the juv_adu_lencons splits out
# length samples based on the length class divisions in the
# predator lookup, and converts to % by weight using the a and b LW
# regression.  This % by weight is applied to the total biomass
# for the predator calculated above, to calculate biomass in
# each length class.  It is output into bio_combined as two biomass groupings
# (juv and adu) where adu is the sum of all the adu size classes.
bio_totals_e <- domain_sum_e %>%
  group_by(year, model, race_group) %>%
  summarize(bio_tons = sum(bio_tons),
            bio_tkm2 = bio_tons/tot_model_area_e, 
            var_bio_tons = sum(var_bio_tons), #FLAG  ####
            .groups="keep")

juv_combined_e <- NULL
for (p in pred_names_e){
  #p <- pred_names[1]
  bio_pred_e <- bio_totals_e %>%
    filter(race_group==p)
  
  juv_adu_lencons_e  <- get_stratum_length_cons(predator=p, model=this.model) %>%
    group_by(year, model, species_name, stratum, lbin) %>%
    summarize(strat_bio_sum = sum(tot_wlcpue_t_km2), .groups="keep") %>%
    left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
    mutate(bio_t_km2 = strat_bio_sum/stations,
           bio_tons  = bio_t_km2 * area, #area for each stratum/bins
           jcat      = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu")) 
  
  juv_proportions_e <- juv_adu_lencons_e %>%
    group_by(year,model,species_name,jcat) %>%
    summarize(bio_tons = sum(bio_tons), .groups="keep") %>%
    pivot_wider(names_from=jcat, values_from=bio_tons) %>%
    mutate(juv_bio_prop = juv/(juv+adu))
  
  juv_combined_e <- rbind(juv_combined_e,juv_proportions_e)
} # end of pred_names loop



bio_with_juvs_e <- bio_totals_e %>%
  left_join(juv_combined_e,by=c("year","model","race_group"="species_name")) %>%
  select(-c(juv,adu)) %>%
  mutate(juv_bio_prop  = replace_na(juv_bio_prop,0.0),
         adu_bio_tkm2  = (1.0-juv_bio_prop) * bio_tkm2,
         juv_bio_tkm2  = juv_bio_prop       * bio_tkm2,
         var_bio_tkm2  = var_bio_tons / (tot_model_area_e^2), #FLAG ####
         # propagate variance to each stanza #
         adu_var_tkm2  = (1 - juv_bio_prop)^2 * var_bio_tkm2,
         juv_var_tkm2  = juv_bio_prop^2      * var_bio_tkm2,
         adu_sd_tkm2  = sqrt(adu_var_tkm2),
         juv_sd_tkm2  = sqrt(juv_var_tkm2),
         adu_cv_tkm2  = adu_sd_tkm2 / adu_bio_tkm2,
         juv_cv_tkm2  = juv_sd_tkm2 / juv_bio_tkm2
  )


bio_combined_e <- NULL
bio_combined_e <- rbind(bio_combined_e,bio_with_juvs_e)


#write.csv(bio_combined_e,
#          "EGOA_source_data/EGOA_groundfish_bio_juv_adu.csv",
#          row.names = F)


model_sum_comb_e <- bio_combined_e %>% 
  select(c(year, model, race_group, adu_bio_tkm2, juv_bio_tkm2, adu_cv_tkm2, juv_cv_tkm2)) %>%
  pivot_longer(!c(year, model, race_group), names_to= c("stanza", ".value"), names_sep = "_") %>% 
  rename(bio_tkm2 = bio,
         cv_tkm2 = cv) %>%
  filter(bio_tkm2 != 0.000000e+00 & !is.na(cv_tkm2)) %>% 
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
  select(c(year, model, race_group, bio_tkm2, cv_tkm2))

#write.csv(model_sum_comb_e,
#          "EGOA_source_data/egoa_groundfish_bio_mt_km2_long.csv",
#          row.names = F)


# Now we need to calculate the means and cvs for the combined data
egoa_race_bio <- model_sum_comb_e %>%
  group_by(model, race_group) %>%
  # expand to all years 1990–2023 for each model × race_group
  complete(
    year= 1990:2023, fill = list(bio_tkm2=NA, cv_tkm2=NA)
  )  %>%
  pivot_wider(names_from = race_group,
              values_from = c(bio_tkm2,cv_tkm2),
              names_sep = "_", 
              names_glue = "{race_group}_{.value}",
              names_vary = 'slowest') #tidyr thing. 

write.csv(egoa_race_bio,
          "EGOA_source_data/egoa_groundfish_bio_mt_km2_wide.csv",
          row.names = F)
