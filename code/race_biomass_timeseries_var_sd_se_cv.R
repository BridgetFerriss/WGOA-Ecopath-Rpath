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

#tot_model_area <- sum(strat_areas$area[strat_areas$model == this.model &
#                                         strat_areas$stratum_bin %in% domains_included])

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
    .groups = "keep"
  )  %>%
  left_join(haul_stratum_summary(this.model), 
            by = c("year","model","stratum")) %>%
  mutate(
    # stations comes from the haul_stratum_summary function so includes
    # stations with 0 biomass.
      n_stations= sum(stations),
      mean_wtcpue   = tot_wtcpue / n_stations,
    # CHANGED below line, was missing a division by n-1 ###
      var_wtcpue    = (tot_wtcpue2 - ((tot_wtcpue * tot_wtcpue) / n_stations))/(n_stations-1),  #tot_wtcpue2 / stations - mean_wtcpue * mean_wtcpue,
    # Convert to variance of mean estimate by dividing by n_stations,
    # replace varest with 0 at this stage if it would be NA (due to 1 station only) 
      varest_wtcpue = ifelse(is.na(var_wtcpue), 0, var_wtcpue/n_stations),
    # Units of wtcpue as returned by get_cpue_all() are kg/km2, convert to t/km2
    # by dividing by 1000, then scale up to tons by multiplying by stratum area.
    # Scale up to tons by stratum area
      bio_tons      = mean_wtcpue   * area / 1000,
      varest_tons   = varest_wtcpue * area * area / (1000*1000)
    ## KYA 5/29/31 ONLY TONS should be added without weighting by area, so 
    ## dropping t_km2 calculations from this object - can be added in 
    ## to final sums later.
      #bio_t_km2     = mean_wtcpue   / 1000,
      #varest_t_km2  = varest_wtcpue /(1000 * 1000),
      #var_bio_t_km2 = var_wtcpue / (1000 * 1000), #FLAG ####
      #var_bio_tons  = var_wtcpue * (area / 1000) * (area / 1000), #FLAG ####
      #var_est_bio_tons = var_bio_tons / n_stations, #FLAG ####
      #var_est_bio_t_km2 = var_bio_t_km2/n_stations, #FLAG ####
      #sd_bio_tons = sqrt(var_bio_tons), #FLAG ####
      #cv_bio_tons = sd_bio_tons / bio_tons, #FLAG ####
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

#KYA - moved this section up - can be removed after confirming everything.
#model_area <- sum(strata_lookup$area[strata_lookup$model==this.model])
#bio_totals <- stratsum %>%
#  group_by(year, model, race_group) %>%
#  summarize(bio_tons = sum(bio_tons),
#            bio_tkm2 = bio_tons/model_area,
#            se_tkm2  = sqrt(sum(var_est_bio_t_km2)),
#            se_tons  = sqrt(sum(var_est_bio_tons)),
#            var_bio_tons = sum(var_bio_tons),
#            var_bio_t_km2= sum(var_bio_t_km2),
#            .groups="keep")

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
stratsum_jad <- bio_totals %>%
  left_join(juv_props, by = c("year","model","race_group")) %>%
  # wherever a species had no juvs, we’ll get NA; that’s okay
  mutate(
    juv_bio_prop = coalesce(juv_bio_prop, 0),
    juv_bio_t_km2 = bio_tkm2 * juv_bio_prop,
    adu_bio_t_km2 = bio_tkm2 * (1 - juv_bio_prop),
    juv_bio_tons = bio_tons * juv_bio_prop,
    adu_bio_tons = bio_tons * (1 - juv_bio_prop), 
    juv_se_bio_tons  = se_tons * juv_bio_prop, #FLAG ####
    adu_se_bio_tons  = se_tons * (1 - juv_bio_prop) #FLAG ####
  )
#write.csv(stratsum_jad, file="WGOA_source_data/stratsum_jad.csv", row.names=FALSE)

strata_long <- stratsum_jad %>%
  select(-bio_tons) %>% 
  pivot_longer(  cols       = c(juv_bio_t_km2, adu_bio_t_km2,
                                juv_bio_tons, adu_bio_tons, 
                                juv_se_bio_tons, adu_se_bio_tons),
                 names_to   = c("stanza", ".value"),
                 names_pattern = "(juv|adu)_(bio_t_km2|bio_tons|se_bio_tons)" # this is the regex that separates the stanza from the value
  )  %>%
  filter(!bio_t_km2==0) %>% 
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
  select(c(-stanza, -bio_tkm2, -se_tons))

#write.csv(strata_long, file="WGOA_source_data/strata_long.csv", row.names=FALSE)

#QUESTION: what to do with years that have only one station? #####

bio_summary2 <- strata_long %>%
  group_by(year, model, race_group) %>%
  summarise(
    total_area = model_area,
    # grab the one model_area per group
    bio_tons  = bio_tons,
    var_tons  = var_bio_tons, #FLAG ####
    se_tons   = se_bio_tons, #FLAG ####
    sd_tons   = sqrt(var_tons), #FLAG ####
    cv_tons   = se_tons / bio_tons, #FLAG ####
    
    bio_mt_km2   = bio_t_km2,
    var_mt_km2   = var_bio_t_km2, #FLAG ####
    se_mt_km2 = se_bio_tons / total_area, #FLAG ####
    sd_mt_km2    = sqrt(var_mt_km2), #FLAG ####
    cv_mt_km2    = se_mt_km2 / bio_mt_km2, #FLAG ####
    .groups = "drop"
  ) %>%
  select(
    year,
    model,
    total_area,
    race_group,
    bio_tons,
    var_tons,
    se_tons ,
    sd_tons ,
    cv_tons ,
    bio_mt_km2,
    var_mt_km2,
    se_mt_km2,
    sd_mt_km2,
    cv_mt_km2
  )
 


#write.csv(bio_summary2, file="WGOA_source_data/wgoa_race_biomass_ts.csv", row.names=FALSE)

bio_summary2[,"Type"] <- NA
bio_summary2[,"Scale1"] <- 1
bio_summary2[,"Scale2"] <- model_area
bio_summary2[,"Species"] <- ""
bio_summary2[,"Loc"] <- ""
bio_summary2[,"n"] <- ""
bio_summary2[,"Source"] <- "race_wgoa"
#bio_summary2[,"SE"] <- NA
  
bio_summary_v2 <-  bio_summary2 %>% 
  select(c(year, race_group, Type, sd_mt_km2, se_mt_km2, bio_mt_km2, Scale1, cv_mt_km2, Species, Loc, n, Source)) %>% 
  mutate(race_group=case_when(race_group=="Pacific herring"~ 
                                "Pacific herring adult", TRUE~race_group))

bio_summary_v2$race_group<-make_clean_names(bio_summary_v2$race_group, allow_dupes = TRUE)

 colnames(bio_summary_v2) <- c("Year", "Group", "Type", "Stdev", "SE", 
           "Value", "Scale", "CV",  "Species", 
            "Loc", "n", "Source") 
write.csv(bio_summary_v2, file="wgoa_data_rpath_fitting/wgoa_race_biomass_ts_fitting_index_v2_ka.csv", row.names=FALSE)


bio_summary_v2_tons <-  bio_summary2 %>% 
  select(c(year, race_group, Type, sd_tons, se_tons, bio_tons, Scale2, cv_tons, Species, Loc, n, Source)) %>% 
  mutate(race_group=case_when(race_group=="Pacific herring"~ 
                                "Pacific herring adult", TRUE~race_group))

bio_summary_v2_tons$race_group <-make_clean_names(bio_summary_v2_tons$race_group, allow_dupes = TRUE)

colnames(bio_summary_v2_tons) <- c("Year", "Group", "Type", "Stdev", "SE", 
                              "Value", "Scale", "CV",  "Species", 
                              "Loc", "n", "Source") 
write.csv(bio_summary_v2_tons, file="wgoa_data_rpath_fitting/wgoa_race_biomass_ts_fitting_index_v2_tons_ka.csv", row.names=FALSE)
