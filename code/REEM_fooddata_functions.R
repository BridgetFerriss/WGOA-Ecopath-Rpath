#------------------------------------------------------------------------------#
#REVIEWED: Bia Dias
#ORIGINAL AUTHORS: Kerim Aydin
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#------------------------------------------------------------------------------#
#source("R/function_get_cpue_byLength.R")
#source("R/function_get_cpue_NoLength.R")
#source("R/function_get_biomass_stratum_Length.R")
#source("R/function_get_biomass_stratum_NoLength.R")

source("code/GAP_get_cpue.R")
source("code/GAP_get_cpue_guild.R")
source("code/GAP_get_biomass_stratum.R")
#source("R/GAP_loadclean.R")

# Need to link supported model names to survey id, mainly to filter
# out slope surveys from the EBS samples.
SURVEY_IDS <<- c("EBS"=98, "NBS"=143, "WGOA"=47, "EGOA"=47, "AI"=52)

#############################################################
get_stratum_length_cons <- function(
  racebase_tables = list(
  cruisedat = cruisedat,
  haul = haul,
  catch = catch,
  length = length),
  predator = "P.cod", #speciescode = 30060, # POP
  model    = "EBS"    #survey_area = "AI"
){
  
  conslens <- get_cpue_length_cons(predator=predator, model=model)
  
  haul_sum <- conslens %>%
    group_by(year,model,stratum_bin,species_name, hauljoin, 
             stationid, stratum, lat, lon, bottom_temp, surface_temp, 
             lbin) %>%
    summarize(tot_wtcpue_t_km2       = mean(wgtcpue)/1000,
              tot_wlcpue_t_km2       = sum(WgtLBin_CPUE_kg_km2)/1000,
              f_t                    = mean(f_t),
              tot_cons_t_km2_bioen   = sum(cons_kg_km2_bioen)/1000,
              tot_cons_vonb_t_km2    = sum(cons_vonb_kg_km2)/1000,
              .groups="keep")
}

#############################################################
get_stratum_length_cons_v2 <- function(
    racebase_tables = list(
      cruisedat = cruisedat,
      haul = haul,
      catch = catch,
      length = length),
    predator = "P.cod", #speciescode = 30060, # POP
    model    = "EBS"    #survey_area = "AI"
){
  
  conslens <- get_cpue_length_cons(predator=predator, model=model)
  
  haul_sum <- conslens %>%
    group_by(year,model,stratum_bin,species_name, hauljoin, 
             stationid, stratum, lat, lon, bottom_temp, surface_temp, 
             lbin) %>%
    summarize(tot_wtcpue_t_km2       = mean(wgtcpue)/1000,
              tot_wlcpue_t_km2       = sum(WgtLBin_CPUE_kg_km2)/1000, #WgtLBin_CPUE_kg_km2=NumLBin_CPUE_km2*body_wt/1000
              f_t                    = mean(f_t),
              tot_cons_t_km2_bioen   = sum(cons_kg_km2_bioen)/1000,
              tot_cons_vonb_t_km2    = sum(cons_vonb_kg_km2)/1000,
              .groups="keep")
}

#############################################################
check_RACE_codes <- function(cpue_dat){
  cpue_code_test <- cpue_dat %>%
    filter(is.na(race_group) & wgtcpue>0.0)
  
  missing_guilds <- sort(unique(cpue_code_test$species_code))

  if(length(missing_guilds)>0){
    message("The following RACE codes have biomass in this model ecosystem, but no guild assigned:")
    print(missing_guilds)
    message("From raw RACE species table:")
    print(species[species$species_code %in% missing_guilds,])
    message("From raw RACE species_classification table:")
    print(species_classification[species_classification$species_code %in% missing_guilds,])
    unknown_guilds <- missing_guilds[!(missing_guilds %in% species$species_code)]
    if(length(unknown_guilds)>0){
      message("Codes not on any RACE lookup table:")
      print(unknown_guilds)
    }
  }
  else{
    message("RACE codes checked - none missing.")
  }
  
}


#############################################################
strat_summary<- function(model){
  this.model=model
  return(strata_lookup %>% filter(model==this.model) %>% select(stratum_bin,area))
}

#############################################################
haul_list <- function(model){
  this.model=model
  stratbins    <- strata_lookup # %>% mutate(stratum_bin = .data[[stratbin_col]])
  x <- haul %>% 
    filter(abundance_haul == "Y" & survey_id == SURVEY_IDS[model]) %>% 
    mutate(year=floor(cruise/100)) %>% 
    left_join(stratbins, by=c("region"="survey", "stratum"="stratum")) %>%
    filter(model==this.model) %>%
    group_by(model, stratum_bin, stratum, year,hauljoin) %>%
    summarize(.groups="keep")
  
}

#############################################################
haul_stratum_summary <- function(model){
  
  this.model=model
  stratbins    <- strata_lookup # %>% mutate(stratum_bin = .data[[stratbin_col]])
  x <- haul %>% 
    filter(abundance_haul == "Y" & survey_id == SURVEY_IDS[this.model]) %>% 
    mutate(year=floor(cruise/100)) %>% 
    left_join(stratbins, by=c("region"="survey", "stratum"="stratum")) %>%
    filter(model==this.model) %>%
    group_by(model, stratum_bin, stratum, area, year) %>% 
    summarize(stations          = length(hauljoin),
              bottom_temp_mean  = mean(gear_temperature,na.rm=T),
              surface_temp_mean = mean(surface_temperature,na.rm=T),
              .groups="keep")
  return(x)
}
#############################################################
haul_domain_summary <- function(model){
  
  this.model=model

  stratbins    <- strata_lookup # %>% mutate(stratum_bin = .data[[stratbin_col]])
  x <- haul %>% 
    filter(abundance_haul == "Y" & survey_id == SURVEY_IDS[this.model]) %>% 
    mutate(year=floor(cruise/100)) %>%
    left_join(stratbins %>% select(model,survey,stratum,stratum_bin), by=c("region"="survey", "stratum"="stratum")) %>%
    filter(model==this.model) %>%
    left_join(strat_areas, by=c("model","stratum_bin")) %>%
    group_by(model, stratum_bin, area, year) %>% 
    summarize(stations          = length(hauljoin),
              bottom_temp_mean  = mean(gear_temperature,na.rm=T),
              surface_temp_mean = mean(surface_temperature,na.rm=T),
              .groups="keep")
  return(x)
}


#############################################################
haul_summary <- function(model){

  this.model=model
  stratbins    <- strata_lookup # %>% mutate(stratum_bin = .data[[stratbin_col]])
  x <- haul %>% 
    filter(abundance_haul == "Y" & survey_id == SURVEY_IDS[model]) %>% 
    mutate(year=floor(cruise/100)) %>% 
    left_join(stratbins, by=c("region"="survey", "stratum"="stratum")) %>%
    filter(model==this.model) %>%
    group_by(model,stratum_bin,year) %>% 
    summarize(stations          = length(hauljoin),
              bottom_temp_mean  = mean(gear_temperature,na.rm=T),
              surface_temp_mean = mean(surface_temperature,na.rm=T),
              .groups="keep")
  return(x)
}

#############################################################
get_lw <- function(predator="P.cod", model="EBS", years=NULL, all.data=F){
  
  stratbins    <- strata_lookup     #%>% mutate(stratum_bin = .data[[stratbin_col]])
  preylookup    <- preynames_lookup #%>% mutate(prey_guild  = .data[[preylook_col]])
  ppar          <- pred_params[[predator]] 
  model_name    <- model    # renamed to avoid name confusion during lookup
  predator_name <- predator # renamed to avoid name confusion during lookup  
  
  speciescode = as.integer(pred_params[[predator]]$race)
  
  sp_specimen <- specimen %>%
    filter(species_code == speciescode)
  
  # KYA added
  #stratbins    <- strata_lookup    %>% mutate(stratum_bin = .data[[stratbin_col]])
  model_haul <- haul %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum"))
  
  x <- model_haul %>%
    left_join(cruisedat,
              by = c("cruisejoin", "region")
    ) %>%
    filter(abundance_haul == "Y" & survey_id == SURVEY_IDS[model] &
             model == model_name) %>% # KYA changed
    left_join(sp_specimen, by = "hauljoin") %>%
    dplyr::select(
      species_code,model,stratum_bin,region.x,#KYA added model, stratum_bin
      cruisejoin.x, vessel.x, haul.x, hauljoin,
      stratum, stationid,
      year,
      start_latitude, start_longitude, gear_depth, bottom_depth,
      gear_temperature, surface_temperature,
      AreaSwept_km2, specimenid, length, sex, weight, age
    ) %>%
    filter(!is.na(specimenid) & length>0 & weight>1) %>%  #remove fish W=1 (logW=0) in which weight is too hard to measure and throws off regression 
    dplyr::rename(
      Lat = start_latitude,
      Lon = start_longitude,
      cruisejoin = cruisejoin.x,
      vessel = vessel.x,
      haul   = haul.x,
      region = region.x,
      Bottom_temp = gear_temperature,
      Surface_temp = surface_temperature
    ) %>%
    mutate(predator = predator_name,
           length_cm = length/10) %>% relocate(predator) 
  
  
  if(!is.null(years)){
    x <- x %>% filter(year %in% years)
  }
  
  lw <- lm(log(weight) ~ log(length_cm), data=x)
  loglw_a <- lw$coef[[1]]
  lw_b <- lw$coef[[2]]
  
  dat <- x %>%
    mutate(lbin = as.character(cut(length_cm, ppar$LCLASS, right=F)),
           log_lw_a = lw$coef[[1]], 
           lw_b = lw$coef[[2]], 
           log_diff = log(weight) - (loglw_a + lw_b*log(length_cm)))
  
  if(all.data){
    return(dat)
  }
  else{
    return(list(lw_a=exp(loglw_a),lw_b=lw_b))
  }
  
}

#############################################################

preylength_splits <- function(pred_nodc, predcut, prey_nodc, preycut, model){
  
  stratbins    <- strata_lookup    #%>% mutate(stratum_bin = .data[[stratbin_col]])  
  model_name    <- model    # renamed to avoid name confusion during lookup
  raw_pp <- preylengths
  this.pred_nodc <- pred_nodc
  this.prey_nodc <- prey_nodc
  
  allpred_tab <- raw_pp %>%
    # Add lookup tables
    #left_join(preylookup, by=c("prey_nodc"="nodc_code")) %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
    #left_join(yearblock, by=c("year"="year")) %>%
    relocate(stratum_bin) %>% relocate(model) %>%
    filter(model %in% model_name)   %>%
    filter(pred_nodc %in% this.pred_nodc) %>%
    filter(prey_nodc %in% this.prey_nodc) %>%
    #filter(!is.na(year)) %>%
    #filter(month %in% months) %>%
    select(1:freq) %>%
    mutate(pred_lbin_cm = as.character(cut(pred_len, predcut, right=F)),
           prey_lbin_mm = as.character(cut(prey_size_mm, preycut, right=F)))
  
}
#################################################################
logit_simple <- function(predator="P.cod", model="EBS"){

  # Global variables
  stratbins    <- strata_lookup    # %>% mutate(stratum_bin = .data[[stratbin_col]])
  preylookup    <- preynames_lookup # %>% mutate(prey_guild  = .data[[preylook_col]])
  #yearblock    <- years_lookup
  ppar          <- pred_params[[predator]]
  raw_pp        <- predprey
  model_name    <- model    # renamed to avoid name confusion during lookup
  predator_name <- predator # renamed to avoid name confusion during lookup

  # Operations done on all predators before selection  
  allpred_tab <- raw_pp %>%
    # Add lookup tables
    left_join(preylookup, by=c("prey_nodc"="nodc_code")) %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
    #left_join(yearblock, by=c("year"="year")) %>%
    relocate(stratum_bin) %>% relocate(model) 
  
  # Select predator (and apply other filters like model and months), apply predator-specific values
  pred_tab <- allpred_tab %>%
    filter(model %in% model_name)   %>%
    filter(pred_nodc %in% ppar$nodc)  %>%
    #filter(!is.na(year)) %>%
    #filter(month %in% months) %>%
    mutate(predator = predator_name) %>% relocate(predator) %>%
    # Then add predator_specific data and make sure it's located before the prey_guild column
    #mutate(full = twt>0) %>% 
    mutate(lbin = as.character(cut(pred_len, ppar$LCLASS, right=F))) %>%
    #mutate(bodywt = ppar$lw_a * pred_len^ppar$lw_b) %>%
    relocate(any_of(c("lbin")), .before=prey_nodc) %>%
    # Group by all columns up to prey_nodc EXCEPT prey_nodc, then stratum and prey bins 
    group_by(across(c(1:prey_nodc,-prey_nodc)), stratum_bin, year, prey_guild) %>%
    summarize(prey_wt=sum(twt), .groups="keep")
  
  pred_counts <- pred_tab %>%
    #select(predator,model,stratum_bin,year,lbin,prey_guild,prey_wt) %>%  
    #filter(prey_wt>0) %>%
    group_by(predator,model,stratum_bin,year,lbin) %>%
    mutate(tot_n=length(predator)) %>%
    ungroup() %>%
    group_by(predator,model,stratum_bin,year,lbin, tot_n, prey_guild) %>%
    summarize(prey_n=length(prey_guild),.groups="keep") %>%
    mutate(logit_mean=)
    
}

################################################################################
add_diets_to_strata_length_cons <- function(strata_length_cons, predator="P.cod", model="EBS", min_sample=1){
  
  diet <- predprey_tables(predator=predator, model=model) %>%
    filter(pred_full >= min_sample)
  
  len_diet <- strata_length_cons %>%
    left_join(diet, by=c("species_name"="predator", "model", "stratum_bin", "year", "lbin")) %>%
    replace_na(list(prey_guild="MISSING", dietprop_wt=1.0,dietprop_sci=1.0)) %>%
    mutate(preycons_sci_t_km2_bioen = tot_cons_t_km2_bioen * dietprop_sci,
           preycons_sci_vonb_rel    = tot_cons_vonb_rel    * dietprop_sci)
  
  diet_sum <- len_diet %>%
    group_by(year,model,stratum_bin,species_name,lbin,prey_guild) %>%
    summarize(strat_preycons_sci_t_km2    = mean(preycons_sci_t_km2_bioen), 
              strat_preycons_sci_vonb_rel = mean(preycons_sci_vonb_rel),
              .groups="keep") %>%
    left_join(strat_areas,by=c("model","stratum_bin")) %>%
    mutate(cons_tons_day = strat_preycons_sci_t_km2 * area,
           cons_rel_vonb = strat_preycons_sci_vonb_rel * area)
  
}

################################################################################
preylength_tables_all <- function(){

  stratbins    <- strata_lookup    
  preylookup    <- preynames_lookup 
  raw_pl        <- preylengths
  #model_name    <- model    # renamed to avoid name confusion during lookup  

  allpred_tab <- raw_pl %>%
    # Add lookup tables
    left_join(preylookup %>% select(nodc_code, prey_guild), by=c("pred_nodc"="nodc_code")) %>%
    left_join(stratbins %>% select(survey, model, stratum, stratum_bin), by=c("region"="survey","stratum"="stratum")) %>%
    relocate(stratum_bin) %>% 
    relocate(model) %>% 
    rename(pred_guild=prey_guild) %>%
    relocate(pred_guild, .after=pred_nodc) %>%
    left_join(preylookup %>% select(nodc_code, prey_guild), by=c("prey_nodc"="nodc_code")) %>%
    relocate(prey_guild, .after=prey_nodc)
  
  return(allpred_tab)
    
}

################################################################################
predprey_tables_all <- function(model="EBS"){
  
  # Global variables
  stratbins    <- strata_lookup    # %>% mutate(stratum_bin = .data[[stratbin_col]])
  preylookup    <- preynames_lookup # %>% mutate(prey_guild  = .data[[preylook_col]])
  #yearblock    <- years_lookup
  #ppar          <- pred_params[[predator]]
  raw_pp        <- predprey
  model_name    <- model    # renamed to avoid name confusion during lookup
  #predator_name <- predator # renamed to avoid name confusion during lookup
  
  # Operations done on all predators before selection  
  allpred_tab <- raw_pp %>%
    # Add lookup tables
    left_join(preylookup, by=c("prey_nodc"="nodc_code")) %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
    #left_join(yearblock, by=c("year"="year")) %>%
    relocate(stratum_bin) %>% relocate(model) 
  
  # Select predator (and apply other filters like model and months), apply predator-specific values
  pred_tab <- allpred_tab %>%
    filter(model %in% model_name)   %>%
    #filter(pred_nodc %in% ppar$nodc)  %>%
    #filter(!is.na(year)) %>%
    #filter(month %in% months)   %>%
    #mutate(predator = predator_name) %>% relocate(predator) %>%
    # Then add predator_specific data and make sure it's located before the prey_guild column
    #mutate(full = twt>0) %>% 
    #mutate(lbin = as.character(cut(pred_len, ppar$LCLASS, right=F))) %>%
    #mutate(bodywt = ppar$lw_a * pred_len^ppar$lw_b) %>%
    #relocate(any_of(c("lbin","bodywt")), .before=prey_nodc) %>%
    # Group by all columns up to prey_nodc EXCEPT prey_nodc, then stratum and prey bins 
    group_by(across(c(1:prey_nodc,-prey_nodc)), stratum_bin, year, prey_guild) %>%
    summarize(prey_wt=sum(twt), .groups="keep")
  
  cat(nrow(pred_tab),"predprey records found, summarizing...\n"); flush.console()
  
  #assign("pred_tab_tmp", value = pred_tab, envir = .GlobalEnv)
  
  # This creates one line per predator with stomach weight totals
  pred_tots <- pred_tab %>%
    group_by(across(c(1:prey_guild,-prey_guild))) %>%
    summarize(tot_wt=sum(prey_wt), full=(prey_wt>0), prey_nguilds=n(), .groups="keep") %>%
    unique()
  
return(pred_tots)
} 

################################################################################
predprey_tables <- function(predator="P.cod", model="EBS", combine.data=F, all.data=F){

  # Global variables
  stratbins    <- strata_lookup    # %>% mutate(stratum_bin = .data[[stratbin_col]])
  preylookup    <- preynames_lookup # %>% mutate(prey_guild  = .data[[preylook_col]])
  #yearblock    <- years_lookup
  ppar          <- pred_params[[predator]]
  raw_pp        <- predprey
  model_name    <- model    # renamed to avoid name confusion during lookup
  predator_name <- predator # renamed to avoid name confusion during lookup

  # Operations done on all predators before selection  
  allpred_tab <- raw_pp %>%
    # Add lookup tables
    left_join(preylookup, by=c("prey_nodc"="nodc_code")) %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
    #left_join(yearblock, by=c("year"="year")) %>%
    relocate(stratum_bin) %>% relocate(model) 
  
  # Select predator (and apply other filters like model and months), apply predator-specific values
  pred_tab <- allpred_tab %>%
    filter(model %in% model_name)   %>%
    filter(pred_nodc %in% ppar$nodc)  %>%
    #filter(!is.na(year)) %>%
    #filter(month %in% months)   %>%
    mutate(predator = predator_name) %>% relocate(predator) %>%
    # Then add predator_specific data and make sure it's located before the prey_guild column
    #mutate(full = twt>0) %>% 
    mutate(lbin = as.character(cut(pred_len, ppar$LCLASS, right=F))) %>%
    mutate(bodywt = ppar$lw_a * pred_len^ppar$lw_b) %>%
    relocate(any_of(c("lbin","bodywt")), .before=prey_nodc) %>%
    # Group by all columns up to prey_nodc EXCEPT prey_nodc, then stratum and prey bins 
    group_by(across(c(1:prey_nodc,-prey_nodc)), stratum_bin, year, prey_guild) %>%
    summarize(prey_wt=sum(twt), .groups="keep")

  cat(nrow(pred_tab),"predprey records found, summarizing...\n"); flush.console()

  #assign("pred_tab_tmp", value = pred_tab, envir = .GlobalEnv)
  
  # This creates one line per predator with stomach weight totals
  pred_tots <- pred_tab %>%
    group_by(across(c(1:prey_guild,-prey_guild))) %>%
    summarize(tot_wt=sum(prey_wt), tot_sci=sum(prey_wt/bodywt), full=(prey_wt>0), prey_nguilds=n(), .groups="keep") %>%
    unique()

  # For the stratum, year and length get totals
  strat_tots <- pred_tots %>% 
    ungroup() %>%
    select(predator,model,stratum_bin,year,lbin,full,tot_wt,tot_sci) %>%
    group_by(predator,model,stratum_bin,year,lbin) %>%
    summarize(pred_n=n(), pred_full=sum(full), tot_wt=sum(tot_wt), tot_sci=sum(tot_sci), .groups="keep")
  
  global_tots <- pred_tots %>% 
    ungroup() %>%
    select(predator,model,full,tot_wt,tot_sci) %>%
    group_by(predator,model) %>%
    summarize(pred_n=n(), pred_full=sum(full), tot_wt=sum(tot_wt), tot_sci=sum(tot_sci), .groups="keep")

  # Sum 
if (!combine.data){
  strat_dietprop <- pred_tab %>%
    ungroup() %>%  
    mutate(prey_sci = prey_wt/bodywt) %>%
    select(predator,model,stratum_bin,year,lbin,prey_guild,prey_wt,prey_sci) %>%  
    #filter(prey_wt>0) %>%
    #adding zeros with complete
    complete(predator,model,stratum_bin,year,lbin, prey_guild,fill=list(prey_n=0,prey_wt=0,prey_sci=0)) %>%
    group_by(predator,model,stratum_bin,year,lbin, prey_guild) %>%
    summarize(prey_n=n(), 
              prey_wt=sum(prey_wt), 
              prey_sci=sum(prey_sci), 
              .groups="keep") %>%
    ungroup() %>%
    # Count prey_n but exclude 0's
    mutate(prey_n=ifelse(prey_wt>0, prey_n, 0)) %>% 
    left_join(strat_tots, 
              by=c("predator"="predator","model"="model","stratum_bin"="stratum_bin", "year"="year", "lbin"="lbin")) %>%
    relocate(any_of(c("pred_n","pred_full","tot_wt","tot_sci")), .before=prey_guild) %>%
    filter(!is.na(pred_n)) %>%
    mutate(dietprop_wt = prey_wt/tot_wt) %>%
    mutate(dietprop_sci = prey_sci/tot_sci)
} else {
  strat_dietprop <- pred_tab %>%
    ungroup() %>%  
    mutate(prey_sci = prey_wt/bodywt) %>%
    select(predator,model,prey_guild,prey_wt,prey_sci) %>%  
    #filter(prey_wt>0) %>%
    #adding zeros with complete
    complete(predator,model, prey_guild,fill=list(prey_n=0,prey_wt=0,prey_sci=0)) %>%
    group_by(predator,model, prey_guild) %>%
    summarize(prey_n=n(), 
              prey_wt=sum(prey_wt), 
              prey_sci=sum(prey_sci), 
              .groups="keep") %>%
    ungroup() %>%
    # Count prey_n but exclude 0's
    mutate(prey_n=ifelse(prey_wt>0, prey_n, 0)) %>% 
    left_join(global_tots, 
              by=c("predator"="predator","model"="model")) %>%
    relocate(any_of(c("pred_n","pred_full","tot_wt","tot_sci")), .before=prey_guild) %>%
    filter(!is.na(pred_n)) %>%
    mutate(dietprop_wt = prey_wt/tot_wt) %>%
    mutate(dietprop_sci = prey_sci/tot_sci)  
  
}
  # No longer using the crosstab version - saving here for future reference (may need tweaks)
  #pred_crosstab <- PP_data %>%
  #  # Add lookup tables
  #  left_join(preylookup, by=c("prey_nodc"="NODC_CODE")) %>%
  #  left_join(stratbins, by=c("region"="survey","stratum"="stratum")) %>%
  #  # First filter out all predators except the main PRED
  #  filter(submodel %in% MODEL)   %>%
  #  filter(pred_nodc %in% ppar$nodc) %>%
  #  # Then add predator_specific data and make sure it's located before the prey_guild column
  #  mutate(lbin = as.character(cut(pred_len, ppar$LCLASS, right=F))) %>%
  #  mutate(bodywt = ppar$A_L * pred_len^ppar$B_L) %>%
  #  relocate(any_of(c("lbin","bodywt")), .before=prey_nodc) %>%
  #  # Make crosstab query grouping by all columns up to prey_nodc EXCEPT prey_nodc, then stratum and prey bins 
  #  group_by(across(c(1:prey_nodc,-prey_nodc)), stratum_bin, prey_guild) %>%
  #  tally(wt=twt) %>%
  #  spread(prey_guild, n, fill=0) #%>% select(-"<NA>") <NA> showing up means a prey code is missing?
  if (all.data){  
    return(list(predprey_table = data.frame(pred_tab),
              pred_totals    = data.frame(pred_tots), 
              strat_dietprop = data.frame(strat_dietprop)))
  } else {
    return(data.frame(strat_dietprop))
  }

}

##################################################################
#
read.clean.csv <- function(filename){
  return(read.csv(filename) %>% janitor::clean_names())
}

##################################################################

fc_T_eq2<-function(TT, cpars){
  CTM_CT0        <- cpars$C_TM - cpars$C_T0    
  CTMoverCTM_CT0 <- cpars$C_TM / CTM_CT0
  Z = log(cpars$C_Q) * CTM_CT0
  Y = log(cpars$C_Q) * (CTM_CT0 + 2.0)  			 
  X_C <- ( Z*Z * (1.0 + ( (1.0 + 40.0/Y) ** 0.5)) ** 2.0)/400.0 
  Vc       <- CTMoverCTM_CT0 - TT / CTM_CT0
  Fc_T     <- (Vc ^ X_C ) * exp(X_C * (1.0 - Vc))

  return(Fc_T)
}
  
  
##################################################################
# Load and name-clean REEM diet files.  This loads 
# region must be one of 'BS', 'AI', or 'GOA',.
REEM.loadclean.diets<- function(data_path = "data/local_reem_data"){
  
  tname <- "predprey"
  fname <- paste(data_path, paste("predprey.csv",sep=""), sep="/")
  cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
  b <- read.csv(file = fname)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(tname, value = b, envir = .GlobalEnv)
  
  tname <- "preylengths"
  fname <- paste(data_path, paste("preylengths.csv",sep=""), sep="/")
  cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
  b <- read.csv(file = fname)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(tname, value = b, envir = .GlobalEnv)    
  #return(list(PP_data=PP_data,PL_data=PL_data))
  
}

##########################################################################
REEM.loadclean.strata<-function(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                                stratum_bin_column    = "strat_groups"){
  strata_lookup    <<- read.clean.csv(strata_lookup_file) %>% 
    mutate(stratum_bin = .data[[stratum_bin_column]])
  strat_areas <<- strata_lookup %>%
    select(model,stratum_bin,area) %>%
    group_by(model, stratum_bin) %>%
    summarize(area=sum(area),.groups="keep")  
}  

##########################################################################
REEM.loadclean.strata.by.stratum<-function(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                                stratum_bin_column    = "stratum"){
  strata_lookup    <<- read.clean.csv(strata_lookup_file) %>% 
    mutate(stratum_bin = .data[[stratum_bin_column]])
  strat_areas <<- strata_lookup %>%
    select(model,stratum_bin,area) %>%
    group_by(model, stratum_bin) %>%
    summarize(area=sum(area),.groups="keep")  
} 

##########################################################################
REEM.loadclean.lookups<-function(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                                 stratum_bin_column    = "strat_groups",
                                 preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                                 prey_guild_column     = "ecopath_prey"){
  
  strata_lookup    <<- read.clean.csv(strata_lookup_file)    %>% mutate(stratum_bin = .data[[stratum_bin_column]])
  preynames_lookup <<- read.clean.csv(preynames_lookup_file) %>% mutate(prey_guild  = .data[[prey_guild_column]])
  strat_areas <<- strata_lookup %>%
    select(model,stratum_bin,area) %>%
    group_by(model, stratum_bin) %>%
    summarize(area=sum(area),.groups="keep")
  
  #assign("strata_lookup",    value = strata_,    envir = .GlobalEnv)  
  #assign("preynames_lookup", value = read.clean.csv(preynames_lookup_file), envir = .GlobalEnv)    
}

##########################################################################
REEM.loadclean.lookups.by.stratum<-function(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                                 stratum_bin_column    = "stratum",
                                 preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                                 prey_guild_column     = "ecopath_prey"){
  
  strata_lookup    <<- read.clean.csv(strata_lookup_file)    %>% mutate(stratum_bin = .data[[stratum_bin_column]])
  preynames_lookup <<- read.clean.csv(preynames_lookup_file) %>% mutate(prey_guild  = .data[[prey_guild_column]])
  strat_areas <<- strata_lookup %>%
    select(model,stratum_bin,area) %>%
    group_by(model, stratum_bin) %>%
    summarize(area=sum(area),.groups="keep")
  
  #assign("strata_lookup",    value = strata_,    envir = .GlobalEnv)  
  #assign("preynames_lookup", value = read.clean.csv(preynames_lookup_file), envir = .GlobalEnv)    
}

##################################################################
# Load and name-clean REEM diet files.  This loads 
# region must be one of 'BS', 'AI', or 'GOA',.
REEM.loadclean.diets.old<- function(region=REGION, path="data/local_reem_data"){
  
  tname <- "PP_data"
  fname <- paste(path, paste(region, "_predprey.csv",sep=""), sep="/")
  cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
  b <- read.csv(file = fname)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  PP_data <- b #assign(tname, value = b, envir = .GlobalEnv)
  
  tname <- "PL_data"
  fname <- paste(path, paste(region, "_preylengths.csv",sep=""), sep="/")
  cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
  b <- read.csv(file = fname)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  PL_data <- b #assign(tname, value = b, envir = .GlobalEnv)    
  return(list(PP_data=PP_data,PL_data=PL_data))
}

##################################################################
##################################################################
#Load and name-clean racebase files
REEM.loadclean.RACE <- function(path="data/local_racebase"){

# Read in all csv files in given directory into tables, cleaning variable names
  a <- list.files(path, pattern = "\\.csv")
  for (i in 1:length(a)) {
    tname <- gsub(pattern = "\\.csv", replacement = "", x = a[i])
    fname <- paste(path, a[i], sep="/")
    cat("loading and cleaning", tname, "from", fname, "\n"); flush.console()
    b <- read.csv(file = fname)
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1") {
      b$x1 <- NULL
    }
    # KYA Note - assign is a global assigment (to environment)
    assign(tname, value = b, envir = .GlobalEnv)
  }
  
# Additions from GAP script. note <<- assignment to export variables to global environment.
  
    # Add area swept in km2 to hauls
    haul <- haul %>% 
      dplyr::left_join(v_cruises%>%select(region,vessel_id,cruise,survey_name,survey_definition_id),
                by=c("region","vessel"="vessel_id","cruise")) %>%
      dplyr::rename(survey_id = survey_definition_id) %>%
      dplyr::mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) 
    haul <<- haul
    
    # Tidy cruise date format
    cruisedat <- cruise %>% 
      dplyr::select(survey_name, region, cruisejoin, agency_name) 
    cruisedat$start_date <- as.Date(cruise$start_date)
    cruisedat$year <- lubridate::year(cruisedat$start_date)
    cruisedat <<- cruisedat

}

##################################################################
##################################################################
#Load and name-clean racebase files using google drive


REEM.loadclean.RACE.googledrive <- function(drive_url) {
  
  # Convert the folder URL or ID to a googledrive id object
  drive_folder_id <- googledrive::as_id(drive_url)
  
  # List all CSV files in the specified Google Drive folder
  drive_files <- googledrive::drive_ls(path = drive_folder_id, type = "csv")
  
  # Loop over each file
  for (i in seq_along(drive_files$name)) {
    file_name <- drive_files$name[i]
    # Remove the .csv extension to create a table name
    tname <- gsub(pattern = "\\.csv", replacement = "", x = file_name)
    cat("Downloading and cleaning", tname, "\n"); flush.console()
    
    # Define a temporary file path for the download
    temp_file <- file.path(tempdir(), file_name)
    
    # Download the CSV file; overwrite if it exists in the temp folder
    googledrive::drive_download(file = drive_files$id[i],
                                path = temp_file,
                                overwrite = TRUE)
    
    # Read the CSV file and clean the variable names
    b <- read.csv(temp_file)
    b <- janitor::clean_names(b)
    
    # Remove the first column if its name is "x1"
    if (names(b)[1] %in% "x1") {
      b$x1 <- NULL
    }
    
    # Assign the cleaned table to the global environment using the file's base name
    assign(tname, value = b, envir = .GlobalEnv)
  }
  
  # Additional processing (as in your original script)
  # Update the 'haul' table by joining with 'v_cruises' and calculating AreaSwept_km2
  haul <- haul %>% 
    dplyr::left_join(v_cruises %>% 
                       dplyr::select(region, vessel_id, cruise, survey_name, survey_definition_id),
                     by = c("region", "vessel" = "vessel_id", "cruise")) %>%
    dplyr::rename(survey_id = survey_definition_id) %>%
    dplyr::mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width))
  haul <<- haul
  
  # Process the cruise table to tidy date formats
  cruisedat <- cruise %>% 
    dplyr::select(survey_name, region, cruisejoin, agency_name)
  cruisedat$start_date <- as.Date(cruise$start_date)
  cruisedat$year <- lubridate::year(cruisedat$start_date)
  cruisedat <<- cruisedat
}
