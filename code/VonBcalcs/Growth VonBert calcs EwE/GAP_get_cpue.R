# Task 1: replicate design-based total biomass per stratum that's in the SQL table
#
# Data and script locations -----------------------------------------------
# CPUE table to replicate
# SQL Developer: AFSC--> Other Users --> AI --> Tables --> CPUE

# SQL script to replicate
# G Drive: G:/GOA/biomass_sizecomp_scripts/biomass/biomass.sql

# Stratum areas to use
# SQL Developer: AFSC--> Other Users --> GOA --> Tables --> GOA_STRATA

# This function replicates the tables in RACEBASE GOA --> CPUE
get_cpue <- function(racebase_tables = list(
                       cruisedat = cruisedat,
                       haul = haul,
                       catch = catch
                     ),
                     # KYA changed
                       predator = "P.cod", #speciescode = 30060, # POP
                       model    = "EBS"    #survey_area = "AI"
                     ){ 

  # survey_area is called region in RACEBASE
  cruisedat <- racebase_tables$cruisedat
  haul <- racebase_tables$haul
  catch <- racebase_tables$catch
  # KYA added
    speciescode = as.integer(pred_params[[predator]]$race)
    model_name    <- model    # renamed to avoid name confusion during lookup
    predator_name <- predator # renamed to avoid name confusion during lookup
  
  sp_catch <- catch %>%
    filter(species_code == speciescode)

  # KYA added
    stratbins    <- strata_lookup  #  %>% mutate(stratum_bin = .data[[stratbin_col]])
    model_haul <- haul %>%
       left_join(stratbins, by=c("region"="survey","stratum"="stratum"))
    
  dat <- model_haul %>%
    left_join(cruisedat,
      by = c("cruisejoin", "region")
    ) %>%
    filter(abundance_haul == "Y" &
      #region == survey_area) %>%
      model == model_name) %>% # KYA changed
    left_join(sp_catch, by = "hauljoin") %>%
    replace_na(list(
      weight = 0,
      number_fish = 0
    )) %>%
    dplyr::select(
      species_code,model,stratum_bin,region.x,#KYA added model, stratum_bin
      cruisejoin.x, vessel.x, haul.x, cruise.x, hauljoin,
      haul_type, performance, duration,
      stratum, stationid,
      distance_fished, weight, year,
      weight, number_fish,
      start_latitude, start_longitude,
      gear_temperature, surface_temperature,
      AreaSwept_km2
    ) %>%
    dplyr::rename(
      lat = start_latitude,
      lon = start_longitude,
      catch_kg = weight,
      vessel = vessel.x,
      region = region.x,
      bottom_temp = gear_temperature,
      surface_temp = surface_temperature
    )
  # %>%
  # filter(year == survey_yr)

  x <- dat %>%
    mutate(
      wgtcpue = catch_kg / AreaSwept_km2,
      numcpue = number_fish / AreaSwept_km2,
      survey = region,
      species_name=predator,
    ) %>%
    replace_na(list(species_code = speciescode)) %>%
    select(
      year, model, stratum_bin,species_name, survey, vessel, haul.x, cruise.x, hauljoin,
      stationid,
      stratum, distance_fished, lat, lon, bottom_temp, surface_temp,
      species_code, catch_kg, number_fish,
      AreaSwept_km2, wgtcpue, numcpue # wgtcpue is kg per km2
    ) %>%
    rename(haul = haul.x,
           cruise = cruise.x) %>%
    arrange(year)

  return(x)
}

##################################################################################
get_cpue_length_cons <- function(racebase_tables = list(
                       cruisedat = cruisedat,
                       haul = haul,
                       catch = catch,
                       length = length
                     ),
                     # KYA changed
                       predator = "P.cod", #speciescode = 30060, # POP
                       model    = "EBS"    #survey_area = "AI"
                     ) {

# Get total unlengthed CPUE - this step also filters predator and ecosystem
  cdat <- get_cpue(racebase_tables = racebase_tables, predator=predator, model=model)

# Add length data from length table  
  dat <- cdat %>%
          left_join(length, by = c("hauljoin", "species_code")) %>%
          select(
            year, model, stratum_bin, species_name, survey,
            vessel.x, haul.x, cruise.x,
            hauljoin, stationid, stratum, distance_fished,
            lat, lon, bottom_temp, surface_temp,
            species_code, catch_kg, number_fish, AreaSwept_km2, wgtcpue, numcpue,
            cruisejoin, catchjoin,       
            length, frequency, sex
          ) %>%
          replace_na(list(length = 0, frequency=1)) %>%
          rename(haul = haul.x,
                 vessel = vessel.x,
                 cruise = cruise.x)

# Add L-W regressions and scale to cpue      
  x <- dat %>%
      group_by(hauljoin, species_code) %>% 
      mutate(NumSamp_SpHaul=sum(frequency)) %>% 
      ungroup() %>%
      mutate(PropLength_Num = frequency/NumSamp_SpHaul,
             NumLBin_CPUE_km2 = PropLength_Num*numcpue,
             lw_a = pred_params[[predator]]$lw_a, 
             lw_b = pred_params[[predator]]$lw_b,
             body_wt = lw_a * (length/10.0)^lw_b,
             lbin = as.character(cut(length/10.0, pred_params[[this.pred]]$LCLASS, right=F)),
             WgtLBin_CPUE_kg_km2 = NumLBin_CPUE_km2*body_wt/1000
    )
  
# add clean (mean by stratum) temperatures  
  haulmeans <- haul_summary(this.model)
  res <- x %>%
    left_join(haulmeans,by=c("model","stratum_bin","year")) %>%
    mutate(bottom_temp_clean = ifelse(is.na(bottom_temp),bottom_temp_mean,bottom_temp),
           surface_temp_clean = ifelse(is.na(surface_temp),surface_temp_mean,surface_temp))
  
  bpar = pred_params[[predator]]$bioen
  conslens <- res %>%
    mutate(cmax_g            = bpar$CA * body_wt^(1+bpar$CB),
           f_t               = fc_T_eq2(bottom_temp_mean, bpar),
           cons_kg_km2       = NumLBin_CPUE_km2 * cmax_g * f_t / 1000)
  
}


####################################################
get_haul_means <- function(model){
  
  model_name <- model
  stratbins    <- strata_lookup  #  %>% mutate(stratum_bin = .data[[stratbin_col]])
  model_haul <- haul %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum"))
  
  dat <- model_haul %>%
    left_join(cruisedat, by = c("cruisejoin", "region")) %>%
    filter(abundance_haul == "Y" & model == model_name) # KYA changed    
  
  out <- dat %>%
    group_by(year, stratum_bin) %>%
    dplyr::summarize(
      haul_count = length(unique(hauljoin)), # number of total abundance hauls
      mean_bottom_temp = mean(gear_temperature,na.rm=TRUE),
      mean_surface_temp = mean(surface_temperature,na.rm=TRUE),
      .groups="keep")
      #var_wgt_cpue = ifelse(haul_count <= 1, NA, var(wgtcpue) / haul_count),
      #mean_num_cpue = mean(numcpue, na.rm = TRUE),
      #var_num_cpue = ifelse(haul_count <= 1, NA, var(numcpue) / haul_count),
      #catch_count = length(which(catch_kg > 0)), # number of hauls with nonzero catch
      #.groups="keep")
  
}

#########################################################################
guild_out <- function(dat){
  x <- dat %>%
    left_join(species, by=c("species_code"="species_code")) %>%
    select(LATITUDE= Lat,
           LONGITUDE= Lon ,
           STATION= stationid,
           STRATUM= stratum,
           YEAR= year,
           DATETIME=start_time ,	
           WTCPUE= wgtcpue,
           NUMCPUE= numcpue,
           COMMON= common_name,
           SCIENTIFIC= species_name,
           SID= species_code,
           BOT_DEPTH= bottom_depth,
           BOT_TEMP= Bottom_temp,
           SURF_TEMP= Surface_temp,
           VESSEL= Vessel,
           CRUISE= cruise,
           HAUL= haul) %>%
    mutate (WTCPUE=WTCPUE/100, NUMCPUE=NUMCPUE/100)
  return(x)
}

##############################################################################
get_cpue_all <- function(racebase_tables = list(
  cruisedat = cruisedat,
  haul = haul,
  catch = catch),model){ 
  
  # survey_area is called region in RACEBASE
  cruisedat <- racebase_tables$cruisedat
  haul <- racebase_tables$haul
  catch <- racebase_tables$catch
  # KYA added
  model_name    <- model    # renamed to avoid name confusion during lookup
  
  sp_catch <- catch #%>% filter(species_code == speciescode)
  
  # KYA added
  stratbins    <- strata_lookup   # %>% mutate(stratum_bin = .data[[stratbin_col]])
  model_haul <- haul %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum"))
  
  dat <- model_haul %>%
    left_join(cruisedat,
              by = c("cruisejoin", "region")
    ) %>%
    filter(abundance_haul == "Y" &
             #region == survey_area) %>%
             model == model_name) %>% # KYA changed
    left_join(sp_catch, by = "hauljoin") %>%
    replace_na(list(
      weight = 0,
      number_fish = 0)) %>%
    dplyr::select(
      species_code,model,stratum_bin,region.x,#KYA added model, stratum_bin
      cruisejoin.x, vessel.x, haul.x, cruise.x, hauljoin,
      haul_type, performance, start_time, duration,
      stratum, stationid, bottom_depth,
      distance_fished, weight, year,
      weight, number_fish,
      start_latitude, start_longitude,
      gear_temperature, surface_temperature,
      AreaSwept_km2
    ) %>%
    dplyr::rename(
      Lat = start_latitude,
      Lon = start_longitude,
      catch_kg = weight,
      Vessel = vessel.x,
      region = region.x,
      Bottom_temp = gear_temperature,
      Surface_temp = surface_temperature
    )
  # %>%
  # filter(year == survey_yr)
  
  x <- dat %>%
    mutate(
      wgtcpue = catch_kg / AreaSwept_km2,
      numcpue = number_fish / AreaSwept_km2,
      survey = region,
    ) %>%
    #replace_na(list(species_code = speciescode)) %>%
    select(
      year, model, species_code, stratum_bin, survey, Vessel, haul.x, cruise.x, hauljoin,
      stationid, bottom_depth,
      stratum, start_time, distance_fished,Lat,Lon,Bottom_temp,Surface_temp,
      species_code, catch_kg, number_fish,
      AreaSwept_km2, wgtcpue, numcpue # wgtcpue is kg per km2
    ) %>%
    rename(haul = haul.x, cruise=cruise.x) %>%
    arrange(year)
  
  return(x)
}




