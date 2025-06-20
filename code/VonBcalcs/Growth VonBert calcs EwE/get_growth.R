## function to get parameters of groth curves (von Bertalanffy, Gompertz, logistic)
#modified function for get_lw (length weight regression)
#uses fishmethods package in R (https://cran.r-project.org/web/packages/fishmethods/fishmethods.pdf)



#############################################################
library("fishmethods")

get_vonBgrowth <- function(predator="P.cod", model="EBS", years=NULL, all.data=F){

  stratbins    <- strata_lookup     #%>% mutate(stratum_bin = .data[[stratbin_col]])
  preylookup    <- preynames_lookup #%>% mutate(prey_guild  = .data[[preylook_col]])
  ppar          <- pred_params[[predator]] 
  model_name    <- model    # renamed to avoid name confusion during lookup
  predator_name <- predator # renamed to avoid name confusion during lookup  
  
  speciescode = as.integer(pred_params[[predator]]$race)
  
  sp_specimen <- specimen %>%
    filter(age != "NA") %>%  #BF added to remove any specimens that were not aged (want to calc length/weight regression from specimens that were aged as well)
    filter(species_code == speciescode)
  
  # KYA added
  #stratbins    <- strata_lookup    %>% mutate(stratum_bin = .data[[stratbin_col]])
  model_haul <- haul %>%
    left_join(stratbins, by=c("region"="survey","stratum"="stratum"))
  
  x <- model_haul %>%
    left_join(cruisedat,
              by = c("cruisejoin", "region")
    ) %>%
    filter(abundance_haul == "Y" &
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
           length_cm = length/10, length_mm=length, 
           weight_g=weight, 
           age_y=age) %>% relocate(predator) #BF added length_mm and weight_g and age
  
  
  if(!is.null(years)){
    x <- x %>% filter(year %in% years)
  }
  
  # growth curve function (fitting curve to  length/age data using nls )
  gr=growth(intype=1, unit=1, size=x$length_mm, age=x$age_y, calctype=1, wgtby=1, Sinf=1000 , K=0.1 , t0=0.1) #individual specimens, units=length, 
  summary(gr) #nls output
  
  #parameters from von Bertalanffy growth equation (Gompertz is 'gout', logistic is 'lout')
  linf= coef(gr$vout)[1]
  k= coef(gr$vout)[2]
  t0=coef(gr$vout)[3]
  
  #output von Bertalanffy growth curve parameters 
  dat <- x %>%
    mutate(lbin = as.character(cut(length_mm, ppar$LCLASS, right=F)), #this line kept from lenwght wieght regression function
           linf= coef(gr$vout)[1], 
           k= coef(gr$vout)[2], 
           t0=coef(gr$vout)[3])
  
  if(all.data){
    return(dat)
  }
  else{
    return(list(linf=linf,k=k, t0=t0))
  }
  
}

#############################################################