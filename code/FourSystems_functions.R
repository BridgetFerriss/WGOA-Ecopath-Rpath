################################################################################
flowmat <- function(bal, timesteps=365){
  
  main_flows <- matrix(0, nrow=bal$NUM_GROUPS+1, ncol=bal$NUM_GROUPS+1)
  rownames(main_flows) <- c(bal$Group, "Import")
  colnames(main_flows) <- c(bal$Group, "Import")
  
  # diet flows (QB scaled, no respiration/egestion)  
  sz <- dim(bal$DC)
  dmat <- t(matrix(bal$DC, nrow=sz[1], ncol=sz[2]))
  pmat <- t(bal$QB[1:bal$NUM_LIVING] * bal$Biomass[1:bal$NUM_LIVING] * dmat / timesteps)
  
  main_flows[1:(bal$NUM_LIVING+bal$NUM_DEAD),1:bal$NUM_LIVING] <- 
    main_flows[1:(bal$NUM_LIVING+bal$NUM_DEAD),1:bal$NUM_LIVING] +
    pmat[1:(bal$NUM_LIVING+bal$NUM_DEAD),1:bal$NUM_LIVING]
  main_flows["Import",1:bal$NUM_LIVING] <- 
    main_flows["Import",1:bal$NUM_LIVING] + pmat[nrow(pmat),]
  
  # Biomass flows   
  bmat <- diag(c(bal$Biomass[1:(bal$NUM_LIVING+bal$NUM_DEAD)],rep(1.0,bal$NUM_GEARS), 0.0))
  main_flows <- main_flows + bmat
  
  # Fishing flows  
  fmat <- (bal$Landings + bal$Discards)/timesteps
  main_flows[1:bal$NUM_GROUPS, (bal$NUM_GROUPS-bal$NUM_GEARS+1):bal$NUM_GROUPS] <- 
  main_flows[1:bal$NUM_GROUPS, (bal$NUM_GROUPS-bal$NUM_GEARS+1):bal$NUM_GROUPS] +
    fmat
  
  # Mzero detrital flows
  mzmat <- (bal$PB * bal$Biomass * (1.0 - bal$EE) / timesteps) * bal$DetFate
  main_flows[1:bal$NUM_GROUPS, (bal$NUM_LIVING+1):(bal$NUM_LIVING+bal$NUM_DEAD)] <-
  main_flows[1:bal$NUM_GROUPS, (bal$NUM_LIVING+1):(bal$NUM_LIVING+bal$NUM_DEAD)] +
    mzmat
  
  # Convert flow vector into transition state (proportion by row)  
  transition <- main_flows/rowSums(main_flows)[row(main_flows)]
  transition[is.nan(transition) | is.na(transition)] <- 0
  
  # Make state vector  
  state <- matrix(rep(0, bal$NUM_GROUPS+1),nrow=1); colnames(state)<-c(bal$Group,"Import")
  
  return(list(state=state,transition=transition))
} 
