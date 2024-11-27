library(Rpath)

# Script assumes wd is WGOA-Ecopath_Rpath main repo (root of repo)

# Read in eiiXML file at the location given by eiipath (currently as a source
# file for debugging, will later be a function incorporated into rpath).
# xml_convert.r should contain generic read-ins only.
  eiifile <- "WGOA_EwE_Files/WGOA_Nov2024.eiixml"
  source("rpath_files/xml_convert.r")

# TODO WGOA note Lingcod name issue
# TODO WGOA names in general
  
# WGOA hand-fixes from entry issues in the EwE model.  These should be
# fixed eventually in the EwE files.  Odd lookup here (data.table quirks). 
  unbal$model[Type==2, "DetInput"] <- 0
  det_names <- unbal$model[Type==2]$Group
  unbal$model[Group%in%det_names, det_names] <- 0
  #unbal$model[Group%in%det_names, "benthic_detritus"] <- 1
  
# TODO RPATH:  COME BACK TO DETRITAL FLOWS AND WARNINGS (what should they be??)
# TODO RPATH:  ALSO, add warning for Biomass versus PB for detritus?
  
# Remove Biomass and add PB to detritus to estimate flows
  unbal$model[Group%in%det_names, "Biomass"] <- NA
  unbal$model[Group%in%det_names, "PB"]      <- 1.0
  
# IGNORE DETRITAL FLOW WARNINGS IN check.rpath.params FOR NOW  
  unbal <- rpath.stanzas(unbal)
  check.rpath.params(unbal)
  bal <- rpath(unbal)

# To check Biomass, EEs etc.
View(data.frame(bal$Biomass, bal$PB, bal$QB, bal$EE))

# Testing rsim
scene0 <- rsim.scenario(bal, unbal, years=1990:2089)
run0   <- rsim.run(scene0, method="RK4", years = 1990:2089)
rsim.plot(run0)

scene1 <- adjust.fishing(scene0, "ForcedFRate", "sablefish_adult", sim.year=1995:2000, value=0.2)
run1   <- rsim.run(scene1, method="RK4", years = 1990:2089)
rsim.plot(run1)
