library(Rpath)

# TODO RPATH PACKAGE:  COME BACK TO DETRITAL FLOWS AND WARNINGS (what should they be??)
# TODO RPATH PACKAGE:  ALSO, add warning for Biomass versus PB for detritus?

source("rpath_files/xml_convert.r")

# import.eiixml function notes (from above script)
  # Read in eiiXML file at the location given by eiifile, and parse into an
  # unbalanced rpath object with stanzas.  Currently only reads base Ecopath
  # files (TODO:  pedigree read-in doesn't work due to comma issues, but 
  # also due to reverse numbering scheme between EwE and Rpath).
  #
  # Required argument eiifile (the eiiXML filename from EwE)
  # Optional Arguments:
  #   verbose=T will list all tables as they're read in, to check data inflow (default F)
  #   export=T will make all input tables available (variables named ewe_[table name])
  #
  # Note:
  # A matrix alignment warning may occur if there are commas in text strings,
  # check to see if misaligned tables are important for Ecopath balance.

# WGOA creation ###############
# Create unbalanced rpath object, don't worry about table alignment warnings
# if the tables aren't ones used in basic path or stanzas.
  unbal <- import.eiixml(eiifile = "rpath_files/WGOA_11Dec2024_renamed.eiixml")

# WGOA hand-fixes from entry issues in the EwE model.  These should be
# fixed eventually in the EwE files.  
  # FIXED in EwE 11-Dec-24 unbal$model[Type==2, "DetInput"] <- 0 
  # FIXED in EwE 11-Dec-24 TODO WGOA note Lingcod name issue 
  # FIXED in EwE 11-Dec-24 TODO WGOA/EGOA names in general
  
# Set Inter-detrital flows the way Rpath prefers (no inter-flows)
# TODO: discuss inter-detrital schemes in EwE versus Rpath
  det_names <- unbal$model[Type==2]$Group
  unbal$model[Group%in%det_names, det_names] <- 0

# Remove Biomass and add PB to detritus to estimate flows
# TODO: discuss detrital balance (appropriate numbers)
  unbal$model[Group%in%det_names, "Biomass"] <- NA
  unbal$model[Group%in%det_names, "PB"]      <- 1.0 # 1.0 means once a year 
  
# Add full stanza calculations to unbalanced model, check parameter set,
# then balance the model, copying over to western version.
  w.unbal <- rpath.stanzas(unbal)
  check.rpath.params(w.unbal)
  w.bal   <- rpath(w.unbal)

# EGOA creation ###############  
  # Create unbalanced rpath object
  unbal <- import.eiixml(eiifile = "rpath_files/EGOA-multi-stanza_2024_12_10_renamed.eiixml",export=T)
  
  # WGOA hand-fixes from entry issues in the EwE model.  These should be
  # fixed eventually in the EwE files. 
  
  # Set Inter-detrital flows the way Rpath prefers (no inter-flows)
  # TODO: discuss inter-detrital schemes in EwE versus Rpath
  det_names <- unbal$model[Type==2]$Group
  unbal$model[Group%in%det_names, det_names] <- 0
  
  # Remove Biomass and add PB to detritus to estimate flows
  # TODO: discuss detrital balance (appropriate numbers)
  unbal$model[Group%in%det_names, "Biomass"] <- NA
  unbal$model[Group%in%det_names, "PB"]      <- 1.0
  
  # Add full stanza calculations to unbalanced model, check parameter set,
  # then balance the model, copying over to western version.
  e.unbal <- rpath.stanzas(unbal)
  check.rpath.params(e.unbal)
  e.bal   <- rpath(e.unbal)
  
#############################################################  
# SCRATCH STUFF not needed for now 
  
# impdiet <- ordgroups$ImpVar; names(impdiet)<-rownames(ordgroups)    
# predlist <- names(unbal$diet)[2:ncol(unbal$diet)]
# for(p in predlist){
#   unbal$diet[Group=="Import", p] <- impdiet[p]
# }
#   
# # To check Biomass, EEs etc.
# View(data.frame(bal$Biomass, bal$PB, bal$QB, bal$EE))
# 
# # Testing rsim
# scene0 <- rsim.scenario(bal, unbal, years=1990:2089)
# run0   <- rsim.run(scene0, method="RK4", years = 1990:2089)
# rsim.plot(run0)
# 
# scene1 <- adjust.fishing(scene0, "ForcedFRate", "sablefish_adult", sim.year=1995:2000, value=0.2)
# run1   <- rsim.run(scene1, method="RK4", years = 1990:2089)
# rsim.plot(run1)

  