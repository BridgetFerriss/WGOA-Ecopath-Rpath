library(Rpath)

# Script assumes wd is WGOA-Ecopath_Rpath main repo (root of repo)

# Read in eiiXML file at the location given by eiipath (currently as a source
# file for debugging, will later be a function incorporated into rpath).
# xml_convert.r should contain generic read-ins only.
#eiifile <- "WGOA_EwE_Files/WGOA_Nov2024.eiixml"
source("rpath_files/xml_convert.r")


# WGOA ############################
  w.unbal <- import.eiixml("WGOA_EwE_Files/WGOA_Nov2024.eiixml")
  # TODO WGOA note Lingcod name issue
  # TODO WGOA names in general

# WGOA hand-fixes from entry issues in the EwE model.  These should be
# fixed eventually in the EwE files.  Odd lookup here (data.table quirks). 
  w.unbal$model[Type==2, "DetInput"] <- 0
  det_names <- w.unbal$model[Type==2]$Group
  w.unbal$model[Group%in%det_names, det_names] <- 0
  # TODO RPATH:  COME BACK TO DETRITAL FLOWS AND WARNINGS (what should they be??)
  # TODO RPATH:  ALSO, add warning for Biomass versus PB for detritus?

# Remove Biomass and add PB to detritus to estimate flows
  w.unbal$model[Group%in%det_names, "Biomass"] <- NA
  w.unbal$model[Group%in%det_names, "PB"]      <- 1.0

# IGNORE DETRITAL FLOW WARNINGS IN check.rpath.params FOR NOW  
  w.unbal <- rpath.stanzas(w.unbal)
  check.rpath.params(w.unbal)
  w.bal <- rpath(w.unbal)

  
# EGOA ##################  
  e.unbal <- import.eiixml("WGOA_EwE_Files/EGOA-multi-stanza.eiixml")
  det_names <- e.unbal$model[Type==2]$Group
  e.unbal$model[Group%in%det_names, det_names] <- 0  
  
  e.unbal
  
  e.unbal <- rpath.stanzas(e.unbal)
  check.rpath.params(e.unbal)
  e.bal <- rpath(e.unbal)