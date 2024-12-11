library(Rpath)

# load models into w.unbal, w.bal for west, e.unbal and e.bal for east.
  source("rpath_files/GOA_rpath_setup.R")

# Groups in the west not the east
  w.unbal$model$Group[which(!(w.unbal$model$Group %in% e.unbal$model$Group))]

# Groups in the east not the west 
  e.unbal$model$Group[which(!(e.unbal$model$Group %in% w.unbal$model$Group))]
  
  