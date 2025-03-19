#library(devtools)
#install_github('NOAA-EDAB/Rpath', ref="dev") 

library(Rpath)
library(dplyr)

# load models into w.unbal, w.bal for west, e.unbal and e.bal for east.
# File names are used in GOA_rpath_setup.R source call
  WGOA_EwE_file <- "rpath_files/WGOA_17Mar2025.eiixml"
  EGOA_EwE_file <- "rpath_files/EGOA_20250317.eiixml" 
  source("code/GOA_rpath_setup.R")

# Checking group names #########################################################
# Groups in the west not the east
  w.unbal$model$Group[which(!(w.unbal$model$Group %in% e.unbal$model$Group))]

# Groups in the east not the west 
  e.unbal$model$Group[which(!(e.unbal$model$Group %in% w.unbal$model$Group))]
  
  
# Making a comparative table for basic balance stats ###########################
  e.groups <- data.frame(e.bal$Group,e.bal$TL,e.bal$Biomass,e.bal$PB,e.bal$QB,e.bal$EE, e.bal$GE)
  w.groups <- data.frame(w.bal$Group,w.bal$TL,w.bal$Biomass,w.bal$PB,w.bal$QB,w.bal$EE, w.bal$GE) 
  
  ew.groups <- e.groups %>%
               full_join(w.groups, by=join_by(e.bal.Group==w.bal.Group))  
  
  #plot(ew.groups$e.bal.PB/ew.groups$w.bal.PB)
  #text(ew.groups$e.bal.PB,ew.groups$w.bal.PB,ew.groups$e.bal.Group)
  
  write.csv(ew.groups,"ewgroups_balance_comp_14Mar25.csv",row.names=F)
  
# Some plots ###################################################################
  
  dat <- ew.groups %>% filter(!( is.na(e.bal.Biomass) | is.na(w.bal.Biomass) ))
  plot(dat$e.bal.TL,dat$w.bal.TL)
  abline(0,1)
  points(dat$e.bal.TL[dat$e.bal.Group=="euphausiids"], dat$w.bal.TL[dat$e.bal.Group=="euphausiids"],col="red", pch=15)
  
  plot(dat$e.bal.TL,dat$w.bal.TL, type='n')
  abline(0,1)
  text(dat$e.bal.TL,dat$w.bal.TL, dat$e.bal.Group, cex=0.5)
  
  
# Testing rsim #################################################################
w.scene0 <- rsim.scenario(w.bal, w.unbal, years=1990:2089)
e.scene0 <- rsim.scenario(e.bal, e.unbal, years=1990:2089)
  
w.run0   <- rsim.run(w.scene0, method="RK4", years = 1990:2089)
rsim.plot(w.run0)

e.run0   <- rsim.run(e.scene0, method="RK4", years = 1990:2089)
rsim.plot(e.run0)

w.scene1 <- adjust.fishing(w.scene0, "ForcedFRate", "sablefish_adult", sim.year=1995:2000, value=0.2)
w.run1   <- rsim.run(w.scene1, method="RK4", years = 1990:2089)
rsim.plot(w.run1)

e.scene1 <- adjust.fishing(e.scene0, "ForcedFRate", "sablefish_adult", sim.year=1995:2000, value=0.2)
e.run1   <- rsim.run(e.scene1, method="RK4", years = 1990:2089)
rsim.plot(e.run1)

# Interactive plots. Use the 

source("rpath_files/rsim.plot.interactive.R")
rsim.plot.interactive(w.run1, spname = "all", indplot = FALSE, palette="b_palette")

test<-rsim.plot.interactive(w.run1, spname = "all", indplot = FALSE, palette=colorspace::rainbow_hcl)
b_palette <- colorspace::rainbow_hcl 
b2_palette <- colorRamps::primary.colors
rsim.plot.interactive(w.run1, spname = "all", indplot = FALSE, palette = "b2_palette" )

test


  