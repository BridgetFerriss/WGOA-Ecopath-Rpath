# First attempt at WGOA sims for GOACLIM
source("code/rpath_graphs.R")

# lines 4:41 from FourSystems_comp.R ------------------------------------------- #
library(Rpath)
library(dplyr)
library(tidyr)

# load models into:
# w.unbal
# w.bal for west GOA, 
# e.unbal
# e.bal for east GOA.
# s.unbal
# s.bal for SEBS

# File names are used in GOA_rpath_setup.R source call
#WGOA
WGOA_EwE_file <- "rpath_files/WGOA_17March25_simpleDet.eiixml"

#EGOA    
EGOA_EwE_file <- "rpath_files/EGOA_20250317_simpleDet.eiixml"

#EBS  
Sbase <- "rpath_files/ebs_aclim3_76bio_base.csv"  # Base biomass, production, fishing, etc.
Sdiet <- "rpath_files/ebs_aclim3_76bio_diet.csv"  # Diet matrix
Sped  <- "rpath_files/ebs_aclim3_76bio_pedigree.csv"  # Data pedigree = quality of input data
Sstz  <- "rpath_files/ebs_aclim3_76bio_stanzas.csv"  # Stanzas
Sstg  <- "rpath_files/ebs_aclim3_76bio_stanza_groups.csv" # Stanza groups

#NBS  
Nbase <- "rpath_files/nbs_2010_base.csv"  # Base biomass, production, fishing, etc.
Ndiet <- "rpath_files/nbs_2010_diet.csv"  # Diet matrix
Nped  <- "rpath_files/nbs_2010_pedigree.csv"  # Data pedigree = quality of input data
Nstz  <- "rpath_files/nbs_stanzas.csv"  # Stanzas
Nstg  <- "rpath_files/nbs_stanza_groups.csv" # Stanza groups

source("code/GOA_rpath_setup.R")
s.unbal <- rpath.stanzas(read.rpath.params(Sbase, Sdiet, Sped, Sstg, Sstz)) # unbalanced
s.bal   <- rpath(s.unbal) # balanced

n.unbal <- rpath.stanzas(read.rpath.params(Nbase, Ndiet, Nped, Nstg, Nstz)) # unbalanced
n.bal   <- rpath(n.unbal) # balanced
# ---------------------------------------------------------------------------- #
# unbalanced WGOA
w.unbal
# balanced WGOA
w.bal

# Make lists of types of model groups
all_living   <- living_groups(w.bal)
all_detritus <- detrital_groups(w.bal)
all_gears    <- gear_groups(w.bal)

# Biological reference points and other fisheries related quantities --------- #
# Equilibrium F-rate
F_equil <- (rowSums(w.bal$Landings)+rowSums(w.bal$Discards))/(w.bal$Biomass);  names(F_equil) <- w.bal$Group
F_equil  <- F_equil[c(all_living,all_detritus)]
# Equilibrium biomass
B_equil  <- w.bal$Biomass; names(B_equil) <- w.bal$Group; 
B_equil  <- B_equil[c(all_living,all_detritus)] 
# WGOA catch data
catch_dat <- read.csv("WGOA_source_data/wgoa_catch_ts_wide.csv")
catch_dat <- catch_dat[,-1]
catch_dat_sp <- c("arrowtooth_flounder_adult",
                  "flathead_sole_adult",
                  "shallow_water_flatfish",
                  "deep_water_flatfish",
                  "pacific_ocean_perch_adult",
                  "slope_rockfish",
                  "demersal_shelf_rockfish",
                  "pelagic_shelf_rockfish",
                  "sablefish_adult",
                  "pacific_cod_adult",
                  "walleye_pollock_adult",
                  "atka_mackerel",
                  "squid",
                  "rex_sole_adult",
                  "salmon_shark",
                  "large_sculpins",
                  "octopus",
                  "pacific_halibut_adult")
colnames(catch_dat) <- c("year", catch_dat_sp)
# SSL prey stocks with a B20 cutoff
ssl_sp <- c("atka_mackerel", "pacific_cod_adult", "walleye_pollock_adult")
# non-SSL prey stocks
nonssl_sp <- c("arrowtooth_flounder_adult",
               "flathead_sole_adult",
               "shallow_water_flatfish",
               "deep_water_flatfish",
               "pacific_ocean_perch_adult",
               "slope_rockfish",
               "demersal_shelf_rockfish",
               "pelagic_shelf_rockfish",
               "sablefish_adult",
               "rex_sole_adult",
               "octopus")
# federally managed groundfish stocks
managed_sp <- c(ssl_sp, nonssl_sp)
# Species for which we have some catch data but not in all years (1991:2023)
# Because the simulations are begining in 1990 and the catch data begins in 1991
# all of the catch_dat_sp are missing catch data in at least one year.
F_partial <- catch_dat_sp
# Species for which F_equil > 0 but we have no catch data
F_species <- c("offshore_killer_whales",
               "other_baleen_whales",
               "dolphins_porpoises",
               "northern_fur_seal_adult",
               "steller_sea_lion",
               "harbor_seal",
               "sea_otter",
               "piscivorous_surface_birds",
               "planktivorous_surface_birds",
               "piscivorous_diving_birds",
               "planktivorous_diving_birds",
               "salmon_returning",
               "salmon_outgoing",
               "pacific_herring_adult",
               "pacific_sandlance",
               "pacific_capelin",
               "shelf_forage_fish",
               "pacific_dogfish",
               "pacific_sleeper_shark",
               "big_skate",
               "longnose_skate",
               "other_skates",
               "slope_rockfish",
               "thornyheads",
               "lingcod",
               "slope_demersal_fish",
               "tanner_crab",
               "king_crab",
               "pandalid_shrimp",
               "motile_epifauna",
               "sessile_epifauna",
               "infauna")

# Year Ranges
hind_years      <- 1990:2020
fore_years      <- 2021:2099
all_years       <- c(hind_years,fore_years)
# roms_hind_years <- 1991:2020
# read in target biomass and Frate
B_target <- read.csv("WGOA_source_data/B_target.csv", row.names = 1)
F_target <- read.csv("WGOA_source_data/F40_opt.csv", row.names = 1)
colnames(F_target) <- "F40"

bioen_pars <- read.csv("WGOA_source_data/WGOA_bioen.csv", header=TRUE, sep=',', 
                       dec='.', row.names=1) #this file is valid for both EGOA and WGOA
# X parameter in Kitchell equation
bioen_sp <- row.names(bioen_pars)

# ---------------------------------------------------------------------------- #
# create base rsim.scenario object that will be the same for all 
# climate-enhanced sims.
# hard coding an Atka mackerel PB
w.bal$PB["atka_mackerel"] <- 0.544
basescene <- rsim.scenario(w.bal, w.unbal, years = all_years) # Ecosim params
scene <- basescene
# Set up hindcast fishing, turn off fishing effort
# Zero effort, freeze Discards.offal
scene <- adjust.fishing(scene, "ForcedEffort", all_gears, all_years, value = 0.0)
scene$forcing$ForcedBio[,"discards_offal"] <- B_equil["discards_offal"]
# apply the observed historical catch
for(i in catch_dat_sp) {
  scene$fishing$ForcedCatch[2:(length(hind_years)),i] <- catch_dat[1:(length(hind_years)-1),i]
}
# Add F_equil for species that we do not have catch data
for (i in 1:(length(F_species))) {
  scene$fishing$ForcedFRate[1:(length(hind_years)), F_species[i]] <-
    F_equil[F_species[i]]
}
# Apply F_equil during years we are missing catch data. The assumption here is
# that was in fact catch for these species but there is not catch data to force with.
for (i in 1:(length(F_partial))) {
  scene$fishing$ForcedFRate[1:(length(hind_years)), F_partial[i]] <-
    ifelse(scene$fishing$ForcedCatch[1:(length(hind_years)), F_partial[i]] <= 0.0,
           F_equil[F_partial[i]], 0.0)
}

# ---------------------------------------------------------------------------- #
# Apply hindcast primary production forcing
hind_pp_wide <- read.csv("WGOA_source_data/ROMSOutputWGOA/wide_WGOA_prod_300_anommaly.csv")
hind_pp_wide$month <- rep(seq(1,12,1),120)
# hindcast years are 1991:2020
hind_pp <- hind_pp_wide[121:492,]
BaseB        <- scene$params$B_BaseRef
names(BaseB) <- scene$params$spname
# Set groups to force   
scene$forcing$ForcedBio[1:372,"large_phytoplankton"] <- BaseB["large_phytoplankton"] * hind_pp[1:372,"phl_ssp126"]
scene$forcing$ForcedBio[1:372,"small_phytoplankton"] <- BaseB["small_phytoplankton"] * hind_pp[1:372,"phs_ssp126"]   
