# First attempt at WGOA sims for GOACLIM
source("code/rpath_graphs.R")

# WGOA_EGOA_comp.R
library(Rpath)
library(dplyr)

# load models into w.unbal, w.bal for west, e.unbal and e.bal for east.
# File names are used in GOA_rpath_setup.R source call
WGOA_EwE_file <- "rpath_files/WGOA_17Mar2025.eiixml"
EGOA_EwE_file <- "rpath_files/EGOA_20250317.eiixml" 
source("code/GOA_rpath_setup.R")


# balanced WGOA
e.bal

# Make lists of types of model groups
all_living   <- living_groups(w.bal)
all_detritus <- detrital_groups(w.bal)
all_gears    <- gear_groups(w.bal)

# WGOA catch data
catch_dat <- read.csv("WGOA_source_data/wgoa_catch_ts_wide.csv")


# federally managed groundfish stocks
managed_sp <- c("arrowtooth_flounder_adult",
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
                "octopus")
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
                "pacific_cod_adult",
                "walleye_pollock_adult",
                "atka_mackerel",
                "squid",
                "rex_sole_adult",
                "salmon_shark",
                "large_sculpins",
                "octopus")
groundfish <- c(ssl_sp, nonssl_sp)


# Year Ranges
hind_years <- 1990:2020
fore_years <- 2021:2099
all_years  <- c(hind_years,fore_years)
basescene <- rsim.scenario(w.bal, w.unbal, years = all_years) # Ecosim params
scene <- basescene
