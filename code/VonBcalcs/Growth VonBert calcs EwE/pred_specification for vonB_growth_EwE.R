#BF made this file for von Bertalanffy growth curveon - a truncated version of 'dietcom_steps_george2022.R' in Jan2023
#added pred names; changed line 12 source; deleted extra query code at end

# setwd() if needed here.  Everything should be run from the main repo
setwd("C:/Users/bridget.ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath")
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

####################################################################

source("R/REEM_fooddata_functions_AgedOnly_ForVonB.R") #BF modified 



# Load Race data and perform some preliminary cleanups/calculations
REEM.loadclean.RACE(path = "data/local_racebase")

# Load diet data and perform some preliminary cleanups/calculations
REEM.loadclean.diets(data_path = "data/local_reem_data")

# Load lookup tables and create summary lookups - re-run this line 
# if lookups change.
REEM.loadclean.lookups(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                       stratum_bin_column    = "strat_groups",
                       preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                       prey_guild_column     = "troy")


# Add species names & codesto list for query (from 'specimen.csv')
pred_params <- list(
  "P.cod"        = list(nodc="8791030401", race="21720", LCLASS=c(0,10,30,60,85,999),
                        bioen=list(ref="cod2015", CA=0.041, CB= -0.122, C_TM=21, C_T0=13.7, C_Q=2.41)),
  "W.pollock"    = list(nodc="8791030701", race="21740", LCLASS=c(0,10,25,40,55,999), 
                        bioen=list(ref="poll2015", CA=0.119, CB=-0.46, C_TM=15, C_T0=10, C_Q=2.6)),
  "Arrowtooth"   = list(nodc="8857040102", race="10110", LCLASS=c(0,10,30,50,999),
                        bioen=list(ref="atf2015", CA=0.125, CB=-0.199, C_TM=26, C_T0=20.512, C_Q=2.497)),
  "P.halibut"    = list(nodc="8857041901", race="10120", LCLASS=c(0,10,50,70,999),
                        bioen=list(ref="hal2018", CA=0.0625, CB=-0.1076, C_TM=18, C_T0=12.97, C_Q=3.084)),
  "R.sole"      = list(nodc="8857040501", race="10200"),
  "Sablefish"   = list(nodc="8827020101", race="20510"),
  "S.thornyhead"   = list(nodc="8826010201", race="30020"),
  "PO.perch"        = list(nodc="8826010102", race="30060"),
  "F.sole"        = list(nodc="8857040601", race="10130")
)

