#------------------------------------------------------------------------------#
#AUTHORS: Andy Whitehouse and Bia Dias
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHORS: andy.whitehouse@noaa.gov and bia.dias@noaa.gov
#
#------------------------------------------------------------------------------#


library('viridis')
library('here')
source("code/simulations/goaclim_sim_v4.r")
source("code/bioenergetic_projections.r")


num_runs <- 100
sim_126_f1 <- as.list(rep(NA,num_runs))
sim_245_f1 <- as.list(rep(NA,num_runs))
sim_585_f1 <- as.list(rep(NA,num_runs))
set.seed(13)

# arguments: (scenario, ssp, hcr, cons, resp, buf)
# test_run_126 <- clim_sim_hcr_obs_err(scene, 126, 1, TRUE, TRUE, TRUE)
# test_run_245 <- clim_sim_hcr_obs_err(scene, 245, 1, TRUE, TRUE, TRUE)
# test_run_585 <- clim_sim_hcr_obs_err(scene, 585, 1, TRUE, TRUE, TRUE)

# This loop took about 20 minutes on my machine
ptm <- proc.time()
for(i in 1:num_runs){
  print(paste0("num_run_",i))
  sim_126_f1[[i]] <- clim_sim_hcr_obs_err(scene, 126, 1, TRUE, TRUE, TRUE)
  print(paste0("num_run_",i))
  sim_245_f1[[i]] <- clim_sim_hcr_obs_err(scene, 245, 1, TRUE, TRUE, TRUE)
  print(paste0("num_run_",i))
  sim_585_f1[[i]] <- clim_sim_hcr_obs_err(scene, 585, 1, TRUE, TRUE, TRUE)
}
proc.time() - ptm

# to access sim outputs, for example
sim_126_f1[[1]]$annual_Catch[,"walleye_pollock_adult"]


# arguments: scene, ssp, hcr, cons, resp, buf
test_run_126 <- clim_sim_hcr_obs_err(scene, 126, 1, TRUE, TRUE, TRUE)
test_run_245 <- clim_sim_hcr_obs_err(scene, 245, 1, TRUE, TRUE, TRUE)
test_run_585 <- clim_sim_hcr_obs_err(scene, 585, 1, TRUE, TRUE, TRUE)

annual_biomass_126 <- as.data.frame(sim_126_f1[[1]]$annual_Biomass) %>% 
  select(c(pacific_cod_adult, walleye_pollock_adult, arrowtooth_flounder_adult))
annual_biomass_245 <- as.data.frame(sim_245_f1[[1]]$annual_Biomass)%>% 
  select(c(pacific_cod_adult, walleye_pollock_adult, arrowtooth_flounder_adult))
annual_biomass_585 <- as.data.frame(sim_585_f1[[1]]$annual_Biomass)%>% 
  select(c(pacific_cod_adult, walleye_pollock_adult, arrowtooth_flounder_adult))

annual_catch_126 <- as.data.frame(sim_126_f1[[1]]$annual_Catch)%>% 
  select(c(pacific_cod_adult, walleye_pollock_adult, arrowtooth_flounder_adult))
annual_catch_245 <- as.data.frame(sim_245_f1[[1]]$annual_Catch)%>% 
  select(c(pacific_cod_adult, walleye_pollock_adult, arrowtooth_flounder_adult))
annual_catch_585 <- as.data.frame(sim_585_f1[[1]]$annual_Catch)%>% 
  select(c(pacific_cod_adult, walleye_pollock_adult, arrowtooth_flounder_adult))

WGOA_CLIM_results <- bind_rows(
  biomass_126 = annual_biomass_126,
  biomass_245 = annual_biomass_245,
  biomass_585 = annual_biomass_585,
  catch_126   = annual_catch_126,
  catch_245   = annual_catch_245,
  catch_585   = annual_catch_585,
  .id = "run_type"
)

write.csv(WGOA_CLIM_results, "results/WGOA_JOINT_CLIM_results_v2.csv")

