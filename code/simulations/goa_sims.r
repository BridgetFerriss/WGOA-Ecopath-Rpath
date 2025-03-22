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