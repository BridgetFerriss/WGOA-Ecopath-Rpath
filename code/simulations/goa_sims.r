library('viridis')
library('here')
source("code/simulations/goaclim_sim_v4.r")
source("code/bioenergetic_projections.r")
# arguments: scene, ssp, hcr, cons, resp, buf
test_run_126 <- clim_sim_hcr_obs_err(scene, 126, 1, TRUE, TRUE, TRUE)
test_run_245 <- clim_sim_hcr_obs_err(scene, 245, 1, TRUE, TRUE, TRUE)
test_run_585 <- clim_sim_hcr_obs_err(scene, 585, 1, TRUE, TRUE, TRUE)
