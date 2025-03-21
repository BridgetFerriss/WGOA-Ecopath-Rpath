# Solve for B zero without climate projections

# Use base scenario object that includes historical fishing (and/or F_equil)
# and includes the hindcast forcing (Jan 1991-Dec 2020) for phytoplankton and
# bioenergetic forcing for managed groundfish.

# Reduce F to zero for a target species while holding all other species at 
# *constant* F and run out to quasi-equilibrium. Repeat for all managed 
# species/stocks. Bzero is the ending biomass in 2099. If transient dynamics are
# still present, an alternative would be mean biomass over last 5 or 10 years.

# During the projection period (2021-2099) all groups set to F_equil (i.e. Ecopath F).
# Then set the exploitation rate for all federally managed groundfish stocks to
# their respective mean F rate over the last 5 years of the hindcast period (2016–
# 2020). Then set one of the target species F equal to zero. That stocks biomass
# at the end of the century is their Bzero.

# ---------------------------------------------------------------------------- #
# insert bioenergetic forcing during hindcast here

# consumption multiplier

# respiration multiplier

# ---------------------------------------------------------------------------- #
# run the hindcast years
run.hind <- rsim.run(scene, method = 'AB', years = hind_years)
# calculate the mean F from 2016–2020
catch_last5 <- 
  run.hind$annual_Catch[(dim(run.hind$annual_Catch)[1] - 5):dim(run.hind$annual_Catch)[1], managed_sp]
bio_last5   <-
  run.hind$annual_Biomass[(dim(run.hind$annual_Biomass)[1] - 5):dim(run.hind$annual_Biomass)[1], managed_sp]
F_last5 <- catch_last5 / bio_last5
F_meanlast5 <- apply(F_last5, 2, 'mean')

# output vector for the Bzeros
B0_Fmeanlast5 <- vector(mode = "numeric", length = length(managed_sp))
names(B0_Fmeanlast5) <- managed_sp
# set F in projection period to F_equil for all species (incl. non-targets, etc.)
for (i in 32:110) {
  scene$fishing$ForcedFRate[i, 2:86] <- F_equil
}
# set F to F_meanlast5 for the managed stocks in the projection period
for (i in 32:110) {
  scene$fishing$ForcedFRate[i, managed_sp] <- F_meanlast5
}
# loop through managed stocks
ptm <- proc.time()
for (i in managed_sp) {
  # set F to zero for i
  scene$fishing$ForcedFRate[32:110, i] <- 0
  # run the sim
  B0.sim_Fmeanlast5 <- rsim.run(scene, method = 'AB', years = all_years)
  # store end_biomass for i
  B0_Fmeanlast5[i] <- end_biomass(B0.sim_Fmeanlast5)[i]
  # reset the Frates to F_equil[i]
  for (j in 32:110) {
    scene$fishing$ForcedFRate[j, managed_sp] <- F_meanlast5
  }
}
proc.time() - ptm

# Get B40 and biomass values of potential interest
B20_Fmeanlast5 <- 0.20 * B0_Fmeanlast5
B25_Fmeanlast5 <- 0.25 * B0_Fmeanlast5
B35_Fmeanlast5 <- 0.35 * B0_Fmeanlast5
B40_Fmeanlast5 <- 0.40 * B0_Fmeanlast5
B50_Fmeanlast5 <- 0.50 * B0_Fmeanlast5
# Can also get limit biomass if that is of interest?


target_bio <- cbind(B20_Fmeanlast5,B25_Fmeanlast5,B35_Fmeanlast5,B40_Fmeanlast5,
                    B50_Fmeanlast5,B0_Fmeanlast5)#,Blim_Fmeanlast5)
colnames(target_bio) <- c("B20","B25","B35","B40","B50","B0")#,"B_limit")
# save to file
write.csv(target_bio, file="WGOA_source_data/B_target.csv", row.names = TRUE)
