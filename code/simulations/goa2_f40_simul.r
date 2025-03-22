# Sum of squares HCR1: minimize difference between end biomass and B_target
# sumsq <- function(sim) {
#   ss <- sum(log(Btarg_Fmeanlast5[managed_sp]/end_biomass(sim)[managed_sp])^2)
#   print(ss)
#   return(ss)
# }

# Sum of squares HCR3: minimize difference between end biomass and *B50*
# sumsq <- function(sim) {
#   ss <- sum(log(target_bio[managed_sp,"B50"]/end_biomass(sim)[managed_sp])^2)
#   print(ss)
#   return(ss)
# }

# Sum of squares for F35: minimize difference between end biomass and *B35*
# sumsq <- function(sim) {
#   ss <- sum(log(target_bio[managed_sp,"B35"]/end_biomass(sim)[managed_sp])^2)
#   print(ss)
#   return(ss)
# }

# Sum of squares F40: minimize difference between end biomass and *B40*
sumsq <- function(sim) {
  ss <- sum(log(target_bio[managed_sp,"B40"]/end_biomass(sim)[managed_sp])^2)
  print(ss)
  return(ss)
}

# Solve for F
obj <- function(pars) {
  # set all harvested species to F_equil (incl. non-targets)
  for(i in 32:110){
    scene$fishing$ForcedFRate[i,2:86] <- F_equil
  }
  # set F to pars for the managed stocks in the projection period
  for(i in 32:110){
    scene$fishing$ForcedFRate[i,managed_sp] <- pars
  }
  
  optrun <- rsim.run(scene, method="AB", years=all_years)
  out <- sumsq(optrun)
  return(out)
  
}


ptm <- proc.time()
set.seed(666)
pars <- rep(0.15,14) # w/o lg_sculpins and squids
# pars <- c(rep(0.1,20),2.3,0.1,0.7,0.3,0.6) # with lg_sculpins and squids
opt.ftarget <- optim(pars, obj, method="L-BFGS-B", lower=0, upper=2, hessian=FALSE, 
                     control=list(maxit=10000, trace=2, REPORT=1, factr=1e14))
proc.time() - ptm
# write.csv(opt.ftarget$par, file="C:/Users/andy.whitehouse/Work/src/aclim2_rpath/brp/F_target.csv")
# F50_opt <- opt.ftarget$par
# F35_opt <- opt.ftarget$par
F40_opt <- opt.ftarget$par
names(F40_opt) <- managed_sp
# F_target_opt <- opt.ftarget$par
write.csv(F40_opt, file="WGOA_source_data/F40_opt.csv")
# target_f <- cbind(F_target_opt,F35_opt,F40_opt,F50_opt,F_2015[managed_sp]) # F_2015 from a2_b40.r
# colnames(target_f) <- c("F_target","F35","F40","F50","F_2015")
# write.csv(target_f, file="C:/Users/andy.whitehouse/Work/src/aclim2_rpath/brp/F_target_21july2023_crab10_pred.csv",
#           row.names = TRUE)
# write.csv(target_f, file="C:/Users/andy.whitehouse/Work/src/aclim2_rpath/brp/F_target_crab101.csv",
#           row.names = TRUE)
# write.csv(target_f, file="C:/Users/andy.whitehouse/Work/src/aclim2_rpath/brp/F_target_7apr2023.csv",
#           row.names = TRUE)


################################################################################
# Sanity checks below here --------------------------------------------------- #

# run a sim with the converged params from opt.ftarget ----------------------- #
# names(opt.ftarget$par) <- managed_sp
# optf_12 <- opt.ftarget$par
# optf_13 <- opt.ftarget$par
# optf_14 <- opt.ftarget$par
# 
for(i in 32:110){
  scene$fishing$ForcedFRate[i,2:86] <- F_equil
}
# set F to opt.ftarget$par for the managed stocks in the projection period
for(i in 32:110){
  # scene$fishing$ForcedFRate[i,managed_sp] <- target_f[managed_sp,"F_target"]
  scene$fishing$ForcedFRate[i,managed_sp] <- F40_opt[managed_sp]
}
# 
ftarget.run1 <- rsim.run(scene, method="AB", years=all_years)
# # plot biomass
par(mfrow=c(3,5))
for(i in managed_sp){
  plot(1990:2099, ftarget.run1$annual_Biomass[,i], type='l', lwd=2,
       col=plasma(4)[1], ylab="Biomass", xlab="", main=i,
       ylim=c(0,max(ftarget.run1$annual_Biomass[,i],target_bio[i,"B0"])))
  abline(h=target_bio[i,"B40"], lty=2, lwd=2, col=plasma(4)[2])
  abline(h=target_bio[i,"B0"], lty=2, lwd=2, col=plasma(4)[3])
  # abline(h=target_bio[i,"B20"], lty=2, lwd=2, col=plasma(4)[4])
  abline(v=2020, lty=2)
  abline(h=0, lty=2, col="gray50")
}
# # plot biomass
# par(mfrow=c(5,5))
# for(i in managed_sp){
#   plot(1991:2099, ftarget.run14$annual_Biomass[,i], type='l', lwd=2, 
#        col=plasma(4)[1], ylab="Biomass", xlab="", main=i,
#        ylim=c(0,max(ftarget.run14$annual_Biomass[,i],target_bio[i,"B0"])))
#   abline(h=target_bio[i,"B40"], lty=2, lwd=2, col=plasma(4)[2])
#   abline(h=target_bio[i,"B0"], lty=2, lwd=2, col=plasma(4)[3])
#   abline(h=target_bio[i,"B20"], lty=2, lwd=2, col=plasma(4)[4])
#   abline(v=2022, lty=2)
#   abline(h=0, lty=2, col="gray50")
# }
# # plot catch
# par(mfrow=c(5,5))
# for(i in managed_sp){
#   plot(1991:2099, ftarget.run$annual_Catch[,i], type='l', lwd=2, 
#        col="blue", ylab="Catch", xlab="", main=i,
#        ylim=c(0,max(ftarget.run$annual_Catch[,i])))
#   abline(v=2022, lty=2)
# }

# (sum(ftarget.run$annual_Catch[dim(ftarget.run$annual_Catch)[1],capped_sp]))*533102
# write.Rpath(bal, file = "C:/Users/andy.whitehouse/Work/src/aclim2_workspaces/ebs_bal_morts.csv", morts = T)
