# March 21, 2025
source("wgoa_proto_sims_base.r")

clim_sim_hcr_obs_err <- function(scene, ssp, hcr, cons, resp, buf) {
  # scene <- scene
  
  print(paste0("climate_scenario_",ssp)) # or rcp
  if (ssp == 126) {
    ssp <- 126
  }
  if (ssp == 585) {
    ssp <- 585
  }
  if (ssp == 245) {
    ssp <- 245
  }
  
  hcr <- hcr
  print(paste0("HCR_",hcr))
  
  scene <- scene
  
  if (buf == FALSE) {
    # Set bottom-up forcing groups to during hindcast to default value of
    # negative 1 (i.e., no forced biomass)
    # scene$forcing$ForcedBio[1:372,"euphausiids"]      <- -1
    # scene$forcing$ForcedBio[1:372,"copepods"]         <- -1
    # scene$forcing$ForcedBio[1:372,"pelagic_microbes"] <- -1
    # scene$forcing$ForcedBio[1:372,"benthic_microbes"] <- -1
    scene$forcing$ForcedBio[1:372, "large_phytoplankton"] <- -1
    scene$forcing$ForcedBio[1:372, "small_phytoplankton"] <- -1
    
  }
  else {
    # -------------------------------------------------------------------------- #
    # Set-up climate forcing
    # climate persistance
    # ifelse(esm == "none" & ssp == "persist",
    #        # do this ---------------------------------------------
    #        if (esm == "c" & ssp == 45) {
    #          # set projection to mean of 2015:2019 (289:360)
    #          scene$forcing$ForcedBio[373:1068, "euphausiids"]      <- mean(scene$forcing$ForcedBio[289:360, "euphausiids"])
    #          scene$forcing$ForcedBio[373:1068, "copepods"]         <- mean(scene$forcing$ForcedBio[289:360, "copepods"])
    #          scene$forcing$ForcedBio[373:1068, "pelagic_microbes"] <- mean(scene$forcing$ForcedBio[289:360, "pelagic_microbes"])
    #          scene$forcing$ForcedBio[373:1068, "benthic_microbes"] <- mean(scene$forcing$ForcedBio[289:360, "benthic_microbes"])
    #          scene$forcing$ForcedBio[373:1068, "large_phytoplankton"] <- mean(scene$forcing$ForcedBio[289:360, "large_phytoplankton"])
    #          scene$forcing$ForcedBio[373:1068, "small_phytoplankton"] <- mean(scene$forcing$ForcedBio[289:360, "small_phytoplankton"])
    #
    #        }
    #        else {
    #          # set projection to mean of 2015:2019 (289:360)
    #          scene$forcing$ForcedBio[373:1308, "euphausiids"]      <- mean(scene$forcing$ForcedBio[289:360, "euphausiids"])
    #          scene$forcing$ForcedBio[373:1308, "copepods"]         <- mean(scene$forcing$ForcedBio[289:360, "copepods"])
    #          scene$forcing$ForcedBio[373:1308, "pelagic_microbes"] <- mean(scene$forcing$ForcedBio[289:360, "pelagic_microbes"])
    #          scene$forcing$ForcedBio[373:1308, "benthic_microbes"] <- mean(scene$forcing$ForcedBio[289:360, "benthic_microbes"])
    #          scene$forcing$ForcedBio[373:1308, "large_phytoplankton"] <- mean(scene$forcing$ForcedBio[289:360, "large_phytoplankton"])
    #          scene$forcing$ForcedBio[373:1308, "small_phytoplankton"] <- mean(scene$forcing$ForcedBio[289:360, "small_phytoplankton"])
    #
    #        },
    
    # else do this ----------------------------------------
    # if(esm == "c" & ssp == 45){
    #   # CLIMATE projection
    #   climate_file <- paste0("climate/", cmip, "_", esm, ssp, ".csv")
    #   climate_proj <- read.csv(climate_file, row.names = NULL)
    #   BaseB        <- scene$params$B_BaseRef
    #   names(BaseB) <- scene$params$spname
    #   # Set groups to force
    #   scene$forcing$ForcedBio[372:1068, "euphausiids"]        <- c((BaseB["euphausiids"]      * climate_proj[, "eup"]), (BaseB["euphausiids"] * climate_proj[696, "eup"]))
    #   scene$forcing$ForcedBio[372:1068, "copepods"]           <- c((BaseB["copepods"]         * climate_proj[, "cop"]), (BaseB["copepods"] * climate_proj[696, "cop"]))
    #   scene$forcing$ForcedBio[372:1068, "pelagic_microbes"]   <- c((BaseB["pelagic_microbes"] * climate_proj[, "mzl"]), (BaseB["pelagic_microbes"] * climate_proj[696, "mzl"]))
    #   scene$forcing$ForcedBio[372:1068, "benthic_microbes"]   <- c((BaseB["benthic_microbes"] * climate_proj[, "mzl"]), (BaseB["benthic_microbes"] * climate_proj[696, "mzl"]))
    #   scene$forcing$ForcedBio[372:1068, "large_phytoplankton"]   <- c((BaseB["large_phytoplankton"] * climate_proj[, "phl"]), (BaseB["large_phytoplankton"] * climate_proj[696, "phl"]))
    #   scene$forcing$ForcedBio[372:1068, "small_phytoplankton"]   <- c((BaseB["small_phytoplankton"] * climate_proj[, "phs"]), (BaseB["small_phytoplankton"] * climate_proj[696, "phs"]))
    #
    # }
    # else {
    # CLIMATE projection
    climate_file <-
      paste0(
        "WGOA_source_data/ROMSOutputWGOA/",
        "ssp",
        ssp,
        "_wide_WGOA_prod_300_anommaly",
        ".csv"
      )
    climate_proj <- read.csv(climate_file, row.names = NULL)
    BaseB        <- scene$params$B_BaseRef
    names(BaseB) <- scene$params$spname
    # Set groups to force
    # scene$forcing$ForcedBio[1:1320, "euphausiids"]        <- c((BaseB["euphausiids"]      * climate_proj[, "eup"]), (BaseB["euphausiids"] * climate_proj[936, "eup"]))
    # scene$forcing$ForcedBio[1:1320, "copepods"]           <- c((BaseB["copepods"]         * climate_proj[, "cop"]), (BaseB["copepods"] * climate_proj[936, "cop"]))
    # scene$forcing$ForcedBio[1:1320, "pelagic_microbes"]   <- c((BaseB["pelagic_microbes"] * climate_proj[, "mzl"]), (BaseB["pelagic_microbes"] * climate_proj[936, "mzl"]))
    # scene$forcing$ForcedBio[1:1320, "benthic_microbes"]   <- c((BaseB["benthic_microbes"] * climate_proj[, "mzl"]), (BaseB["benthic_microbes"] * climate_proj[936, "mzl"]))
    # scene$forcing$ForcedBio[1:1320, "large_phytoplankton"]   <-
    #   c((BaseB["large_phytoplankton"] * climate_proj[, "prod_PhL"]), (BaseB["large_phytoplankton"] * climate_proj[936, "prod_PhL"]))
    # scene$forcing$ForcedBio[1:1320, "small_phytoplankton"]   <-
    #   c((BaseB["small_phytoplankton"] * climate_proj[, "prod_PhS"]), (BaseB["small_phytoplankton"] * climate_proj[936, "prod_PhS"]))
    scene$forcing$ForcedBio[1:1320, "large_phytoplankton"]   <- BaseB["large_phytoplankton"] * climate_proj[121:1440, "prod_PhL"]
    scene$forcing$ForcedBio[1:1320, "small_phytoplankton"]   <- BaseB["small_phytoplankton"] * climate_proj[121:1440, "prod_PhS"]

  }
  
  
  # zero-trap: if biomass is <= 0 then make it = epsilon, otherwise leave it alone.
  epsilon <- 1 * 10 ^ -15 # a really small number > 0
  # scene$forcing$ForcedBio[, "euphausiids"]      <- ifelse(scene$forcing$ForcedBio[, "euphausiids"] < (BaseB["euphausiids"] * epsilon * 1.01),
  #                                                         (BaseB["euphausiids"] * epsilon * 1.01),
  #                                                         scene$forcing$ForcedBio[, "euphausiids"])
  # scene$forcing$ForcedBio[, "copepods"]      <- ifelse(scene$forcing$ForcedBio[, "copepods"] < (BaseB["copepods"] * epsilon * 1.01),
  #                                                      (BaseB["copepods"] * epsilon * 1.01),
  #                                                      scene$forcing$ForcedBio[, "copepods"])
  # scene$forcing$ForcedBio[, "pelagic_microbes"]      <- ifelse(scene$forcing$ForcedBio[, "pelagic_microbes"] < (BaseB["pelagic_microbes"] * epsilon * 1.01),
  #                                                              (BaseB["pelagic_microbes"] * epsilon * 1.01),
  #                                                              scene$forcing$ForcedBio[, "pelagic_microbes"])
  # scene$forcing$ForcedBio[, "benthic_microbes"]      <- ifelse(scene$forcing$ForcedBio[, "benthic_microbes"] < (BaseB["benthic_microbes"] * epsilon * 1.01),
  #                                                              (BaseB["benthic_microbes"] * epsilon * 1.01),
  #                                                              scene$forcing$ForcedBio[, "benthic_microbes"])
  scene$forcing$ForcedBio[, "large_phytoplankton"]      <-
    ifelse(
      scene$forcing$ForcedBio[, "large_phytoplankton"] < (BaseB["large_phytoplankton"] * epsilon * 1.01),
      (BaseB["large_phytoplankton"] * epsilon * 1.01),
      scene$forcing$ForcedBio[, "large_phytoplankton"]
    )
  scene$forcing$ForcedBio[, "small_phytoplankton"]      <-
    ifelse(
      scene$forcing$ForcedBio[, "small_phytoplankton"] < (BaseB["small_phytoplankton"] * epsilon * 1.01),
      (BaseB["small_phytoplankton"] * epsilon * 1.01),
      scene$forcing$ForcedBio[, "small_phytoplankton"]
    )
  


# -------------------------------------------------------------------------- #
# Set-up bioenergetic forcing
bioen_sp_noceph <- bioen_sp[!bioen_sp %in% c("octopus", "squid")]
# bioen_proj <- bioen_params(ssp)
# consumption multiplier
if (cons == TRUE) {
  # hindcast
  for (i in bioen_sp_noceph) {
    scene$forcing$ForcedSearch[1:372, i] <- tdc_hind_bt[, i]
  }
  # projection
  bioen_proj <- bioen_params(ssp)
  for (i in bioen_sp_noceph) {
    scene$forcing$ForcedSearch[373:1320, i] <- bioen_proj[[1]][, i]
  }


}

# respiration multiplier ---------- ---------- #
if (resp == TRUE) {
  # hindcast
  for (i in bioen_sp_noceph) {
    scene$forcing$ForcedActresp[1:372, i] <- tdr_hind_bt[, i]
  }
  # projection
  bioen_proj <- bioen_params(ssp)
  for (i in bioen_sp_noceph) {
    scene$forcing$ForcedActresp[373:1320, i] <- bioen_proj[[2]][, i]
  }
}

# -------------------------------------------------------------------------- #
# Run hindcast years
run.hind <- rsim.run(scene, method = 'AB', years = hind_years)
goaclim.sim <- run.hind

# object to store epsilon
epsilon_mat            <-
  matrix(nrow = (length(fore_years)), ncol = (length(managed_sp)))
row.names(epsilon_mat) <- fore_years
colnames(epsilon_mat)  <- managed_sp
# set autocorrelation and standard deviation of biomass estimates
# long-lived pars from Wiedenmann et al. (2015)
phi   <- 0.89
sigma_obs <- 0.34

# object to store "stock status"
ss_mat            <-
  matrix(nrow = (length(fore_years)), ncol = (length(managed_sp)))
row.names(ss_mat) <- fore_years
colnames(ss_mat)  <- managed_sp
# object to store ABC
abc_mat            <-
  matrix(nrow = (length(fore_years)), ncol = (length(managed_sp)))
row.names(abc_mat) <- fore_years
colnames(abc_mat)  <- managed_sp


# non-ATTACH scenarios ----------------------------------------------------- #
# if (attach == "none") {
#   # F_2015 scenario
#   if (hcr == "static") {
#     if (esm == "c" & ssp == 45) {
#       scene_2015 <- scene
#       # set F in projection period to F_2015
#       for (i in 32:89) {
#         scene_2015$fishing$ForcedFRate[i, 1:73] <- F_2015
#       }
#       aclim.sim <- rsim.run(scene_2015, method = 'AB', years = all_years)
#
#     }
#     else {
#       scene_2015 <- scene
#       # set F in projection period to F_2015
#       for (i in 32:109) {
#         scene_2015$fishing$ForcedFRate[i, 1:73] <- F_2015
#       }
#       aclim.sim <- rsim.run(scene_2015, method = 'AB', years = all_years)
#
#     }
#   }
#
#   else {
#     # F_0 scenario
#     if (hcr == 0) {
#       if (esm == "c" & ssp == 45) {
#         scene_0 <- scene
#         # set F in projection period to F_2015
#         for (i in 32:89) {
#           scene_0$fishing$ForcedFRate[i, 1:73] <- F_zero
#         }
#         aclim.sim <- rsim.run(scene_0, method = 'AB', years = all_years)
#
#       }
#       else {
#         scene_0 <- scene
#         # set F in projection period to F_2015
#         for (i in 32:109) {
#           scene_0$fishing$ForcedFRate[i, 1:73] <- F_zero
#         }
#         aclim.sim <- rsim.run(scene_0, method = 'AB', years = all_years)
#
#       }
#     }
#   }
# }

# projections with ATTACH -------------------------------------------------- #
# else {
for (yr in fore_years) {
  cat(yr, ssp, hcr, "\n")
  flush.console()
  Ftarget    <- F_equil
  Btarget    <- B_equil
  
  # Assessment with observation error
  # calculate epsilon
  if (yr == 2021) {
    epsilon_mat[as.character(yr), ] <-
      rnorm(length(managed_sp), 0, sigma_obs)
  }
  else {
    epsilon_mat[as.character(yr), ] <-
      phi * epsilon_mat[as.character(yr - 1), ] + sqrt(1 - phi ^ 2) * rnorm(length(managed_sp), 0, sigma_obs)
  }
  # epsilon_mat[yr] <- epsilon
  # do the assessment
  assessment <- end_biomass(goaclim.sim) #; print(assessment)
  groundfish_obs_error <-
    exp(epsilon_mat[as.character(yr), managed_sp] - 0.5 * sigma_obs ^ 2) #; print(groundfish_obs_error)
  assessment[managed_sp] <-
    assessment[managed_sp] * groundfish_obs_error #; print(assessment[managed_sp])
  # store assessed stock status
  ss_mat[as.character(yr), ] <- assessment[managed_sp]
  
  if (hcr == 2 & yr > 2022) {
    print(falpha_ssl)
  }
  
  if (hcr == 1) {
    Ftarget[managed_sp] <- F_target$F40
    Btarget[managed_sp] <- B_target$B40
    falpha_crab     <- 0.1
    fbeta_crab      <- 0.25 # Beta is 25% of B target (B35)
    falpha_ssl     <- 0.05
    fbeta_ssl      <- 0.50 # B20 (i.e., half of B40)
    falpha_nonssl     <- 0.05
    fbeta_nonssl      <- 0.05
  }
  
  if (hcr == 2) {
    Ftarget[managed_sp] <- F40
    Btarget[managed_sp] <- B40
    fbeta_crab      <-
      0.625 # cutoff is B25 (i.e., 62.5% of B40)
    fbeta_ssl      <- 0.625 # cutoff is B25 (i.e., 62.5% of B40)
    fbeta_nonssl      <-
      0.625 # cutoff is B25 (i.e., 62.5% of B40)
    
    if (yr == 2022) {
      # Ftarget[managed_sp] <- F40
      # Btarget[managed_sp] <- B40
      falpha_crab     <-
        rep(0.05, length(crab_sp))
      names(falpha_crab) <- crab_sp
      # fbeta_crab      <- 0.625 # cutoff is B25 (i.e., 62.5% of B40)
      # for(i in 1:length(ssl_sp)) {
      #   if(yr > 2022) {
      #     if(falpha_ssl[ssl_sp[i]] < 0.25) {falpha_ssl[ssl_sp[i]] == 0.05}
      # }
      falpha_ssl     <-
        rep(0.05, length(ssl_sp))
      names(falpha_ssl) <- ssl_sp #; print(falpha_ssl)
      # fbeta_ssl      <- 0.625 # cutoff is B25 (i.e., 62.5% of B40)
      falpha_nonssl     <-
        rep(0.05, length(nonssl_sp))
      names(falpha_nonssl) <- nonssl_sp
      # fbeta_nonssl      <- 0.625 # cutoff is B25 (i.e., 62.5% of B40)
    }
    # check if alpha is re-building alpha. if so keep "big" alpha until rebuilt, i.e., Bratio >= 1
    else {
      for (i in 1:length(crab_sp)) {
        ifelse(falpha_crab[i] >  0.05,
               falpha_crab[i] <- falpha_crab[i],
               falpha_crab[i] <- 0.05)
      }
      for (i in 1:length(ssl_sp)) {
        ifelse(falpha_ssl[i] >  0.05,
               falpha_ssl[i] <- falpha_ssl[i],
               falpha_ssl[i] <- 0.05)
      }
      for (i in 1:length(nonssl_sp)) {
        ifelse(
          falpha_nonssl[i] >  0.05,
          falpha_nonssl[i] <- falpha_nonssl[i],
          falpha_nonssl[i] <- 0.05
        )
      }
      # print(falpha_ssl)
    }
  }
  
  if (hcr == 3) {
    Ftarget[managed_sp] <- F50
    Btarget[managed_sp] <- B50
    falpha_crab     <- 0.05
    fbeta_crab      <- 0.4 # cutoff is B20 (i.e., 40% of B50)
    falpha_ssl     <- 0.05
    fbeta_ssl      <- 0.4 # cutoff is B20 (i.e., 40% of B50)
    falpha_nonssl     <- 0.05
    fbeta_nonssl      <- 0.4 # cutoff is B20 (i.e., 40% of B50)
  }
  
  # Create Species-indexed vector F_ABC:
  F_ABC <- Ftarget
  C_ABC <- F_ABC * end_biomass(goaclim.sim)
  
  # Institute control rule for crab species
  # sp <- crab_sp
  # # assessment <- end_biomass(goaclim.sim) #* rnorm(length(end_biomass(goaclim.sim)), mean=1, sd=0.2)
  # Bratio     <- assessment / Btarget
  # # Bstatus[tyr,sp] <- Bratio[sp]
  # if (hcr == 2) {
  #   for (i in 1:length(crab_sp)) {
  #     if (Bratio[crab_sp[i]] <  fbeta_crab) {falpha_crab[crab_sp[i]] <- 0.25}
  #     if (Bratio[crab_sp[i]] >= 1)          {falpha_crab[crab_sp[i]] <- 0.05}
  #   }
  #   F_ABC[sp]  <- ifelse((Bratio[sp] <= falpha_crab[sp]) | (Bratio[sp] <= fbeta_crab), 0.0,
  #                        ifelse(
  #                          Bratio[sp] < 1.0,
  #                          Ftarget[sp] * (Bratio[sp] - falpha_crab[sp]) / (1.0 - falpha_crab[sp]),
  #                          Ftarget[sp]
  #                        )
  #   )
  #   C_ABC[sp] <- F_ABC[sp] * assessment[sp]
  # }
  #
  # if(hcr == 1 | hcr == 3){
  #   F_ABC[sp]  <- ifelse((Bratio[sp] <= falpha_crab) | (Bratio[sp] <= fbeta_crab), 0.0,
  #                        ifelse(
  #                          Bratio[sp] < 1.0,
  #                          Ftarget[sp] * (Bratio[sp] - falpha_crab) / (1.0 - falpha_crab),
  #                          Ftarget[sp]
  #                        )
  #   )
  #   C_ABC[sp] <- F_ABC[sp] * assessment[sp]
  # }
  
  # Institute control rule for ssl prey species (pollock, p.cod, and atka)
  sp <- ssl_sp
  # assessment <- end_biomass(goaclim.sim) #* rnorm(length(end_biomass(goaclim.sim)), mean=1, sd=0.2)
  Bratio     <- assessment / Btarget
  if (hcr == 2) {
    for (i in 1:length(ssl_sp)) {
      if (Bratio[ssl_sp[i]] <  fbeta_ssl) {
        falpha_ssl[ssl_sp[i]] <- 0.25
      }
      if (Bratio[ssl_sp[i]] >= 1)         {
        falpha_ssl[ssl_sp[i]] <- 0.05
      }
    }
    F_ABC[sp]  <-
      ifelse((Bratio[sp] <= falpha_ssl[sp]) |
               (Bratio[sp] <= fbeta_ssl),
             0.0,
             ifelse(
               Bratio[sp] < 1.0,
               Ftarget[sp] * (Bratio[sp] - falpha_ssl[sp]) / (1.0 - falpha_ssl[sp]),
               Ftarget[sp]
             )
      )
    C_ABC[sp] <- F_ABC[sp] * assessment[sp]
  }
  
  if (hcr == 1 | hcr == 3) {
    F_ABC[sp]  <-
      ifelse((Bratio[sp] <= falpha_ssl) | (Bratio[sp] <= fbeta_ssl),
             0.0,
             ifelse(
               Bratio[sp] < 1.0,
               Ftarget[sp] * (Bratio[sp] - falpha_ssl) / (1.0 - falpha_ssl),
               Ftarget[sp]
             )
      )
    C_ABC[sp] <- F_ABC[sp] * assessment[sp]
  }
  
  # Institute control rule for capped species (Federal Groundfish)
  sp <- nonssl_sp
  # assessment <- end_biomass(goaclim.sim) #* rnorm(length(end_biomass(goaclim.sim)), mean=1, sd=0.2)
  # Bratio     <- assessment / Btarget
  if (hcr == 2) {
    for (i in 1:length(nonssl_sp)) {
      if (Bratio[nonssl_sp[i]] <  fbeta_nonssl) {
        falpha_nonssl[nonssl_sp[i]] <- 0.25
      }
      if (Bratio[nonssl_sp[i]] >= 1)            {
        falpha_nonssl[nonssl_sp[i]] <- 0.05
      }
    }
    F_ABC[sp]  <-
      ifelse((Bratio[sp] <= falpha_nonssl[sp]) |
               (Bratio[sp] <= fbeta_nonssl),
             0.0,
             ifelse(
               Bratio[sp] < 1.0,
               Ftarget[sp] * (Bratio[sp] - falpha_nonssl[sp]) / (1.0 - falpha_nonssl[sp]),
               Ftarget[sp]
             )
      )
    C_ABC[sp] <- F_ABC[sp] * assessment[sp]
  }
  
  if (hcr == 1 | hcr == 3) {
    F_ABC[sp]  <-
      ifelse((Bratio[sp] <= falpha_nonssl) |
               (Bratio[sp] <= fbeta_nonssl),
             0.0,
             ifelse(
               Bratio[sp] < 1.0,
               Ftarget[sp] * (Bratio[sp] - falpha_nonssl) / (1.0 - falpha_nonssl),
               Ftarget[sp]
             )
      )
    C_ABC[sp] <- F_ABC[sp] * assessment[sp]
  }
  
  # store ABCs
  # store assessed stock status
  abc_mat[as.character(yr), ] <- C_ABC[managed_sp]
  
  # Now that we have C_ABC, feed it to catchfunction
  # Scenarios
  # 1 - status quo
  # 2 - Pollock and Pcod dominated
  # 3 - Flatfish dominated
  # 4 - No fishing
  # Set up list for catch_function
  # sp <- capped_sp   # specify all federally managed groundfish
  # ABC_named <- c(attach, C_ABC[sp] * 533102)
  # names(ABC_named) <- c("scenario", names(sp))
  # ABC_list <- as.list(ABC_named) #; print(ABC_list)
  # # Call catch_function
  # TAC_out  <- do.call(catch_function, ABC_list) #; print(TAC_out)
  # # Copy catch_function ooutput back into C_ABC
  # C_ABC[sp] <- as.numeric(TAC_out) / 533102
  
  # Add resulting fishing to current year's CATCH matrix
  # scene <- adjust.fishing(scene, "CATCH", names(C_ABC), yr, value = C_ABC)
  scene$fishing$ForcedCatch[as.character(yr), names(C_ABC)] <-
    C_ABC
  
  # Run an rsim year
  goaclim.sim <-
    rsim.step(scene, goaclim.sim, method = 'AB', year.end = yr)
}
return(goaclim.sim)
# }
# outputs
# sim_out <- list(goaclim.sim, ss_mat, abc_mat)
# names(sim_out) <- c("sim_out", "ss_mat", "abc_mat")

# return(sim_out)

# return(aclim.sim)
}
