# run growth function to estimate von Bertalannfy parameters for select species for  Ecopath
### function to get parameters of groth curves (von Bertalanffy, Gompertz, logistic)
#modified function for get_lw (length weight regression)
#uses fishmethods package in R (https://cran.r-project.org/web/packages/fishmethods/fishmethods.pdf)
setwd("C:/Users/bridget.ferriss/Work/Rpath/GOA_Rpath_git/Growth VonBert calcs EwE")
 source("REEM_fooddata_functions_AgedOnly_ForVonB.r") #note added a filter to only include specimens that have l/w/ and aged
setwd("C:/Users/bridget.ferriss/Work/Rpath/GOA_Rpath_git/Growth VonBert calcs EwE") #data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAASCAYAAABB7B6eAAABCklEQVR42p2ULQ7CMBiGISQIDAqHxKLxOwAX2Am4ACEkKAQ3IFkgbbd1XVJH0Hg0FskFsBhKi4L2XdJvb1K150nWn/frrDdbw3MVLFGoZecnsVwQY0y3QX4LUaVUDkZr3W+QX6xQCZWDyTI9bJCfnMsplYM5SDlGssjVw32jcjDHspxgub4zpkdUDoYV9azhGK5Zdh5QORgh6jmSWa5O9rJ7VA7vRFQL/PbrfRsuiIVWSHR/3IbzjqhKXYmQ5EpH5bxLVokrD5Jc2ajc/7HYsnxLAyRXMioXFM2VBUl+0WK4YFRY6IYkf1TEcMGws9AFSf6wi+GCcc0LJZHkj+sYDr31HdyufdttOD8f3G/pQdD2FdIAAAAASUVORK5CYII=
source("REEM_download_functions.R")
 source("GAP_get_CPUE.R")
 source("GAP_get_biomass_Stratum.R")
setwd("C:/Users/bridget.ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/R") 
 source("pred_specification for LW regression.r")# (previously "dietcomp_Steps.r")
setwd("C:/Users/bridget.ferriss/Work/Rpath/GOA_Rpath_git/Growth VonBert calcs EwE")
 source("get_growth.R") #vonB growth function

#test code to get von Bertalanffy growth curve paramters
get_vonBgrowth (predator="P.cod", model="EBS", years=NULL, all.data=T)
#years=NULL=all years or years=1982:2021


## Filter data to only include samples that are aged and have length/weights (already done) - so use same samples to calc von bertalanfy and length/weigth regression

#!. Q1 - in get_LW transofrm log(a) back to a before output (line 124) - but table heading in output of function says 'log_lw_a'. is it backtransformed? 
# I initially tranformend length from mm to cm before regression so 

# get predator names from predparams list

#Sablefish - GOA-wide (model = WGOA+EGOA)
GOA_gr_SB=get_vonBgrowth(predator="Sablefish", model=c("WGOA","EGOA"), years=1990:2019, all.data=T)
length(GOA_gr_SB[,2]); GOA_gr_SB$linf[1]; GOA_gr_SB$k[1]; GOA_gr_SB$t0[1]
min(GOA_gr_SB$length); max(GOA_gr_SB$length)

##  WGOA Calcualte growth parameters from von Betalanffy 

WGOA_gr_Pcod=get_vonBgrowth(predator="P.cod", model="WGOA", years=1990:2019, all.data=T)
length(WGOA_gr_Pcod[,2]); WGOA_gr_Pcod$linf[1]; WGOA_gr_Pcod$k[1]; WGOA_gr_Pcod$t0[1]
min(WGOA_gr_Pcod$length); max(WGOA_gr_Pcod$length)

WGOA_gr_Pol=get_vonBgrowth(predator="W.pollock", model="WGOA", years=1990:2019, all.data=T)
length(WGOA_gr_Pol[,2]); WGOA_gr_Pol$linf[1]; WGOA_gr_Pol$k[1]; WGOA_gr_Pol$t0[1]
min(WGOA_gr_Pol$length); max(WGOA_gr_Pol$length)

WGOA_gr_ATF=get_vonBgrowth(predator="Arrowtooth", model="WGOA", years=1990:2019, all.data=T)
length(WGOA_gr_ATF[,2]); WGOA_gr_ATF$linf[1]; WGOA_gr_ATF$k[1]; WGOA_gr_ATF$t0[1]
min(WGOA_gr_ATF$length); max(WGOA_gr_ATF$length)

#WGOA_gr_Hal=get_vonBgrowth(predator="P.halibut", model="WGOA", years=1990:2019, all.data=T)
#length(WGOA_gr_Hal[,2]); WGOA_gr_Hal$linf[1]; WGOA_gr_Hal$k[1]; WGOA_gr_Hal$t0[1]
#min(WGOA_gr_Hal$length); max(WGOA_gr_Hal$length)

WGOA_gr_FS=get_vonBgrowth(predator="F.sole", model="WGOA", years=1990:2019, all.data=T)
length(WGOA_gr_FS[,2]); WGOA_gr_FS$linf[1]; WGOA_gr_FS$k[1]; WGOA_gr_FS$t0[1]
min(WGOA_gr_FS$length); max(WGOA_gr_FS$length)

WGOA_gr_RS=get_vonBgrowth(predator="R.sole", model="WGOA", years=1990:2019, all.data=T)
length(WGOA_gr_RS[,2]); WGOA_gr_RS$linf[1]; WGOA_gr_RS$k[1]; WGOA_gr_RS$t0[1]
min(WGOA_gr_RS$length); max(WGOA_gr_RS$length)

WGOA_gr_SB=get_vonBgrowth(predator="Sablefish", model="WGOA", years=1990:2019, all.data=T)
length(WGOA_gr_SB[,2]); WGOA_gr_SB$linf[1]; WGOA_gr_SB$k[1]; WGOA_gr_SB$t0[1]
min(WGOA_gr_SB$length); max(WGOA_gr_SB$length)

#thornyhead - not enough data
#WGOA_gr_ST=get_vonBgrowth(predator="S.thornyhead", model="WGOA", years=1990:2019, all.data=T)
#length(WGOA_gr_ST[,2]); WGOA_gr_ST$linf[1]; WGOA_gr_ST$k[1]; WGOA_gr_ST$t0[1]
#min(WGOA_gr_ST$length); max(WGOA_gr_ST$length)

WGOA_gr_POP=get_vonBgrowth(predator="PO.perch", model="WGOA", years=1990:2019, all.data=T)
length(WGOA_gr_POP[,2]); WGOA_gr_POP$linf[1]; WGOA_gr_POP$k[1]; WGOA_gr_POP$t0[1]
min(WGOA_gr_POP$length_mm); max(WGOA_gr_POP$length_mm)

#output table WGOA
linf_list_WGOA=rbind (WGOA_gr_Pcod$linf[1], WGOA_gr_Pol$linf[1], WGOA_gr_ATF$linf[1],WGOA_gr_FS$linf[1],WGOA_gr_RS$linf[1],WGOA_gr_SB$linf[1], WGOA_gr_POP$linf[1]) #WGOA_gr_ST$linf[1],
k_list_WGOA=rbind (WGOA_gr_Pcod$k[1], WGOA_gr_Pol$k[1], WGOA_gr_ATF$k[1],WGOA_gr_FS$k[1],WGOA_gr_RS$k[1],WGOA_gr_SB$k[1], WGOA_gr_POP$k[1]) #,WGOA_gr_ST$k[1]
t0_list_WGOA=rbind (WGOA_gr_Pcod$t0[1], WGOA_gr_Pol$t0[1], WGOA_gr_ATF$t0[1],WGOA_gr_FS$t0[1],WGOA_gr_RS$t0[1],WGOA_gr_SB$t0[1], WGOA_gr_POP$t0[1]) #,WGOA_gr_ST$t0[1]
pred_list_WGOA=rbind ("P. cod","W.pollock", "Arrowtooth",  "F.sole", "R.sole", "Sablefish", "PO.perch") #, "S.thornyhead"
n_list_WGOA=rbind (length(WGOA_gr_Pcod[,2]), length(WGOA_gr_Pol[,2]), length(WGOA_gr_ATF[,2]), length(WGOA_gr_FS[,2]), length(WGOA_gr_RS[,2]), length(WGOA_gr_SB[,2]),  length(WGOA_gr_POP[,2])) #length(WGOA_gr_ST[,2]),
min_list_WGOA=rbind (min(WGOA_gr_Pcod$length_mm), min(WGOA_gr_Pol$length_mm), min(WGOA_gr_ATF$length_mm), min(WGOA_gr_FS$length_mm), min(WGOA_gr_RS$length_mm), min(WGOA_gr_SB$length_mm),  min(WGOA_gr_POP$length_mm)) #min(WGOA_gr_ST$length_mm),
max_list_WGOA=rbind (max(WGOA_gr_Pcod$length_mm), max(WGOA_gr_Pol$length_mm), max(WGOA_gr_ATF$length_mm), max(WGOA_gr_FS$length_mm), max(WGOA_gr_RS$length_mm), max(WGOA_gr_SB$length_mm),  max(WGOA_gr_POP$length_mm)) #max(WGOA_gr_ST$length_mm),

cbind(pred_list_WGOA, linf_list_WGOA, k_list_WGOA, t0_list_WGOA, n_list_WGOA, min_list_WGOA, max_list_WGOA) 

########################################################
### EGOA von Bertalanffy Growth Parameters
########################################################

##  EGOA Calcualte growth parameters from von Betalanffy 

EGOA_gr_Pcod=get_vonBgrowth(predator="P.cod", model="EGOA", years=1990:2019, all.data=T)
length(EGOA_gr_Pcod[,2]); EGOA_gr_Pcod$linf[1]; EGOA_gr_Pcod$k[1]; EGOA_gr_Pcod$t0[1]
min(EGOA_gr_Pcod$length); max(EGOA_gr_Pcod$length)

EGOA_gr_Pol=get_vonBgrowth(predator="W.pollock", model="EGOA", years=1990:2019, all.data=T)
length(EGOA_gr_Pol[,2]); EGOA_gr_Pol$linf[1]; EGOA_gr_Pol$k[1]; EGOA_gr_Pol$t0[1]
min(EGOA_gr_Pol$length); max(EGOA_gr_Pol$length)

EGOA_gr_ATF=get_vonBgrowth(predator="Arrowtooth", model="EGOA", years=1990:2019, all.data=T)
length(EGOA_gr_ATF[,2]); EGOA_gr_ATF$linf[1]; EGOA_gr_ATF$k[1]; EGOA_gr_ATF$t0[1]
min(EGOA_gr_ATF$length); max(EGOA_gr_ATF$length)

#EGOA_gr_Hal=get_vonBgrowth(predator="P.halibut", model="EGOA", years=1990:2019, all.data=T)
#length(EGOA_gr_Hal[,2]); EGOA_gr_Hal$linf[1]; EGOA_gr_Hal$k[1]; EGOA_gr_Hal$t0[1]
#min(EGOA_gr_Hal$length); max(EGOA_gr_Hal$length)

EGOA_gr_FS=get_vonBgrowth(predator="F.sole", model="EGOA", years=1990:2019, all.data=T)
length(EGOA_gr_FS[,2]); EGOA_gr_FS$linf[1]; EGOA_gr_FS$k[1]; EGOA_gr_FS$t0[1]
min(EGOA_gr_FS$length); max(EGOA_gr_FS$length)

EGOA_gr_RS=get_vonBgrowth(predator="R.sole", model="EGOA", years=1990:2019, all.data=T)
length(EGOA_gr_RS[,2]); EGOA_gr_RS$linf[1]; EGOA_gr_RS$k[1]; EGOA_gr_RS$t0[1]
min(EGOA_gr_RS$length); max(EGOA_gr_RS$length)

EGOA_gr_SB=get_vonBgrowth(predator="Sablefish", model="EGOA", years=1990:2019, all.data=T)
length(EGOA_gr_SB[,2]); EGOA_gr_SB$linf[1]; EGOA_gr_SB$k[1]; EGOA_gr_SB$t0[1]
min(EGOA_gr_SB$length); max(EGOA_gr_SB$length)

#EGOA_gr_ST=get_vonBgrowth(predator="S.thornyhead", model="EGOA", years=1990:2019, all.data=T)
#length(EGOA_gr_ST[,2]); EGOA_gr_ST$linf[1]; EGOA_gr_ST$k[1]; EGOA_gr_ST$t0[1]
#min(EGOA_gr_ST$length); max(EGOA_gr_ST$length)

EGOA_gr_POP=get_vonBgrowth(predator="PO.perch", model="EGOA", years=1990:2019, all.data=T)
length(EGOA_gr_POP[,2]); EGOA_gr_POP$linf[1]; EGOA_gr_POP$k[1]; EGOA_gr_POP$t0[1]
min(EGOA_gr_POP$length); max(EGOA_gr_POP$length)


#output table EGOA
linf_list_EGOA=rbind (EGOA_gr_Pcod$linf[1], EGOA_gr_Pol$linf[1], EGOA_gr_ATF$linf[1],EGOA_gr_FS$linf[1],EGOA_gr_RS$linf[1],EGOA_gr_SB$linf[1], EGOA_gr_POP$linf[1]) #EGOA_gr_ST$linf[1],
k_list_EGOA=rbind (EGOA_gr_Pcod$k[1], EGOA_gr_Pol$k[1], EGOA_gr_ATF$k[1],EGOA_gr_FS$k[1],EGOA_gr_RS$k[1],EGOA_gr_SB$k[1], EGOA_gr_POP$k[1]) #,EGOA_gr_ST$k[1]
t0_list_EGOA=rbind (EGOA_gr_Pcod$t0[1], EGOA_gr_Pol$t0[1], EGOA_gr_ATF$t0[1],EGOA_gr_FS$t0[1],EGOA_gr_RS$t0[1],EGOA_gr_SB$t0[1], EGOA_gr_POP$t0[1]) #,EGOA_gr_ST$t0[1]
pred_list_EGOA=rbind ("P. cod","W.pollock", "Arrowtooth",  "F.sole", "R.sole", "Sablefish", "PO.perch") #, "S.thornyhead"
n_list_EGOA=rbind (length(EGOA_gr_Pcod[,2]), length(EGOA_gr_Pol[,2]), length(EGOA_gr_ATF[,2]), length(EGOA_gr_FS[,2]), length(EGOA_gr_RS[,2]), length(EGOA_gr_SB[,2]),  length(EGOA_gr_POP[,2])) #length(EGOA_gr_ST[,2]),
min_list_EGOA=rbind (min(EGOA_gr_Pcod$length_mm), min(EGOA_gr_Pol$length_mm), min(EGOA_gr_ATF$length_mm), min(EGOA_gr_FS$length_mm), min(EGOA_gr_RS$length_mm), min(EGOA_gr_SB$length_mm),  min(EGOA_gr_POP$length_mm)) #min(EGOA_gr_ST$length_mm),
max_list_EGOA=rbind (max(EGOA_gr_Pcod$length_mm), max(EGOA_gr_Pol$length_mm), max(EGOA_gr_ATF$length_mm), max(EGOA_gr_FS$length_mm), max(EGOA_gr_RS$length_mm), max(EGOA_gr_SB$length_mm),  max(EGOA_gr_POP$length_mm)) #max(EGOA_gr_ST$length_mm),

cbind(pred_list_EGOA, linf_list_EGOA, k_list_EGOA, t0_list_EGOA, n_list_EGOA, min_list_EGOA, max_list_EGOA) 
