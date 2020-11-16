#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R Optirrig      ######################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script drives the Optirrig-D version of the Optirrig model (Cheviron et al., 2016)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2019, October, by
# B. Bonté -> make RCormas function to get/set Cormas attributes/probes
# M. Delmas -> make adapted daily Optirrig function, adapt it for 
# meadows
# B. Richard -> make this script, make optiParams funcion and
# generate Optirrig climate file with specific R scripts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




rm(list=ls(all=T)); start_time <- Sys.time(); #sessionInfo()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1. Settings #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1.1 Load functions [REQUIRED] #######
for(FileName in list.files("Rfunctions/", pattern="\\.[Rr]$")){ source(file.path("Rfunctions/",FileName)); }
# is.numeric0 <- function(x) {identical(x, numeric(0))}

####### 1.2 Load libraries [REQUIRED] #######
load <- c(require(zoo), require (multiplex), require(tidyr),require(dplyr),require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

####### 1.3 Core parallelism [OPTIONAL] #######
cores <- parallel:::detectCores(); registerDoParallel(cores-1);

####### 1.4 Activate results saving [OPTIONAL] #######
saveRes <- 1 #if 0 -> no saving if 1 -> saving activated


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Config and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2.1 Specification of case study [REQUIRED] #######
# case_study_name <- "Aspres_coupling_baseline_cerealOK"
# case_study_name <- "Aspres_coupling_alternative_cerealOK"
# case_study_name <- "Aspres_noCoupling_noIrri_cerealOK"
# case_study_name <- "Aspres_noCoupling_IrriFixed12d_cerealOK"
# case_study_name <- "Aspres_noCoupling_IrriOpt_cerealOK"
# case_study_name <- "Aspres_noCoupling_noIrri_p2"
# case_study_name <- "Aspres_noCoupling_IrriFixed12d_p2"
# case_study_name <- "Aspres_coupling_baseline_p3"
# case_study_name <- "Aspres_coupling_alternative_p3"
# case_study_name <- "Aspres_noCoupling_IrriFixed12d_p3"
# case_study_name <- "Aspres_noCoupling_IrriOpt_p3"
# case_study_name <- "Aspres_noCoupling_IrriOptGGE_p3"
# case_study_name <- "Aspres_noCoupling_noIrri_p3"
# case_study_name <- "Aspres_coupling_alternative6_p3"
# case_study_name <- "Aspres_coupling_alternative_tt"
# case_study_name <- "Aspres_coupling_baseline_tt"
# case_study_name <- "Aspres_coupling_10dVariant2_tt"
# case_study_name <- "Aspres_coupling_8dVariant1_tt"
# case_study_name <- "Aspres_coupling_8dVariant2_tt"
# case_study_name <- "Aspres_coupling_12dVariant1_tt"
# case_study_name <- "Aspres_coupling_12dVariant2_tt"
# case_study_name <- "Aspres_coupling_baseline_watasit2.2_tt"
# case_study_name <- "Aspres_coupling_alternative_watasit2.2_tt"
# case_study_name <- "Aspres_noCoupling_dailySlots_Ref_pcum_watasit2.2_tt"
# case_study_name <- "Aspres_noCoupling_IrriFixed12d_Ref_pcum_watasit2.2_tt_stopTm"
# case_study_name <- "Aspres_noCoupling_dailySlots_Ref_pcum_watasit2.2_tt_stopTm"
# case_study_name <- "Aspres_coupling_baseline_watasit2.2_tt_stopTm"
# case_study_name <- "Aspres_coupling_alternative_watasit2.2_tt_stopTm"
case_study_name <- "Aspres_coupling_alternative_watasit2.2_tt_stopTm_aff"
# case_study_name <- "Aspres_coupling_baseline_watasit2.2_tt_stopTm_aff"

####### 2.2 Importation of meteo data input  [REQUIRED] #######
climate_file_name <- 'climate_buech_2016-2017.csv'
date_start_sim <- as.Date("2016-10-15", "%Y-%m-%d"); date_end_sim <- as.Date("2017-07-31", "%Y-%m-%d")
input_meteo <- computeClimateInput(climate_file_name, date_start_sim, date_end_sim)
DOY_start_coupling <- 198
DOY_stop_coupling <- dim(input_meteo)[1]

####### 2.3 Irrigation options  [REQUIRED] #######
coupling <- 1 # If 0 -> no coupling if 1 -> coupling activated with gravity-fed WatASit model
if (coupling == 1) {
  itest = 0 # Managed in WatASit
  gge = 0 # Not used if coupling = 1
  dosap = 0 # Managed in WatASit
  th_w = 0 # Not used if coupling = 1
  coupling_scenario <- "Alternative" # Choose "Baseline" (without irrigation coordination of the network) or "Alternative" (with irrigation daily slots managed in WatASit)
  optirrigDailySlot <- 0} # Managed in WatASit
if (coupling == 0) { 
  itest <- 0  # Choose fixed irrigation dates (itest = 0) or optimized irrigation dates (itest=1)
  dose <- 43.2 # In mm, for optirrig irrigation dose injected on each plot
  TM_spring <- 1200 # In °C, spring cereal is considered mature when TT >= TM_spring
  TM_winter <- 1700 # In °C, winter cereal is considered mature when TT >= TM_winter} 
  optirrigDailySlot <- 0 # Choose 0 or 1: if optirrigDailySlot = 1 -> activate dailySlot at plot scale
  if (itest == 0) {th_w = 0; gge = 0; dosap = 0}
  if (itest == 1) {th_w = 100; gge = 0; dosap = dose} # If itest = 1: choose surface (gge = 0) or drip (gge = 1) irigation technology
}
  



####### 2.4 Generation of an Optirrig paramfile for each WatASit plots  #######
# list_idParcel <- optiParams('paramfiles/', case_study_name, 'watasit_winterCereals.csv', 'paramDBAllCereals.csv','climate_buech_2016-2017.csv', as.numeric(format(date_end_sim,"%Y")), 1, 365, 'irrig_file_watasit.dat')
# list_idParcel <- optiParams('paramfiles/', case_study_name, 'watasit_allIrrigatedCereals.csv', 'paramDBCereals.csv','climate_buech_2016-2017.csv', as.numeric(format(date_end_sim,"%Y")), jdsim = 1, jfsim = 365, irrigfile_name = 'NA', itest = 0, gge = 0, dosap = 0); warnings()
# list_idParcel <- optiParams(dir = 'paramfiles/', case_study_name = case_study_name, shapefile_name = 'watasit_IrrigatedCereals1.csv', paramDBfile_name = 'paramDBCereals.csv', climatefile_name = climate_file_name, jdsim = 1, jfsim = dim(input_meteo)[1], itest = itest, gge = gge, dosap = dosap); warnings()
list_idParcel <- optiParams(dir = 'paramfiles/', case_study_name = case_study_name, shapefile_name = 'watasit_IrrigatedCerealsOKp3.csv', paramDBfile_name = 'paramDBCereals.csv', climatefile_name = climate_file_name, jdsim = 1, jfsim = dim(input_meteo)[1], itest = itest, gge = gge, dosap = dosap, th_w = th_w); warnings()


# I1 i surface irrigation:
I1   = matrix(0, nrow = dim(input_meteo)[1], ncol = length(list_idParcel)) # Initialize null matrix
# I2 is deep buried irrigation:
I2 = matrix(0, nrow = dim(input_meteo)[1], ncol = length(list_idParcel)) # Initialize null matrix
if (coupling == 0) { if ( itest == 0 && optirrigDailySlot == 0){#Give irrigation dates and doses for:
# I1[207,] = dose; I1[219,] = dose; I1[231,] = dose; I1[243,] = dose; I1[255,] = dose; I1[267,] = dose; I1[279,] = dose  #; I1[305,] = dose; I1[320,] = dose;
I1[198,] = dose; I1[210,] = dose; I1[222,] = dose; I1[234,] = dose; I1[246,] = dose; I1[258,] = dose; I1[270,] = dose; I1[282,] = dose }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. Initialization of Optirrig model #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
param_frame <- data.frame(); irr <- data.frame()
for (i in 1:length(list_idParcel)){
  ####### 3.1 Load params of each plot #######
  param = read.csv(paste0('paramfiles/paramfiles_',case_study_name,'/',list_idParcel[i],'/parF', list_idParcel[i],'.csv'), header = TRUE,sep=",",dec = ".",stringsAsFactor=FALSE)
  
  ####### 3.2 Create frame with all parameters #######
  param_frame <- rbind(param_frame, param)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Run Optirrig #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4.1 Initialize optirrig on first day #######
day = 1
cstes_list <- list(); inval_list <- list(); vect_list <- list(); 
I1_mat <- matrix(NA, ncol = length(list_idParcel), nrow = dim(input_meteo)[1]); I2_mat = ET0_mat = EP_mat = R1_mat = R2_mat = R3_1_mat = d3_mat = R3_mat = teta3_mat = tetaRU3_mat = RU1_mat = RU2_mat =  RU3_mat = Sw_lai_mat = TPM2_mat = ETM_mat =  ETR_mat = TRP_mat = TS_mat =TS_p_mat = TT_mat = TT_p_mat = LAI_p_mat = LAI_mat = LAImax_mat = LAI_av_mat = LAIp_av_mat = TDM_mat = TDM_p_mat = imoins_mat =  fac_mat = crac_mat = crat_mat = dHz2_mat = ks_mat = Hz2_mat = taum_mat = kt_mat = askAct_mat = floodAct_mat = floodDRAct_mat = askAff_mat = floodAff_mat = floodDRAff_mat = I1_mat; p_cumTwelveDays_vect <- is.vector(x = length(input_meteo$Date)); slot_vect <- is.vector(x = (length(input_meteo$Date)-DOY_start_coupling))
p_cumTwelveDays_vect[day] <- 0; slot_vect[day] <- 0

for (i in 1:length(list_idParcel)){
  init <- init_optirr(param_frame[i,], input_meteo)
  cstes = init$cstes; cstes_list <- rbind(cstes_list, cstes) # Constants
  inval = init$inval; inval_list <-  rbind(inval_list, inval) # Calculation values for which the history is not required (list of values)
  vect  = init$vect ; vect_list <-  rbind(vect_list, vect) # Vector of stored state variables as time series in vectors (list of vectors)
  
  ET0_mat[day,i] <- vect$ET0[day];  EP_mat[day,i] <- vect$EP[day]; R1_mat[day,i] <- vect$R1[day]; R2_mat[day,i] <- vect$R2[day];
  R3_1_mat[day,i] <- vect$R3_1[day]; d3_mat[day,i] <- vect$d3[day]; R3_mat[day,i] <- vect$R3[day]; teta3_mat[day,i] <- vect$teta3[day];
  tetaRU3_mat[day,i] <- vect$tetaRU3[day]; RU1_mat[day,i] <- vect$RU1[day]; RU2_mat[day,i] <- vect$RU2[day]; RU3_mat[day,i] <- vect$RU3[day];
  Sw_lai_mat[day,i] <- vect$Sw_lai[day]; TPM2_mat[day,i] <- vect$TPM2[day]; ETM_mat[day,i] <- vect$ETM[day]; ETR_mat[day,i] <- vect$ETR[day];
  TRP_mat[day,i] <- vect$TRP[day]; TS_mat[day,i] <- vect$TS[day]; TS_p_mat[day,i] <- vect$TS_p[day]; TT_mat[day,i] <- vect$TT[day];
  TT_p_mat[day,i] <- vect$TT_p[day];LAI_p_mat[day,i] <- vect$LAI_p[day]; LAI_mat[day,i] <- vect$LAI[day]; LAImax_mat[day,i] <- vect$LAImax[day];
  LAI_av_mat[day,i] <- vect$LAI_av[day]; LAIp_av_mat[day,i] <- vect$LAIp_av[day]; TDM_mat[day,i] <- vect$TDM[day]; TDM_p_mat[day,i] <- vect$TDM_p[day];
  imoins_mat[day,i] <- vect$imoins[day]; fac_mat[day,i] <- vect$fac[day]; crac_mat[day,i] <- vect$crac[day]; crat_mat[day,i] <- vect$crat[day];
  dHz2_mat[day,i] <- vect$dHz2[day]; ks_mat[day,i] <- vect$ks[day]; Hz2_mat[day,i] <- vect$Hz2[day]; taum_mat[day,i] <- vect$taum[day];
  kt_mat[day,i] <- vect$kt[day];
}

####### 4.2 Simulate Optirrig on other days #######
slot = 0; hist_irri <- rep(12,16)
for (day in 2:dim(input_meteo)[1]){
# for (day in 2:206){
  
  # Precipitation conditions of the day 
  P<-input_meteo$P
  if (day == 2) {p_cumTenDays = P[day-1]}
  for (dd in 3:10){if (day == dd){ p_cumTenDays = sum(as.numeric(P[(day-(dd-1)):(day-1)]), na.rm = TRUE) }}
  if (day >= 11) {p_cumTenDays = sum(as.numeric(P[(day-10):(day-1)]), na.rm = TRUE)}
  p_cumTwelveDays = sum(c(p_cumTenDays,as.numeric(P[(day-12):(day-11)])), na.rm = TRUE) ; p_cumTwelveDays_vect[day] <- p_cumTwelveDays
  
  # List initialization
  inval2_list <- list()
  vect2_list <- list()
  
  if (coupling == 1) { if (day >= DOY_start_coupling && day <= DOY_stop_coupling) { irriDailyDose <- RCormas(case_study_name = case_study_name, day = day, DOY_start_coupling = DOY_start_coupling, DOY_stop_coupling = DOY_stop_coupling, input_meteo = input_meteo, modelName = "COWAT", parcelFile = "WatASit[2.2_TT].pcl", init = "INIT_2017_54x44", scenario = coupling_scenario) 
  } } # make coupling with Cormas
  
  if (coupling == 0){ 
            if (day >= DOY_start_coupling){ slot = slot + 1
                                            if(slot == 11){slot <- 1}
                                            hist_irri <- hist_irri + 1
                                            slot_vect[day] <- slot
                                            if (p_cumTwelveDays >= 120) {I1[day] <- 0} 
                                            } 
                    }

  for (i in 1:length(list_idParcel)){
    cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","slot = ", slot, "Precip = ",P[day]," mm","\n")
    
    if (coupling == 1) { if (day >= DOY_start_coupling && day <= DOY_stop_coupling) { 
      #if (length(irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),2]) != 0){
      I1[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),2]; cat("!!! coupled I1[day] =", I1[day,i], '\n')
      askAct_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),4]
      floodAct_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),5]
      floodDRAct_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),6]
      askAff_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),7]
      floodAff_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),8]
      floodDRAff_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),9]
                                                                                    }
                        }#} # Update irrigation from Cormas
    
    if (optirrigDailySlot == 1) { if(slot == 1 | slot == 2 | slot == 3){ if (list_idParcel[i] == 379236){ if((hist_irri[which(list_idParcel == list_idParcel[i])] >= 12) && (p_cumTwelveDays <= 120)) { I1[day,i] <- dose ; hist_irri[which(list_idParcel == list_idParcel[i])] <- 0} else {I1[day,i] <- 0}}}
                                  if(slot == 4 | slot == 5 | slot == 6){ if (list_idParcel[i] == 369276 | list_idParcel[i] == 405601 | list_idParcel[i] == 417443 | list_idParcel[i] == 420598) { if((hist_irri[which(list_idParcel == list_idParcel[i])] >= 12) && (p_cumTwelveDays <= 120)) { I1[day,i] <- dose ; hist_irri[which(list_idParcel == list_idParcel[i])] <- 0} else {I1[day,i] <- 0}}}
                                  if(slot == 7){ if (list_idParcel[i] == 369276 | list_idParcel[i] == 369279 | list_idParcel[i] == 417443 | list_idParcel[i] == 420518 | list_idParcel[i] == 420598){ if((hist_irri[which(list_idParcel == list_idParcel[i])] >= 12) && (p_cumTwelveDays <= 120)) { I1[day,i] <- dose ; hist_irri[which(list_idParcel == list_idParcel[i])] <- 0} else {I1[day,i] <- 0}}}
                                  if(slot == 8 | slot == 9 | slot == 10){ if (list_idParcel[i] == 369176 | list_idParcel[i] == 369185 | list_idParcel[i] == 376888 | list_idParcel[i] == 377143 | list_idParcel[i] == 377158 | list_idParcel[i] == 405599 | list_idParcel[i] == 405609 | list_idParcel[i] == 405610 | list_idParcel[i] == 417443 | list_idParcel[i] == 420602){ if((hist_irri[which(list_idParcel == list_idParcel[i])] >= 12) && (p_cumTwelveDays <= 120)) {I1[day,i] <- dose ; hist_irri[which(list_idParcel == list_idParcel[i])] <- 0} else {I1[day,i] <- 0}}}
    }
    
    if (coupling == 0 && day >= DOY_start_coupling){ 
      if ((list_idParcel[i] == 369176) && (TT_mat[day-1,1] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 369185) && (TT_mat[day-1,2] >= TM_spring)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 369276) && (TT_mat[day-1,3] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 369279) && (TT_mat[day-1,4] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 376888) && (TT_mat[day-1,5] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 377143) && (TT_mat[day-1,6] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 377158) && (TT_mat[day-1,7] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 379236) && (TT_mat[day-1,8] >= TM_spring)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 405599) && (TT_mat[day-1,9] >= TM_spring)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 405601) && (TT_mat[day-1,10] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 405609) && (TT_mat[day-1,11] >= TM_spring)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 405610) && (TT_mat[day-1,12] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 417443) && (TT_mat[day-1,13] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 420518) && (TT_mat[day-1,14] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 420598) && (TT_mat[day-1,15] >= TM_winter)) {I1[day,i] <- 0}
      if ((list_idParcel[i] == 420602) && (TT_mat[day-1,16] >= TM_winter)) {I1[day,i] <- 0}
      }
    
    cat("Inputs:","\n","I1[day] = ", I1[day,i],"\n","I2[day] = ", I2[day,i],"\n", "p_cumTwelveDays = ", p_cumTwelveDays, "\n"); #if(I1[day] != 0) {stop("I1[day] != 0")}
    param<-param_frame[i,]; cstes<-cstes_list[i,];  inval<-inval_list[i,]; vect<-vect_list[i,]
    optirday = daily_optirr(param,
                            input_meteo,
                            cstes,
                            inval,
                            vect,
                            I1[,i], 
                            I2[,i], # Deep irrigation (buried drip)
                            day) # Time step
    inval2 = optirday$inval ; inval2_list[[i]] <- inval2 ; inval_list[i,] <- inval2 # New constants
    vect2  = optirday$vect ; vect2_list[[i]] <- vect2 ; vect_list[i,] <- vect2 # New vectors
    
    ET0_mat[day,i] <- vect2$ET0[day];  EP_mat[day,i] <- vect2$EP[day]; R1_mat[day,i] <- vect2$R1[day]; R2_mat[day,i] <- vect2$R2[day];
    R3_1_mat[day,i] <- vect2$R3_1[day]; d3_mat[day,i] <- vect2$d3[day]; R3_mat[day,i] <- vect2$R3[day]; teta3_mat[day,i] <- vect2$teta3[day];
    tetaRU3_mat[day,i] <- vect2$tetaRU3[day]; RU1_mat[day,i] <- vect2$RU1[day]; RU2_mat[day,i] <- vect2$RU2[day]; RU3_mat[day,i] <- vect2$RU3[day];
    Sw_lai_mat[day,i] <- vect2$Sw_lai[day]; TPM2_mat[day,i] <- vect2$TPM2[day]; ETM_mat[day,i] <- vect2$ETM[day]; ETR_mat[day,i] <- vect2$ETR[day];
    TRP_mat[day,i] <- vect2$TRP[day]; TS_mat[day,i] <- vect2$TS[day]; TS_p_mat[day,i] <- vect2$TS_p[day]; TT_mat[day,i] <- vect2$TT[day];
    TT_p_mat[day,i] <- vect2$TT_p[day];LAI_p_mat[day,i] <- vect2$LAI_p[day]; LAI_mat[day,i] <- vect2$LAI[day]; LAImax_mat[day,i] <- vect2$LAImax[day];
    LAI_av_mat[day,i] <- vect2$LAI_av[day]; LAIp_av_mat[day,i] <- vect2$LAIp_av[day]; TDM_mat[day,i] <- vect2$TDM[day]; TDM_p_mat[day,i] <- vect2$TDM_p[day];
    imoins_mat[day,i] <- vect2$imoins[day]; fac_mat[day,i] <- vect2$fac[day]; crac_mat[day,i] <- vect2$crac[day]; crat_mat[day,i] <- vect2$crat[day];
    dHz2_mat[day,i] <- vect2$dHz2[day]; ks_mat[day,i] <- vect2$ks[day]; Hz2_mat[day,i] <- vect2$Hz2[day]; taum_mat[day,i] <- vect2$taum[day];
    kt_mat[day,i] <- vect2$kt[day]; irr = optirday$irr ; I1_mat[day,i] <- irr$I1; I2_mat[day,i] <- irr$I2
    }
                                           
  ####### 4.3 Set the new crop LAI in WatASit #######
  if (coupling == 1) { if (day >= DOY_start_coupling && day <= DOY_stop_coupling) {
    setAttributesOfEntities("tt", "Ecrop", as.numeric(list_idParcel), as.numeric(TT_mat[day,])) }}
  
} 
end_time <- Sys.time(); simu_time = end_time - start_time; cat ("................................................................",
                                                                "\n","Simulation time is ", round(simu_time,2), "minutes", "\n")
warnings()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 5. Save simulation results #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (saveRes == 1) {
  
  dir.create("save/simulations_Roptirrig/"); dir.create(paste0("save/simulations_Roptirrig/",case_study_name))
  write.csv(cstes_list, paste0("save/simulations_Roptirrig/",case_study_name,"/","cstes.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(inval2_list, paste0("save/simulations_Roptirrig/",case_study_name,"/","inval2.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(I1_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","I1_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(I2_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","I2_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(ET0_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","ET0_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(EP_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","EP_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(R1_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","R1_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(R2_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","R2_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(R3_1_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","R3_1_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(d3_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","d3_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(R3_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","R3_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(teta3_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","teta3_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(tetaRU3_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","tetaRU3_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(RU1_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","RU1_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(RU2_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","RU2_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(RU3_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","RU3_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(Sw_lai_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","Sw_lai_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(TPM2_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","TPM2_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(ETM_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","ETM_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(ETR_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","ETR_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(TRP_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","TRP_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(TS_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","TS_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(TS_p_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","TS_p_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(TT_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","TT_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(TT_p_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","TT_p_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(LAI_p_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","LAI_p_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(LAI_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","LAI_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(LAImax_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","LAImax_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(LAI_av_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","LAI_av_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(LAIp_av_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","LAIp_av_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(TDM_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","TDM_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(TDM_p_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","TDM_p_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(imoins_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","imoins_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(fac_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","fac_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(crac_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","crac_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(crat_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","crat_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(dHz2_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","dHz2_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(ks_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","ks_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(Hz2_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","Hz2_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(taum_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","taum_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(kt_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","kt_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(askAct_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","askAct_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(floodAct_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","floodAct_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(floodDRAct_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","floodDRAct_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(askAff_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","askAff_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(floodAff_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","floodAff_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(floodDRAff_mat, paste0("save/simulations_Roptirrig/",case_study_name,"/","floodDRAff_mat.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(p_cumTwelveDays_vect, paste0("save/simulations_Roptirrig/",case_study_name,"/","pcumTwelveDays_vect.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  write.csv(slot_vect, paste0("save/simulations_Roptirrig/",case_study_name,"/","slot_vect.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
}
