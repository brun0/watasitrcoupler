#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      Rcoupler[WatASit=Optirrig]      ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script couple the WatASit ABM (Richard et al., 2020) with 
# the Optirrig-D version of the Optirrig model (Cheviron et al., 2016)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2017-2020, by
# B. Bonté -> make RCormas function to get/set Cormas attributes/probes
# M. Delmas -> make the R Optirrig-D version
# B. Richard -> make this script and associated R funcions and
# generate case study data and parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 0. Cleaning & time reset #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all.names =T)); start_time <- Sys.time()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1. Settings #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1.1 Load functions #######
for(FileName in list.files("Rfunctions/", pattern="\\.[Rr]$")){ source(file.path("Rfunctions/",FileName))}

####### 1.2 Load libraries #######
load <- c(require(zoo), require (multiplex), require(tidyr),require(dplyr),require(doParallel))
if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); }

####### 1.3 Core parallelism [OPTIONAL] #######
cores <- parallel:::detectCores(); registerDoParallel(cores-1);

####### 1.4 Activate results saving [OPTIONAL] #######
saveRes <- T # Choose T or F. If F -> no saving, if T -> saving activated


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Config and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2.1 General #######
# simulation_name <- "EXAMPLE_Aspres_coupling_baseline" # To be specified
simulation_name <- "Aspres_coupling_baseline_watasit1.1.2_CoMSES"
date_start_sim <- as.Date("2016-10-15", "%Y-%m-%d") # To be specified
date_end_sim <- as.Date("2017-07-31", "%Y-%m-%d") # To be specified

####### 2.2 Importation of meteo input #######
climate_file_name <- 'climate_buech_2016-2017.csv' # Specified a file that keeps similar format (see example in climate_file folder)
input_meteo <- computeClimateInput(climate_file_name, date_start_sim, date_end_sim)

####### 2.3 Irrigation campaign options #######
coupling <- T # Choose F or T. If F -> no coupling, if T -> coupling activated with gravity-fed WatASit model

if (coupling) {
  model_name = "COWAT" # Cormas model name o be specified
  # parcel_file = "WatASit[2.2_TT].pcl" # Cormas parcel file To be specified
  parcel_file = "WatASit[1.1.2_CoMSES].pcl" # Cormas parcel file To be specified
  init_method = "INIT_2017_54x44" # Cormas init method to be specified
  coupling_scenario <- "Baseline" # Choose "Baseline" (without coordination of the network managed in WatASit) or "Alternative" (with daily slots coordination managed in WatASit)
  DOY_start_coupling <- 198 # To be specified (start of the irrigation campaign)
  DOY_stop_coupling <- dim(input_meteo)[1] # To be specified (end of the irrigation campaign)
  itest = 0 # Managed in WatASit
  gge = 0 # Not used if coupling = T
  dosap = 0 # Managed in WatASit
  th_w = 0 # Not used if coupling = T
  optirrig_slots <- F} # Managed in WatASit

if (!coupling) { 
  itest <- 0  # Choose fixed irrigation dates (itest = 0) or optimized irrigation dates (itest=1)
  dose <- 43.2 # To be specified in mm (irrigation dose injected on each plot in Optirrig)
  TM_spring <- 1200 # To be specified in °C (spring cereal is considered mature when TT >= TM_spring)
  TM_winter <- 1700 # To be specified in °C (winter cereal is considered mature when TT >= TM_winter) 
  optirrig_slots <- F # Choose F or T. If optirrig_slots = T -> activate daily slots at plot scale
  if (itest == 0) {th_w = 0; gge = 0; dosap = 0}
  if (itest == 1) {th_w = 100; gge = 0; dosap = dose} # Choose surface (gge = 0) or drip (gge = 1) irigation technology
}

####### 2.4 Importation of farm plot input and parameters  #######
plotfile_name <- 'watasit_IrrigatedCerealsOKp3.csv' # To be specified
paramDBfile_name <- 'paramDBCereals.csv' # To be specified
list_idParcel <- optiParams(dir = 'paramfiles/', case_study_name = simulation_name, plotfile_name = plotfile_name, paramDBfile_name = paramDBfile_name, climatefile_name = climate_file_name, jdsim = 1, jfsim = dim(input_meteo)[1], itest = itest, gge = gge, dosap = dosap, th_w = th_w); warnings() # To be specified

####### 2.5 Initialization of irrigation matrix  #######
I1 = matrix(0, nrow = dim(input_meteo)[1], ncol = length(list_idParcel)) # Initialize I1 surface irrigation
I2 = matrix(0, nrow = dim(input_meteo)[1], ncol = length(list_idParcel)) # Initialize I2 deep buried irrigation

if (!coupling) { if ( itest == 0 && !optirrig_slots){ # Give irrigation dates and doses for:
  I1[198,] = dose; I1[210,] = dose; I1[222,] = dose; I1[234,] = dose; I1[246,] = dose; I1[258,] = dose; I1[270,] = dose; I1[282,] = dose }} # To be specified if (coupling == F) and if ( itest == 0 && optirrig_slots == F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. Preparation of parameter files for Optirrig #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
param_frame <- data.frame(); irr <- data.frame()
for (i in 1:length(list_idParcel)){
  ####### 3.1 Load params of each plot #######
  param = read.csv(paste0('paramfiles/paramfiles_',simulation_name,'/',list_idParcel[i],'/parF', list_idParcel[i],'.csv'), header = TRUE,sep=",",dec = ".",stringsAsFactor=FALSE)
  
  ####### 3.2 Create frame with all parameters #######
  param_frame <- rbind(param_frame, param)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Run Optirrig #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
slot = 0; hist_irri <- rep(12,length(list_idParcel))
for (day in 2:dim(input_meteo)[1]){

  # Update precipitation conditions of the day 
  P<-input_meteo$P
  if (day == 2) {p_cumTenDays = P[day-1]}
  for (dd in 3:10){if (day == dd){ p_cumTenDays = sum(as.numeric(P[(day-(dd-1)):(day-1)]), na.rm = TRUE) }}
  if (day >= 11) {p_cumTenDays = sum(as.numeric(P[(day-10):(day-1)]), na.rm = TRUE)}
  p_cumTwelveDays = sum(c(p_cumTenDays,as.numeric(P[(day-12):(day-11)])), na.rm = TRUE) ; p_cumTwelveDays_vect[day] <- p_cumTwelveDays
  
  # Variable list initialization
  inval2_list <- list()
  vect2_list <- list()
  
  # # make coupling with WatASit if activated
  if (coupling) { if (day >= DOY_start_coupling && day <= DOY_stop_coupling) { irriDailyDose <- RCormas(case_study_name = simulation_name, day = day, DOY_start_coupling = DOY_start_coupling, DOY_stop_coupling = DOY_stop_coupling, input_meteo = input_meteo, modelName = model_name, parcelFile = parcel_file, init = init_method, scenario = coupling_scenario) 
  } } 
  
  # Irrigation option management if coupling is desactivated
  if (!coupling){ 
    if (day >= DOY_start_coupling){ slot = slot + 1
    if(slot == 11){slot <- 1}
    hist_irri <- hist_irri + 1
    slot_vect[day] <- slot
    if (p_cumTwelveDays >= 120) {I1[day] <- 0} 
    } 
  }
  
  # Crop state simulation in Optirrig
  for (i in 1:length(list_idParcel)){
    cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","slot = ", slot, "Precip = ",P[day]," mm","\n")
    
    # Force irrigation orders from WatASit if coupling is activated
    if (coupling) { if (day >= DOY_start_coupling && day <= DOY_stop_coupling) { 
      I1[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),2]; cat("!!! coupled I1[day] =", I1[day,i], '\n')
      askAct_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),4]
      floodAct_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),5]
      floodDRAct_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),6]
      askAff_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),7]
      floodAff_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),8]
      floodDRAff_mat[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),9]
    }
  }
    
    # Force daily slots coordination if coupling is not activated ##TO DO: make generic the code of the section below##
    if (optirrig_slots) { if(slot == 1 | slot == 2 | slot == 3){ if (list_idParcel[i] == 379236){ if((hist_irri[which(list_idParcel == list_idParcel[i])] >= 12) && (p_cumTwelveDays <= 120)) { I1[day,i] <- dose ; hist_irri[which(list_idParcel == list_idParcel[i])] <- 0} else {I1[day,i] <- 0}}}
      if(slot == 4 | slot == 5 | slot == 6){ if (list_idParcel[i] == 369276 | list_idParcel[i] == 405601 | list_idParcel[i] == 417443 | list_idParcel[i] == 420598) { if((hist_irri[which(list_idParcel == list_idParcel[i])] >= 12) && (p_cumTwelveDays <= 120)) { I1[day,i] <- dose ; hist_irri[which(list_idParcel == list_idParcel[i])] <- 0} else {I1[day,i] <- 0}}}
      if(slot == 7){ if (list_idParcel[i] == 369276 | list_idParcel[i] == 369279 | list_idParcel[i] == 417443 | list_idParcel[i] == 420518 | list_idParcel[i] == 420598){ if((hist_irri[which(list_idParcel == list_idParcel[i])] >= 12) && (p_cumTwelveDays <= 120)) { I1[day,i] <- dose ; hist_irri[which(list_idParcel == list_idParcel[i])] <- 0} else {I1[day,i] <- 0}}}
      if(slot == 8 | slot == 9 | slot == 10){ if (list_idParcel[i] == 369176 | list_idParcel[i] == 369185 | list_idParcel[i] == 376888 | list_idParcel[i] == 377143 | list_idParcel[i] == 377158 | list_idParcel[i] == 405599 | list_idParcel[i] == 405609 | list_idParcel[i] == 405610 | list_idParcel[i] == 417443 | list_idParcel[i] == 420602){ if((hist_irri[which(list_idParcel == list_idParcel[i])] >= 12) && (p_cumTwelveDays <= 120)) {I1[day,i] <- dose ; hist_irri[which(list_idParcel == list_idParcel[i])] <- 0} else {I1[day,i] <- 0}}}
    }
    
    if (!coupling && day >= DOY_start_coupling){ 
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
    
    # Daily time step
    cat("Inputs:","\n","I1[day] = ", I1[day,i],"\n","I2[day] = ", I2[day,i],"\n", "p_cumTwelveDays = ", p_cumTwelveDays, "\n")
    param<-param_frame[i,]; cstes<-cstes_list[i,];  inval<-inval_list[i,]; vect<-vect_list[i,]
    optirday = daily_optirr(param,
                            input_meteo,
                            cstes,
                            inval,
                            vect,
                            I1[,i], 
                            I2[,i],
                            day) 
    inval2 = optirday$inval ; inval2_list[[i]] <- inval2 ; inval_list[i,] <- inval2 # New constants
    vect2  = optirday$vect ; vect2_list[[i]] <- vect2 ; vect_list[i,] <- vect2 # New vectors
    
    # Record daily variables
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
  
  ####### 4.3 Set the new crop state in WatASit if coupling is activated #######
  if (coupling) { if (day >= DOY_start_coupling && day <= DOY_stop_coupling) {
    setAttributesOfEntities("tt", "Ecrop", as.numeric(list_idParcel), as.numeric(TT_mat[day,])) }}
  
} 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 5. Save simulation results #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (saveRes) {
  dir.create("save/simulations_Roptirrig/"); dir.create(paste0("save/simulations_Roptirrig/",simulation_name))
  saveResults(cstes_list, inval2_list, I1_mat, I2_mat, ET0_mat, EP_mat, R1_mat, R2_mat, R3_1_mat, d3_mat, R3_mat, teta3_mat, tetaRU3_mat, RU1_mat, RU2_mat, RU3_mat, Sw_lai_mat, TPM2_mat, ETM_mat, ETR_mat, TRP_mat, TS_mat, TS_p_mat, TT_mat, TT_p_mat, LAI_p_mat, LAI_mat, LAImax_mat, LAI_av_mat, LAIp_av_mat, TDM_mat, TDM_p_mat, imoins_mat, fac_mat, crac_mat, dHz2_mat, crat_mat, ks_mat, Hz2_mat, taum_mat, kt_mat, askAct_mat, floodAct_mat,
          floodDRAct_mat, askAff_mat, floodAff_mat, floodDRAff_mat, p_cumTwelveDays_vect, slot_vect, simulation_name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### End of simulation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
end_time <- Sys.time(); simu_time = end_time - start_time; cat ("................................................................",
                                                                "\n","Simulation time is ", round(simu_time,2), "minutes", "\n")
warnings()