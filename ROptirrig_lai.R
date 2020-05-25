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
saveRes <- 0 #if 0 -> no saving if 1 -> saving activated


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
case_study_name <- "Aspres_coupling_alternative_p3"
# case_study_name <- "Aspres_noCoupling_IrriFixed12d_p3"
# case_study_name <- "Aspres_noCoupling_IrriOpt_p3"
# case_study_name <- "Aspres_noCoupling_IrriOptGGE_p3"
# case_study_name <- "Aspres_noCoupling_noIrri_p3"
# case_study_name <- "Aspres_coupling_alternative6_p3"

####### 2.2 Importation of meteo data input  [REQUIRED] #######
climate_file_name <- 'climate_buech_2016-2017.csv'
# date_start_sim <- as.Date("2016-10-15", "%Y-%m-%d"); date_end_sim <- as.Date("2017-10-14", "%Y-%m-%d")
date_start_sim <- as.Date("2016-10-15", "%Y-%m-%d"); date_end_sim <- as.Date("2017-07-31", "%Y-%m-%d")
input_meteo <- computeClimateInput(climate_file_name, date_start_sim, date_end_sim)
DOY_start_coupling <- 198;
DOY_stop_coupling <- dim(input_meteo)[1]; #DOY_stop_coupling <- 365; 

####### 2.3 Irrigation options  [REQUIRED] #######
coupling <- 1 #if 0 -> no coupling if 1 -> coupling activated with gravity-fed WatASit model
itest <- 0    #Choose fixed irrigation dates (itest = 0) or optimized irrigation dates (itest=1)
if (coupling == 1) {itest = 0; gge = 0; dosap =0; th_w = 0; coupling_scenario <- "Alternative"} #compulsory for collective irrigation in WatASit #Choose Baseline (without irrigation time slot) or Alternative (with irrigation time slot)
if (coupling == 0) { 
  if (itest == 0) {th_w = 0; gge = 0; dosap = 0}
  if (itest == 1) {th_w = 100; gge = 0; dosap = 43.2} } #Fill in irrigation dose dosap (in mm) if itest = 1 #Choose surface (gge = 0) or drip (gge = 1) irigation technology

####### 2.4 Generation of an Optirrig paramfile for each WatASit plots  #######
# list_idParcel <- optiParams('paramfiles/', case_study_name, 'watasit_winterCereals.csv', 'paramDBAllCereals.csv','climate_buech_2016-2017.csv', as.numeric(format(date_end_sim,"%Y")), 1, 365, 'irrig_file_watasit.dat')
# list_idParcel <- optiParams('paramfiles/', case_study_name, 'watasit_allIrrigatedCereals.csv', 'paramDBCereals.csv','climate_buech_2016-2017.csv', as.numeric(format(date_end_sim,"%Y")), jdsim = 1, jfsim = 365, irrigfile_name = 'NA', itest = 0, gge = 0, dosap = 0); warnings()
# list_idParcel <- optiParams(dir = 'paramfiles/', case_study_name = case_study_name, shapefile_name = 'watasit_IrrigatedCereals1.csv', paramDBfile_name = 'paramDBCereals.csv', climatefile_name = climate_file_name, jdsim = 1, jfsim = dim(input_meteo)[1], itest = itest, gge = gge, dosap = dosap); warnings()
list_idParcel <- optiParams(dir = 'paramfiles/', case_study_name = case_study_name, shapefile_name = 'watasit_IrrigatedCerealsOKp3.csv', paramDBfile_name = 'paramDBCereals.csv', climatefile_name = climate_file_name, jdsim = 1, jfsim = dim(input_meteo)[1], itest = itest, gge = gge, dosap = dosap, th_w = th_w); warnings()

# if (itest == 0) { #Give irrigation dates and doses for:
# I1 i surface irrigation:
I1   = matrix(0, nrow = dim(input_meteo)[1], ncol = length(list_idParcel))
# I1[207,] = 43.2; I1[219,] = 43.2; I1[231,] = 43.2; I1[243,] = 43.2; I1[255,] = 43.2; I1[267,] = 43.2; I1[279,] = 44  #; I1[305,] = 44; I1[320,] = 44;
# I2 is deep buried irrigation:
I2 = matrix(0, nrow = dim(input_meteo)[1], ncol = length(list_idParcel)) ;
# }


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
I1_mat <- matrix(NA, ncol = length(list_idParcel), nrow = dim(input_meteo)[1]); I2_mat = ET0_mat = EP_mat = R1_mat = R2_mat = R3_1_mat = d3_mat = R3_mat = teta3_mat = tetaRU3_mat = RU1_mat = RU2_mat =  RU3_mat = Sw_lai_mat = TPM2_mat = ETM_mat =  ETR_mat = TRP_mat = TS_mat =TS_p_mat = TT_mat = TT_p_mat = LAI_p_mat = LAI_mat = LAImax_mat = LAI_av_mat = LAIp_av_mat = TDM_mat = TDM_p_mat = imoins_mat =  fac_mat = crac_mat = crat_mat = dHz2_mat = ks_mat = Hz2_mat = taum_mat = kt_mat = I1_mat

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
for (day in 2:dim(input_meteo)[1]){
  inval2_list <- list()
  vect2_list <- list()
  
  if (coupling == 1) { if (day >= DOY_start_coupling && day <= DOY_stop_coupling) { irriDailyDose <- RCormas(case_study_name = case_study_name, day = day, DOY_start_coupling = DOY_start_coupling, DOY_stop_coupling = DOY_stop_coupling, input_meteo = input_meteo, modelName = "COWAT", parcelFile = "WatASit[2.0_LAI].pcl", init = "INIT_2017_54x44", scenario = coupling_scenario) 
  } } # make coupling with Cormas
  
  for (i in 1:length(list_idParcel)){
    cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
    
    if (coupling == 1) { if (day >= DOY_start_coupling && day <= DOY_stop_coupling) { 
      #if (length(irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),2]) != 0){
      I1[day,i] <- irriDailyDose[which(irriDailyDose$id == list_idParcel[i]),2]; cat("!!! coupled I1[day] =", I1[day,i], '\n') } }#} # Update irrigation from Cormas
    
    cat("Inputs:","\n","I1[day] = ", I1[day,i],"\n","I2[day] = ", I2[day,i],"\n"); #if(I1[day] != 0) {stop("I1[day] != 0")}
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
    setAttributesOfEntities("lai", "Ecrop", as.numeric(list_idParcel), as.numeric(LAI_mat[day,])) }}
  
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
  
}
