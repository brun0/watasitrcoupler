#####################################################
loadLibraries = function() {
  load <- c(require(ConfigParser), require(R.utils), require(RSQLite), require(feather), require(zoo), require (multiplex), require(tidyr), require(ggplot2), require(dplyr), require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };
}

#####################################################
readConfigFile = function() {
  
args = commandArgs(trailingOnly=TRUE)
DEBUG = FALSE
configFilePath = "./rcoupler.cfg"
for (arg in args) {
  if (arg == '-d') {
    DEBUG = TRUE
  } else {
    configFilePath = arg
  }
}
configFileName = basename(configFilePath)
configFileDir = dirname(configFilePath)
stderrP = FALSE
stdoutP = FALSE
if (DEBUG) {
  stderrP = ""
  stdoutP = ""
}
config = ConfigParser$new(NULL)
config$read(configFilePath)

jamsRootPath = config$get("jamsRoot", NA, "tools")
if (!isAbsolutePath(jamsRootPath)) {
  jamsRootPath = paste(configFileDir, jamsRootPath, sep="/")
}
jamsStarterPath = paste(jamsRootPath, "jams-starter.jar", sep="/")

cormasRootPath = config$get("cormasRoot", NA, "tools")
if (!isAbsolutePath(cormasRootPath)) {
  cormasRootPath = paste(configFileDir, cormasRootPath, sep="/")
}
cormasPath = paste(cormasRootPath, "cormas.im", sep="/")
vwPath = paste(cormasRootPath, "..", "bin", "win", "visual.exe", sep="/")

requiredFiles = c(jamsStarterPath, cormasPath, vwPath)
for (path in requiredFiles) {
  if (!file.exists(path)) {
    cat(paste("File ", path, " not found.\n", sep=""))
    quit(status=1)
  }
}

return <- c(jamsRootPath,stderrP,stdoutP)
}
#####################################################
initializeJ2K = function(jamsRootPath, jams_file_name, stdoutP, stderrP, wd){
  killJ2K() # kill jams if it's running
  setwd(jamsRootPath)
  system2(
    'java',
    args=c('-jar', 'jams-starter.jar', '-m', paste0('data/J2K_cowat/',jams_file_name), '-n'),
    wait=F, stdout=stdoutP, stderr=stderrP
  )
  cat('\n\nWaiting 3 seconds to make sure J2K coupling module starts listening...')
  Sys.sleep(3)
  setwd(wd)
}
#####################################################
createReachOutputList = function(cormasReachIDs){
  
reach_Runoff_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_Runoff_dt <- reach_Runoff_dt[-1,]; names(reach_Runoff_dt)<- cormasReachIDs
reach_actRD1_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_actRD1_dt <- reach_actRD1_dt[-1,]; names(reach_actRD1_dt)<-cormasReachIDs
reach_actRD2_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_actRD2_dt <- reach_actRD2_dt[-1,]; names(reach_actRD2_dt)<-cormasReachIDs
reach_actRG1_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_actRG1_dt <- reach_actRG1_dt[-1,]; names(reach_actRG1_dt)<-cormasReachIDs
reach_actRG2_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_actRG2_dt <- reach_actRG2_dt[-1,]; names(reach_actRG2_dt)<-cormasReachIDs
reach_inRD1_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_inRD1_dt <- reach_inRD1_dt[-1,]; names(reach_inRD1_dt)<-cormasReachIDs
reach_inRD2_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_inRD2_dt <- reach_inRD2_dt[-1,]; names(reach_inRD2_dt)<-cormasReachIDs
reach_inRG1_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_inRG1_dt <- reach_inRG1_dt[-1,]; names(reach_inRG1_dt)<-cormasReachIDs
reach_inRG2_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_inRG2_dt <- reach_inRG2_dt[-1,]; names(reach_inRG2_dt)<-cormasReachIDs
reach_outRD1_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_outRD1_dt <- reach_outRD1_dt[-1,]; names(reach_outRD1_dt)<-cormasReachIDs
reach_outRD2_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_outRD2_dt <- reach_outRD2_dt[-1,]; names(reach_outRD2_dt)<-cormasReachIDs
reach_outRG1_dt <- as.data.frame(matrix(NA, ncol = length(cormasReachIDs))); reach_outRG1_dt <- reach_outRG1_dt[-1,]; names(reach_outRG1_dt)<-cormasReachIDs

reachOutputList = list (reach_Runoff_dt, reach_actRD1_dt, reach_actRD2_dt, reach_actRG1_dt, reach_actRG2_dt, reach_inRD1_dt, reach_inRD2_dt, reach_inRG1_dt, reach_inRG2_dt, reach_outRD1_dt, reach_outRD2_dt, reach_outRG1_dt)
return(reachOutputList)
}
#####################################################
createHRUOutputList = function(cormasParcelIds){

hru_actLAI_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_actLAI_dt <- hru_actLAI_dt[-1,]; names(hru_actLAI_dt)<-cormasParcelIds
hru_CropCoeff_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_CropCoeff_dt <- hru_CropCoeff_dt[-1,]; names(hru_CropCoeff_dt)<-cormasParcelIds
hru_netRain_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_netRain_dt <- hru_netRain_dt[-1,]; names(hru_netRain_dt)<-cormasParcelIds
hru_netSnow_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_netSnow_dt <- hru_netSnow_dt[-1,]; names(hru_netSnow_dt)<-cormasParcelIds
hru_potET_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_potET_dt <- hru_potET_dt[-1,]; names(hru_potET_dt)<-cormasParcelIds
hru_actET_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_actET_dt <- hru_actET_dt[-1,]; names(hru_actET_dt)<-cormasParcelIds
hru_actMPS_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_actMPS_dt <- hru_actMPS_dt[-1,]; names(hru_actMPS_dt)<-cormasParcelIds
hru_actLPS_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_actLPS_dt <- hru_actLPS_dt[-1,]; names(hru_actLPS_dt)<-cormasParcelIds
hru_actDPS_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_actDPS_dt <- hru_actDPS_dt[-1,]; names(hru_actDPS_dt)<-cormasParcelIds
hru_satMPS_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_satMPS_dt <- hru_satMPS_dt[-1,]; names(hru_satMPS_dt)<-cormasParcelIds
hru_satLPS_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_satLPS_dt <- hru_satLPS_dt[-1,]; names(hru_satLPS_dt)<-cormasParcelIds
hru_satSoil_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_satSoil_dt <- hru_satSoil_dt[-1,]; names(hru_satSoil_dt)<-cormasParcelIds
hru_percolation_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_percolation_dt <- hru_percolation_dt[-1,]; names(hru_percolation_dt)<-cormasParcelIds
hru_inRD1_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_inRD1_dt <- hru_inRD1_dt[-1,]; names(hru_inRD1_dt)<-cormasParcelIds
hru_inRD2_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_inRD2_dt <- hru_inRD2_dt[-1,]; names(hru_inRD2_dt)<-cormasParcelIds
hru_outRD1_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_outRD1_dt <- hru_outRD1_dt[-1,]; names(hru_outRD1_dt)<-cormasParcelIds
hru_outRD2_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_outRD2_dt <- hru_outRD2_dt[-1,]; names(hru_outRD2_dt)<-cormasParcelIds
hru_actRG1_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_actRG1_dt <- hru_actRG1_dt[-1,]; names(hru_actRG1_dt)<-cormasParcelIds
hru_actRG2_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_actRG2_dt <- hru_actRG2_dt[-1,]; names(hru_actRG2_dt)<-cormasParcelIds
hru_outRG1_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_outRG1_dt <- hru_outRG1_dt[-1,]; names(hru_outRG1_dt)<-cormasParcelIds
hru_outRG2_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_outRG2_dt <- hru_outRG2_dt[-1,]; names(hru_outRG2_dt)<-cormasParcelIds
hru_irrigationTotal_dt <- as.data.frame(matrix(NA, ncol = length(cormasParcelIds))); hru_irrigationTotal_dt <- hru_irrigationTotal_dt[-1,]; names(hru_irrigationTotal_dt)<-cormasParcelIds

hruOutputList = list (hru_actLAI_dt, hru_CropCoeff_dt, hru_netRain_dt, hru_netSnow_dt, hru_potET_dt, hru_actET_dt, hru_actMPS_dt, hru_actLPS_dt, hru_actDPS_dt, hru_satMPS_dt, hru_satLPS_dt, hru_percolation_dt, hru_inRD1_dt, hru_inRD2_dt, hru_outRD1_dt, hru_outRD2_dt, hru_actRG1_dt, hru_actRG2_dt, hru_outRG1_dt, hru_outRG2_dt, hru_irrigationTotal_dt)
return(hruOutputList)
}
#####################################################
recoverReachOutputs = function(reachOutputList) {
  
reach_Runoff = j2kGetOneValueAllReachs("Runoff")
reach_Runoff_i <- as.vector(as.numeric(reach_Runoff[,2]))
# reach_Runoff_dt <- rbind(reach_Runoff_dt,reach_Runoff_i)
reachOutputList[[1]]<- rbind(reachOutputList[[1]],reach_Runoff_i)

reach_actRD1 = j2kGetOneValueAllReachs("actRD1")
reach_actRD1_i <- as.vector(as.numeric(reach_actRD1[,2]))
# reach_acrRD1_dt <- rbind(reach_actRD1_dt,reach_actRD1_i)
reachOutputList[[2]]<- rbind(reachOutputList[[2]],reach_actRD1_i)

reach_actRD2 = j2kGetOneValueAllReachs("actRD2")
reach_actRD2_i <- as.vector(as.numeric(reach_actRD2[,2]))
# reach_actRD2_dt <- rbind(reach_actRD2_dt,reach_actRD2_i)
reachOutputList[[3]]<- rbind(reachOutputList[[3]],reach_actRD2_i)

reach_actRG1 = j2kGetOneValueAllReachs("actRG1")
reach_actRG1_i <- as.vector(as.numeric(reach_actRG1[,2]))
# reach_actRG1_dt <- rbind(reach_actRG1_dt,reach_actRG1_i)
reachOutputList[[4]]<- rbind(reachOutputList[[4]],reach_actRG1_i)

reach_actRG2 = j2kGetOneValueAllReachs("actRG2")
reach_actRG2_i <- as.vector(as.numeric(reach_actRG2[,2]))
# reach_actRG2_dt <- rbind(reach_actRG2_dt,reach_actRG2_i)
reachOutputList[[5]]<- rbind(reachOutputList[[5]],reach_actRG2_i)

reach_inRD1 = j2kGetOneValueAllReachs("inRD1")
reach_inRD1_i <- as.vector(as.numeric(reach_inRD1[,2]))
# reach_inRD1_dt <- rbind(reach_inRD1_dt,reach_inRD1_i)
reachOutputList[[6]]<- rbind(reachOutputList[[6]],reach_inRD1_i)

reach_inRD2 = j2kGetOneValueAllReachs("inRD2")
reach_inRD2_i <- as.vector(as.numeric(reach_inRD2[,2]))
# reach_inRD2_dt <- rbind(reach_inRD2_dt,reach_inRD2_i)
reachOutputList[[7]]<- rbind(reachOutputList[[7]],reach_inRD2_i)

reach_inRG1 = j2kGetOneValueAllReachs("inRG1")
reach_inRG1_i <- as.vector(as.numeric(reach_inRG1[,2]))
# reach_inRG1_dt <- rbind(reach_inRG1_dt,reach_inRG1_i)
reachOutputList[[8]]<- rbind(reachOutputList[[8]],reach_inRG1_i)

reach_inRG2 = j2kGetOneValueAllReachs("inRG2")
reach_inRG2_i <- as.vector(as.numeric(reach_inRG2[,2]))
# reach_inRG2_dt <- rbind(reach_inRG2_dt,reach_inRG2_i)
reachOutputList[[9]]<- rbind(reachOutputList[[9]],reach_inRG2_i)

reach_outRD1 = j2kGetOneValueAllReachs("outRD1")
reach_outRD1_i <- as.vector(as.numeric(reach_outRD1[,2]))
# reach_outRD1_dt <- rbind(reach_outRD1_dt,reach_outRD1_i)
reachOutputList[[10]]<- rbind(reachOutputList[[10]],reach_outRD1_i)

reach_outRD2 = j2kGetOneValueAllReachs("outRD2")
reach_outRD2_i <- as.vector(as.numeric(reach_outRD2[,2]))
# reach_outRD2_dt <- rbind(reach_outRD2_dt,reach_outRD2_i)
reachOutputList[[11]]<- rbind(reachOutputList[[11]],reach_outRD2_i)

reach_outRG1 = j2kGetOneValueAllReachs("outRG1")
reach_outRG1_i <- as.vector(as.numeric(reach_outRG1[,2]))
# reach_outRG1_dt <- rbind(reach_outRG1_dt,reach_outRD2_i)
reachOutputList[[12]]<- rbind(reachOutputList[[12]],reach_outRG1_i)
 return(reachOutputList)
}

#####################################################
recoverHruOutputs = function(hruOutputList) {
  
  hru_actLAI = j2kGetOneValueAllHrus("actLAI")
  hru_actLAI_i <- as.vector(as.numeric(hru_actLAI[,2]))
  # hru_actLAI_dt <- rbind(hru_actLAI_dt,hru_actLAI_i)
  hruOutputList[[1]]<- rbind(hruOutputList[[1]], hru_actLAI_i)
  
  hru_CropCoeff = j2kGetOneValueAllHrus("actKC")
  hru_CropCoeff_i <- as.vector(as.numeric(hru_CropCoeff[,2]))
  # hru_CropCoeff_dt <- rbind(hru_CropCoeff_dt,hru_CropCoeff_i)
  hruOutputList[[2]]<- rbind(hruOutputList[[2]], hru_CropCoeff_i)
  
  hru_netRain = j2kGetOneValueAllHrus("netrain")
  hru_netRain_i <- as.vector(as.numeric(hru_netRain[,2]))
  # hru_netRain_dt <- rbind(hru_netRain_dt,hru_netRain_i)
  hruOutputList[[3]]<- rbind(hruOutputList[[3]], hru_netRain_i)

  hru_netSnow = j2kGetOneValueAllHrus("netsnow")
  hru_netSnow_i <- as.vector(as.numeric(hru_netSnow[,2]))
  # hru_netSnow_dt <- rbind(hru_netSnow_dt,hru_netSnow_i)
  hruOutputList[[4]]<- rbind(hruOutputList[[4]], hru_netSnow_i)
  
  hru_potET = j2kGetOneValueAllHrus("etpot")
  hru_potET_i <- as.vector(as.numeric(hru_potET[,2]))
  # hru_potET_dt <- rbind(hru_potET_dt,hru_potET_i)
  hruOutputList[[5]]<- rbind(hruOutputList[[5]], hru_netSnow_i)
  
  hru_actET = j2kGetOneValueAllHrus("etact")
  hru_actET_i <- as.vector(as.numeric(hru_actET[,2]))
  # hru_actET_dt <- rbind(hru_actET_dt,hru_actET_i)
  hruOutputList[[6]]<- rbind(hruOutputList[[6]], hru_actET_i)

  hru_actMPS = j2kGetOneValueAllHrus("actMPS")
  hru_actMPS_i <- as.vector(as.numeric(hru_actMPS[,2]))
  # hru_actMPS_dt <- rbind(hru_actMPS_dt,hru_actMPS_i)
  hruOutputList[[7]]<- rbind(hruOutputList[[7]], hru_actMPS_i)
  
  hru_actLPS = j2kGetOneValueAllHrus("actLPS")
  hru_actLPS_i <- as.vector(as.numeric(hru_actLPS[,2]))
  # hru_actLPS_dt <- rbind(hru_actLPS_dt,hru_actLPS_i)
  hruOutputList[[8]]<- rbind(hruOutputList[[8]], hru_actLPS_i)
  
  hru_actDPS = j2kGetOneValueAllHrus("actDPS")
  hru_actDPS_i <- as.vector(as.numeric(hru_actDPS[,2]))
  # hru_actDPS_dt <- rbind(hru_actDPS_dt,hru_actDPS_i)
  hruOutputList[[9]]<- rbind(hruOutputList[[9]], hru_actDPS_i)

  hru_satMPS = j2kGetOneValueAllHrus("satMPS")
  hru_satMPS_i <- as.vector(as.numeric(hru_satMPS[,2]))
  # hru_satMPS_dt <- rbind(hru_satMPS_dt,hru_satMPS_i)
  hruOutputList[[10]]<- rbind(hruOutputList[[10]], hru_satMPS_i)
  
  hru_satLPS = j2kGetOneValueAllHrus("satLPS")
  hru_satLPS_i <- as.vector(as.numeric(hru_satLPS[,2]))
  # hru_satLPS_dt <- rbind(hru_satLPS_dt,hru_satLPS_i)
  hruOutputList[[11]]<- rbind(hruOutputList[[11]], hru_satLPS_i)

  hru_satSoil = j2kGetOneValueAllHrus("satSoil")
  hru_satSoil_i <- as.vector(as.numeric(hru_satSoil[,2]))
  # hru_satSoil_dt <- rbind(hru_satSoil_dt,hru_satSoil_i)
  hruOutputList[[12]]<- rbind(hruOutputList[[12]], hru_satSoil_i)

  hru_percolation = j2kGetOneValueAllHrus("Percolation")
  hru_percolation_i <- as.vector(as.numeric(hru_percolation[,2]))
  # hru_percolation_dt <- rbind(hru_percolation_dt,hru_percolation_i)
  hruOutputList[[13]]<- rbind(hruOutputList[[13]], hru_percolation_i)

  hru_inRD1 = j2kGetOneValueAllHrus("RD1")
  hru_inRD1_i <- as.vector(as.numeric(hru_inRD1[,2]))
  # hru_inRD1_dt <- rbind(hru_inRD1_dt,hru_inRD1_i)
  hruOutputList[[14]]<- rbind(hruOutputList[[14]], hru_inRD1_i)
  
  hru_inRD2 = j2kGetOneValueAllHrus("RD2")
  hru_inRD2_i <- as.vector(as.numeric(hru_inRD2[,2]))
  # hru_inRD2_dt <- rbind(hru_inRD2_dt,hru_inRD2_i)
  hruOutputList[[15]]<- rbind(hruOutputList[[15]], hru_inRD2_i)

  hru_outRD1 = j2kGetOneValueAllHrus("RD1OUT")
  hru_outRD1_i <- as.vector(as.numeric(hru_outRD1[,2]))
  # hru_outRD1_dt <- rbind(hru_outRD1_dt,hru_outRD1_i)
  hruOutputList[[16]]<- rbind(hruOutputList[[16]], hru_outRD1_i)
  
  hru_outRD2 = j2kGetOneValueAllHrus("RD2OUT")
  hru_outRD2_i <- as.vector(as.numeric(hru_outRD2[,2]))
  # hru_outRD2_dt <- rbind(hru_outRD2_dt,hru_outRD2_i)
  hruOutputList[[17]]<- rbind(hruOutputList[[17]], hru_outRD2_i)

  hru_actRG1 = j2kGetOneValueAllHrus("actRG1")
  hru_actRG1_i <- as.vector(as.numeric(hru_actRG1[,2]))
  # hru_actRG1_dt <- rbind(hru_actRG1_dt,hru_actRG1_i)
  hruOutputList[[18]]<- rbind(hruOutputList[[18]], hru_actRG1_i)

  hru_actRG2 = j2kGetOneValueAllHrus("actRG2")
  hru_actRG2_i <- as.vector(as.numeric(hru_actRG2[,2]))
  # hru_actRG2_dt <- rbind(hru_actRG2_dt,hru_actRG2_i)
  hruOutputList[[19]]<- rbind(hruOutputList[[19]], hru_actRG2_i)

  hru_outRG1 = j2kGetOneValueAllHrus("RG1OUT")
  hru_outRG1_i <- as.vector(as.numeric(hru_outRG1[,2]))
  # hru_outRG1_dt <- rbind(hru_outRG1_dt,hru_outRG1_i)
  hruOutputList[[20]]<- rbind(hruOutputList[[20]], hru_outRG1_i)

  hru_outRG2 = j2kGetOneValueAllHrus("RG2OUT")
  hru_outRG2_i <- as.vector(as.numeric(hru_outRG2[,2]))
  # hru_outRG2_dt <- rbind(hru_outRG2_dt,hru_outRG2_i)
  hruOutputList[[21]]<- rbind(hruOutputList[[21]], hru_outRG2_i)

  hru_irrigationTotal = j2kGetOneValueAllHrus("irrigationTotal")
  hru_irrigationTotal_i <- as.vector(as.numeric(hru_irrigationTotal[,2]))
  # hru_irrigationTotal_dt <- rbind(hru_irrigationTotal_dt,hru_irrigationTotal_i)
  hruOutputList[[22]]<- rbind(hruOutputList[[22]], hru_irrigationTotal_i)

  return(hruOutputList)
}
#####################################################
writeOutputs = function(outputs.dir, case_study_name, reachOutputList, hruOutputList) {

dir.create(outputs.dir)
dir.create(paste0(outputs.dir,case_study_name))

reachfiles.names <- c("reach_Runoff_dt.csv","reach_actRD1_dt.csv","reach_actRD2_dt.csv","reach_actRG1_dt.csv","reach_actRG2_dt.csv","reach_inRD1_dt.csv","reach_inRD2_dt.csv","reach_inRG1_dt.csv","reach_inRG2_dt.csv","reach_outRD1_dt.csv","reach_outRD2_dt.csv","reach_outRG1_dt.csv")
for (i in 1:length(reachfiles.names)){
write.csv(reachOutputList[[i]], paste0(outputs.dir,case_study_name,"/",reachfiles.names[i]), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
}

hrufiles.names <- c("hru_actLAI_dt.csv","hru_CropCoeff_dt.csv","hru_netRain_dt.csv","hru_netSnow_dt.csv","hru_potET_dt.csv","hru_actET_dt.csv","hru_actMPS_dt.csv","hru_actLPS_dt.csv","hru_actDPS_dt.csv","hru_satMPS_dt.csv","hru_satLPS_dt.csv","hru_satSoil_dt.csv","hru_percolation_dt.csv","hru_inRD1_dt.csv","hru_inRD2_dt.csv","hru_outRD1_dt.csv","hru_outRD2_dt.csv","hru_actRG1_dt.csv","hru_actRG2_dt.csv","hru_outRG1_dt.csv","hru_outRG2_dt.csv","hru_irrigationTotal_dt.csv")

for (k in 1:length(hrufiles.names)){
  write.csv(hruOutputList[[k]], paste0(outputs.dir,case_study_name,"/",hrufiles.names[k]), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
}
}