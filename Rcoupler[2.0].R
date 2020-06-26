#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R coupler 2.0      ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script runs J2K-WatASit-Optirrig coupled simulations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2020, Jan-June, by
# J. Veyssier -> make superjams_6, socket methods and Rcoupler v1
# B. Bonté -> make RCormas methods and Rcoupler v1
# B. Richard -> make param and climate Rfunctions and Rcoupler v2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list=ls()); start_time <- Sys.time();
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1. R Settings #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1.1 Set directory for coupling #######
# Not necessary if you open watasit_rcoupler.Rproject
wd <- getwd()
initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.dirname <- dirname(script.name)
if (paste0(script.dirname, "runInConsole") == "runInConsole")
  script.dirname <- wd

####### 1.2 Load functions #######
wd_Functions <- file.path(script.dirname, "Rfunctions/")
for(FileName in list.files(wd_Functions, pattern="\\.[Rr]$")){ source(file.path(wd_Functions, FileName)); }

####### 1.3 Load libraries #######
load <- c(require(ConfigParser), require(R.utils), require(RSQLite), require(feather), require(zoo), require (multiplex), require(tidyr), require(ggplot2), require(dplyr), require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

####### 1.4 Core parallelism #######
cores <- parallel:::detectCores(); registerDoParallel(cores-2);

####### 1.5 Read config file #######
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Simulation Settings and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### 2.0 Specification of case study name and simulation dates [COMPULSORY] #######
case_study_name <- "Aspres_cowat_10_ok2_j2k_only_irriModDesactivatedNotSoil_allvar_1989-2013"

date_start_hydro <- as.Date("1989-01-01", "%Y-%m-%d") # Attention la date de début de simulation de j2k doit être la mêne que dans le .jam (date modifiée dans juice!)
date_start_crop <- as.Date("2016-10-15", "%Y-%m-%d"); doy_start_crop <- as.numeric(difftime(date_start_crop,date_start_crop,units='days'))
date_start_irri <- as.Date("2017-05-01", "%Y-%m-%d"); doy_start_irri <- as.numeric(difftime(date_start_irri,date_start_crop,units='days'))
date_end_irri <- as.Date("1991-12-31", "%Y-%m-%d"); doy_end_irri <- as.numeric(difftime(date_end_irri,date_start_crop,units='days'))

####### 2.1 Importation of meteo data input for Optirrig and WatASit [COMPULSORY] #######
climate_file_name <- 'climate_buech_2016-2017.csv'
input_meteo <- computeClimateInput(climate_file_name, date_start_crop, date_end_irri)
str(input_meteo)

####### 2.2 Specification for J2K/JAMS #######
hydro_warmup_doy_nb <- as.numeric(difftime(date_start_crop, date_start_hydro,units='days')-1)
jams_file_name <- "cowat.jam"
makeWaterBalance <- T; if (makeWaterBalance) { storedWater <- NULL; inOutWater <-NULL}

####### 2.3 Specification for WatASit/Cormas coupling [COMPULSORY] #######
with_cormas <- F # choose True (T) or False (F)
if (with_cormas) {
modelName = "COWAT"
parcelFile = "WatASit[2.0_TT].pcl"
init = "INIT_2017_54x44"
cormas_doy_nb <- as.numeric(difftime(date_end_irri,date_start_irri,units='days'))
scenario <- "Baseline" #Choose Baseline ("simultaneous" scenario) or Alternative ("daily slots" scenario)
}

####### 2.4 Specification for Optirrig coupling [COMPULSORY] #######
with_optirrig <- F
if (with_optirrig) { 
itest <- 0    # Choose itest = 0 (fixed irrigation dates) or itest=1 (optimized irrigation dates)
  if (with_cormas) {itest = 0; gge = 0; dosap =0; th_w = 0} #compulsory for collective irrigation in WatASit 
  else { if (itest == 0) {th_w = 0; gge = 0; dosap = 0}
         if (itest == 1) {th_w = 100; gge = 0; dosap = 43.2} } #Fill in irrigation dose dosap (in mm) and choose surface (gge = 0) or drip (gge = 1) irigation technology

list_idParcel <- NULL; # Generation of an Optirrig paramfile for each plot
list_idParcel <- optiParams(dir = 'paramfiles/', 
                            case_study_name = case_study_name, 
                            shapefile_name = 'watasit_IrrigatedCerealsOKp3.csv', 
                            paramDBfile_name = 'paramDBCereals.csv', 
                            climatefile_name = climate_file_name, jdsim = 1, 
                            jfsim = dim(input_meteo)[1], 
                            itest = itest, 
                            gge = gge, 
                            dosap = dosap, 
                            th_w = th_w); warnings()

I1 = matrix(0, nrow = dim(input_meteo)[1], ncol = length(list_idParcel)) # Initialization of surface irrigation vector
I2 = matrix(0, nrow = dim(input_meteo)[1], ncol = length(list_idParcel)) # Initialization of deep irrigation vector
 }

####### 2.5 Activate results saving #######
saveRes <- F #if False -> don't save results if True -> save results


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. WatASit, Optirrig and J2K initialisation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3.1 Connexion and opening of WatASit model #######
# Open Cormas: dans le répertoire de cormas taper: "wine cormas.exe"
if (with_cormas){
cormasInVW7dir = cormasRootPath
setwd(cormasInVW7dir)
if (!isCormasListening()) {
  # Open Cormas listenning for instruction
  system2(
    'wine',
      args=c('../bin/win/visual.exe', 'cormas.im' ,'-doit', '"CormasNS.Kernel.Cormas current startWSForR"'),
    # adding headless successfully launches cormas and the model loading appears to be working
    # but at some point Rcoupler crashes
    #args=c('../bin/win/visual.exe', 'cormas.im', '-headless' ,'-doit', '"CormasNS.Kernel.Cormas current startWSForR"'),
    wait=F, stdout=stdoutP, stderr=stderrP
  )
  cat('\n\nWaiting 3 seconds to make sure cormas starts listening...')
  Sys.sleep(3)
}
setwd(wd)

# Ça ouvre une image de cormas avec le modèle chargé mais ne pas regarder
# Dans l'interface principale, aller dans le menu: "simulation/Analysis/cormas<-->R/Sart webservie for R".
# Un petit logo R avec un point vert doit apparaitre.. Le tour est joué.
r <- openModel(modelName, parcelFile = parcelFile)

####### 3.2 Choose of WatASit initial state and time step function (scenarios) #######
# Note that the model is not initialized, we just set the init method..
r <- setInit(init) # Init
r <- setStep(paste0("R_go",scenario,"Step:")) # Stepper

####### 3.4 Initialize Cormas model #######
r <- initSimu() # initialize the model
} # end of with_cormas loop

####### 3.5 Initialize Optirrig model #######
if (with_optirrig) {
  param_frame <- data.frame(); irr <- data.frame()
  for (i in 1:length(list_idParcel)){
    #Load params of each plot #######
    param = read.csv(paste0('paramfiles/paramfiles_',case_study_name,'/',list_idParcel[i],'/parF', list_idParcel[i],'.csv'), header = TRUE,sep=",",dec = ".",stringsAsFactor=FALSE)
    #Create frame with all parameters #######
    param_frame <- rbind(param_frame, param)
  }
} # end of with_optirrig loop

####### 3.5 Initialize J2K model #######
# On laisse le coupleur lancer JAMS/J2K
# On peut aussi lancer J2K manuellement de la manière suivante.
# En étant dans le dossier "superjams" (qui vient de l'archive superjams.zip) :
# java -jar jams-starter.jar -m data/J2K_cowat/j2k_cowat_buech_ju_couplage.jam -n
# et hop ça lance juste le modèle, pas d'interface graphique, pas  d'éditeur de modèle. Pour l'arrêter : CTRL+C .
# S'il s'arrête tout seul au bout de 2 minutes d'inactivité : CTRL+C et on peut le relancer avec la même commande.
#  "Rfunctions/Rj2k.R".
# Pour lancer juice directement, en étant dans le dossier "superjams":
# java -jar juice-starter.jar dans un terminal
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Run simulations #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat('\nRunning simulation!!!\n')
####### 4.1 Create results dataFrame #######
reachID = as.numeric(j2kGet("reach")[,1])
reach_Runoff_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_Runoff_dt <- reach_Runoff_dt[-1,]
# reach_actRD1_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_actRD1_dt <- reach_actRD1_dt[-1,]
# reach_actRD2_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_actRD2_dt <- reach_actRD2_dt[-1,]
# reach_actRG1_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_actRG1_dt <- reach_actRG1_dt[-1,]
# reach_actRG2_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_actRG2_dt <- reach_actRG2_dt[-1,]
# reach_inRD1_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_inRD1_dt <- reach_inRD1_dt[-1,]
# reach_inRD2_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_inRD2_dt <- reach_inRD2_dt[-1,]
# reach_inRG1_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_inRG1_dt <- reach_inRG1_dt[-1,]
# reach_inRG2_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_inRG2_dt <- reach_inRG2_dt[-1,]
# reach_outRD1_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_outRD1_dt <- reach_outRD1_dt[-1,]
# reach_outRD2_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_outRD2_dt <- reach_outRD2_dt[-1,]
# reach_outRG1_dt <- as.data.frame(matrix(NA, ncol = length(reachID))); reach_outRG1_dt <- reach_outRG1_dt[-1,]
# hruID = as.numeric(j2kGet("hru")[,1])
# hru_actLAI_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_actLAI_dt <- hru_actLAI_dt[-1,]
# hru_CropCoeff_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_CropCoeff_dt <- hru_CropCoeff_dt[-1,]
# hru_netRain_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_netRain_dt <- hru_netRain_dt[-1,]
# hru_netSnow_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_netSnow_dt <- hru_netSnow_dt[-1,]
# hru_potET_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_potET_dt <- hru_potET_dt[-1,]
# hru_actET_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_actET_dt <- hru_actET_dt[-1,]
# hru_actMPS_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_actMPS_dt <- hru_actMPS_dt[-1,]
# hru_actLPS_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_actLPS_dt <- hru_actLPS_dt[-1,]
# hru_actDPS_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_actDPS_dt <- hru_actDPS_dt[-1,]
# hru_satMPS_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_satMPS_dt <- hru_satMPS_dt[-1,]
# hru_satLPS_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_satLPS_dt <- hru_satLPS_dt[-1,]
# hru_satSoil_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_satSoil_dt <- hru_satSoil_dt[-1,]
# hru_percolation_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_percolation_dt <- hru_percolation_dt[-1,]
# hru_inRD1_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_inRD1_dt <- hru_inRD1_dt[-1,]
# hru_inRD2_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_inRD2_dt <- hru_inRD2_dt[-1,]
# hru_outRD1_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_outRD1_dt <- hru_outRD1_dt[-1,]
# hru_outRD2_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_outRD2_dt <- hru_outRD2_dt[-1,]
# hru_actRG1_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_actRG1_dt <- hru_actRG1_dt[-1,]
# hru_actRG2_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_actRG2_dt <- hru_actRG2_dt[-1,]
# hru_outRG1_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_outRG1_dt <- hru_outRG1_dt[-1,]
# hru_outRG2_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_outRG2_dt <- hru_outRG2_dt[-1,]
# hru_irrigationTotal_dt <- as.data.frame(matrix(NA, ncol = length(hruID))); hru_irrigationTotal_dt <- hru_irrigationTotal_dt[-1,]

####### 4.2 Run models
if (!any(c(with_cormas, with_optirrig))) {

  # Run j2k on the whole simulation period
  simuProgress <- txtProgressBar(min = 1,
                                 max = as.numeric(difftime(date_end_irri,date_start_hydro,units='days')),
                                 style = 3)
  for (i in 1:as.numeric(difftime(date_end_irri,date_start_hydro,units='days'))){ cat("\n","Running step:",i,"\n"); setTxtProgressBar(simuProgress, i)
  # Run one step
  if (makeWaterBalance) {storedWater <- rbind(storedWater, j2kWaterStorage())}
  j2kMakeStep()
  if (makeWaterBalance) {inOutWater <- rbind(inOutWater, j2kInOutWater())}
  
  # Get reach variables
  ##TO DO: function to get multiple reach or hru variables
  # reach_var = c("Runoff", "actRD1", "actRD2", "actRG1", "actRG2", "inRD1", "inRD2", "inRG1", "inRG2", "outRD1", "outRD2", "outRG1")
  # getReachVariables = function (reach_var = reach_var) {
  #   for (v in reach_var) {
  #     var = j2kGetOneValueAllReachs(paste0(reach_var[i]))
  #     var_i = as.vector(as.numeric(var[,2]))
  #     var_dt = rbind(var_dt, var_i)
  #   }
  # }
  # OR
  # j2kReachRunoff <- j2kGetOneValueAllReachs("Runoff") %>%
  #   as.data.frame() %>%
  #   mutate(Runoff = as.numeric(as.character(Runoff))) %>%
  #   mutate(ID = as.numeric(as.character(ID))) %>%
  #   tbl_df()
  
  ## Manual method
  reach_Runoff = j2kGetOneValueAllReachs("Runoff")
  reach_Runoff_i <- as.vector(as.numeric(reach_Runoff[,2]))
  reach_Runoff_dt <- rbind(reach_Runoff_dt,reach_Runoff_i)

  # reach_actRD1 = j2kGetOneValueAllReachs("actRD1")
  # reach_actRD1_i <- as.vector(as.numeric(reach_actRD1[,2]))
  # reach_acrRD1_dt <- rbind(reach_actRD1_dt,reach_actRD1_i)
  # 
  # reach_actRD2 = j2kGetOneValueAllReachs("actRD2")
  # reach_actRD2_i <- as.vector(as.numeric(reach_actRD2[,2]))
  # reach_actRD2_dt <- rbind(reach_actRD2_dt,reach_actRD2_i)
  # 
  # reach_actRG1 = j2kGetOneValueAllReachs("actRG1")
  # reach_actRG1_i <- as.vector(as.numeric(reach_actRG1[,2]))
  # reach_actRG1_dt <- rbind(reach_actRG1_dt,reach_actRG1_i)
  # 
  # reach_actRG2 = j2kGetOneValueAllReachs("actRG2")
  # reach_actRG2_i <- as.vector(as.numeric(reach_actRG2[,2]))
  # reach_actRG2_dt <- rbind(reach_actRG2_dt,reach_actRG2_i)
  # 
  # reach_inRD1 = j2kGetOneValueAllReachs("inRD1")
  # reach_inRD1_i <- as.vector(as.numeric(reach_inRD1[,2]))
  # reach_inRD1_dt <- rbind(reach_inRD1_dt,reach_inRD1_i)
  # 
  # reach_inRD2 = j2kGetOneValueAllReachs("inRD2")
  # reach_inRD2_i <- as.vector(as.numeric(reach_inRD2[,2]))
  # reach_inRD2_dt <- rbind(reach_inRD2_dt,reach_inRD2_i)
  # 
  # reach_inRG1 = j2kGetOneValueAllReachs("inRG1")
  # reach_inRG1_i <- as.vector(as.numeric(reach_inRG1[,2]))
  # reach_inRG1_dt <- rbind(reach_inRG1_dt,reach_inRG1_i)
  # 
  # reach_inRG2 = j2kGetOneValueAllReachs("inRG2")
  # reach_inRG2_i <- as.vector(as.numeric(reach_inRG2[,2]))
  # reach_inRG2_dt <- rbind(reach_inRG2_dt,reach_inRG2_i)
  # 
  # reach_outRD1 = j2kGetOneValueAllReachs("outRD1")
  # reach_outRD1_i <- as.vector(as.numeric(reach_outRD1[,2]))
  # reach_outRD1_dt <- rbind(reach_outRD1_dt,reach_outRD1_i)
  # 
  # reach_outRD2 = j2kGetOneValueAllReachs("outRD2")
  # reach_outRD2_i <- as.vector(as.numeric(reach_outRD2[,2]))
  # reach_outRD2_dt <- rbind(reach_outRD2_dt,reach_outRD2_i)
  # 
  # reach_outRG1 = j2kGetOneValueAllReachs("outRG1")
  # reach_outRG1_i <- as.vector(as.numeric(reach_outRG1[,2]))
  # reach_outRG1_dt <- rbind(reach_outRG1_dt,reach_outRD2_i)

  # Get HRU variables
  # hru_actLAI = j2kGetOneValueAllHrus("actLAI")
  # hru_actLAI_i <- as.vector(as.numeric(hru_actLAI[,2]))
  # hru_actLAI_dt <- rbind(hru_actLAI_dt,hru_actLAI_i)
  # 
  # hru_CropCoeff = j2kGetOneValueAllHrus("actKC")
  # hru_CropCoeff_i <- as.vector(as.numeric(hru_CropCoeff[,2]))
  # hru_CropCoeff_dt <- rbind(hru_CropCoeff_dt,hru_CropCoeff_i)
  
  # hru_netRain = j2kGetOneValueAllHrus("netrain")
  # hru_netRain_i <- as.vector(as.numeric(hru_netRain[,2]))
  # hru_netRain_dt <- rbind(hru_netRain_dt,hru_netRain_i)
  # 
  # hru_netSnow = j2kGetOneValueAllHrus("netsnow")
  # hru_netSnow_i <- as.vector(as.numeric(hru_netSnow[,2]))
  # hru_netSnow_dt <- rbind(hru_netSnow_dt,hru_netSnow_i)
  # 
  # hru_potET = j2kGetOneValueAllHrus("etpot")
  # hru_potET_i <- as.vector(as.numeric(hru_potET[,2]))
  # hru_potET_dt <- rbind(hru_potET_dt,hru_potET_i)
  # 
  # hru_actET = j2kGetOneValueAllHrus("etact")
  # hru_actET_i <- as.vector(as.numeric(hru_actET[,2]))
  # hru_actET_dt <- rbind(hru_actET_dt,hru_actET_i)
  # 
  # hru_actMPS = j2kGetOneValueAllHrus("actMPS")
  # hru_actMPS_i <- as.vector(as.numeric(hru_actMPS[,2]))
  # hru_actMPS_dt <- rbind(hru_actMPS_dt,hru_actMPS_i)
  # 
  # hru_actLPS = j2kGetOneValueAllHrus("actLPS")
  # hru_actLPS_i <- as.vector(as.numeric(hru_actLPS[,2]))
  # hru_actLPS_dt <- rbind(hru_actLPS_dt,hru_actLPS_i)
  # 
  # hru_actDPS = j2kGetOneValueAllHrus("actDPS")
  # hru_actDPS_i <- as.vector(as.numeric(hru_actDPS[,2]))
  # hru_actDPS_dt <- rbind(hru_actDPS_dt,hru_actDPS_i)
  # 
  # hru_satMPS = j2kGetOneValueAllHrus("satMPS")
  # hru_satMPS_i <- as.vector(as.numeric(hru_satMPS[,2]))
  # hru_satMPS_dt <- rbind(hru_satMPS_dt,hru_satMPS_i)
  # 
  # hru_satLPS = j2kGetOneValueAllHrus("satLPS")
  # hru_satLPS_i <- as.vector(as.numeric(hru_satLPS[,2]))
  # hru_satLPS_dt <- rbind(hru_satLPS_dt,hru_satLPS_i)
  # 
  # hru_satSoil = j2kGetOneValueAllHrus("satSoil")
  # hru_satSoil_i <- as.vector(as.numeric(hru_satSoil[,2]))
  # hru_satSoil_dt <- rbind(hru_satSoil_dt,hru_satSoil_i)
  # 
  # hru_percolation = j2kGetOneValueAllHrus("Percolation")
  # hru_percolation_i <- as.vector(as.numeric(hru_percolation[,2]))
  # hru_percolation_dt <- rbind(hru_percolation_dt,hru_percolation_i)
  # 
  # hru_inRD1 = j2kGetOneValueAllHrus("RD1")
  # hru_inRD1_i <- as.vector(as.numeric(hru_inRD1[,2]))
  # hru_inRD1_dt <- rbind(hru_inRD1_dt,hru_inRD1_i)
  # 
  # hru_inRD2 = j2kGetOneValueAllHrus("RD2")
  # hru_inRD2_i <- as.vector(as.numeric(hru_inRD2[,2]))
  # hru_inRD2_dt <- rbind(hru_inRD2_dt,hru_inRD2_i)
  # 
  # hru_outRD1 = j2kGetOneValueAllHrus("RD1OUT")
  # hru_outRD1_i <- as.vector(as.numeric(hru_outRD1[,2]))
  # hru_outRD1_dt <- rbind(hru_outRD1_dt,hru_outRD1_i)
  # 
  # hru_outRD2 = j2kGetOneValueAllHrus("RD2OUT")
  # hru_outRD2_i <- as.vector(as.numeric(hru_outRD2[,2]))
  # hru_outRD2_dt <- rbind(hru_outRD2_dt,hru_outRD2_i)
  # 
  # hru_actRG1 = j2kGetOneValueAllHrus("actRG1")
  # hru_actRG1_i <- as.vector(as.numeric(hru_actRG1[,2]))
  # hru_actRG1_dt <- rbind(hru_actRG1_dt,hru_actRG1_i)
  # 
  # hru_actRG2 = j2kGetOneValueAllHrus("actRG2")
  # hru_actRG2_i <- as.vector(as.numeric(hru_actRG2[,2]))
  # hru_actRG2_dt <- rbind(hru_actRG2_dt,hru_actRG2_i)
  # 
  # hru_outRG1 = j2kGetOneValueAllHrus("RG1OUT")
  # hru_outRG1_i <- as.vector(as.numeric(hru_outRG1[,2]))
  # hru_outRG1_dt <- rbind(hru_outRG1_dt,hru_outRG1_i)
  # 
  # hru_outRG2 = j2kGetOneValueAllHrus("RG2OUT")
  # hru_outRG2_i <- as.vector(as.numeric(hru_outRG2[,2]))
  # hru_outRG2_dt <- rbind(hru_outRG2_dt,hru_outRG2_i)
  # 
  # hru_irrigationTotal = j2kGetOneValueAllHrus("irrigationTotal")
  # hru_irrigationTotal_i <- as.vector(as.numeric(hru_irrigationTotal[,2]))
  # hru_irrigationTotal_dt <- rbind(hru_irrigationTotal_dt,hru_irrigationTotal_i)
  } # end of time loop
  names(reach_Runoff_dt)<-reachID #; names(reach_actRD1_dt)<-reachID; names(reach_actRD2_dt)<-reachID; names(reach_actRG1_dt)<-reachID; names(reach_actRG2_dt)<-reachID;
  # names(reach_inRD1_dt)<-reachID; names(reach_inRD2_dt)<-reachID; names(reach_inRG1_dt)<-reachID;names(reach_inRG2_dt)<-reachID;names(reach_outRD1_dt)<-reachID;names(reach_outRD2_dt)<-reachID;
  # names(reach_outRG1_dt)<-reachID
  # names(hru_actLAI_dt)<-hruID; names(hru_CropCoeff_dt)<-hruID; 
  # names(hru_netRain_dt)<-hruID; names(hru_netSnow_dt)<-hruID; names(hru_potET_dt)<-hruID;
  # names(hru_actET_dt)<-hruID; names(hru_actMPS_dt)<-hruID; names(hru_actLPS_dt)<-hruID; names(hru_actDPS_dt)<-hruID;
  # names(hru_satMPS_dt)<-hruID; names(hru_satLPS_dt)<-hruID; names(hru_satSoil_dt)<-hruID;
  # names(hru_percolation_dt)<-hruID; names(hru_inRD1_dt)<-hruID;
  # names(hru_inRD2_dt)<-hruID; names(hru_outRD1_dt)<-hruID; names(hru_outRD2_dt)<-hruID; names(hru_actRG1_dt)<-hruID; names(hru_outRG1_dt)<-hruID; names(hru_outRG2_dt)<-hruID;
  # names(hru_irrigationTotal_dt)<-hruID;
}




###!!!Ci-dessous: WorkInProgress: case j2k and WatASit only
if (with_cormas == T && with_optirrig == F) { 
  simuProgress <- txtProgressBar(min = 1,
                                 max = as.numeric(difftime(date_end_irri,date_start_hydro,units='days')),
                                 style = 3)
  
    for (i in 1:as.numeric(difftime(date_end_irri,date_start_hydro,units='days'))){ 
    cat("\n","Running step:",i,"\n"); setTxtProgressBar(simuProgress, i)
        
        ####### A. Getting flow from j2k #######
        # Getting corespondance table between cormas ids and j2k idReach (ID dans les modules Rj2k)
        reach_Runoff = j2kGetOneValueAllReachs("Runoff")
        reach_Runoff_i <- as.vector(as.numeric(reach_Runoff[,2]))
        reach_Runoff_dt <- rbind(reach_Runoff_dt,reach_Runoff_i)
        
        ####### B. Updating flows in Cormas #######
        # cormasRiverReachs <- getAttributesOfEntities(attributeName = "idReach", "RiverReach")
        reachsToUpdate <- cormasRiverReachs %>%
         rename(cormasId = id,
                 ID = idReach) %>%
          inner_join(j2kReachRunoff, by = "ID") %>%
          mutate(q = ( Runoff / 1000 ) / (24 * 3600) ) # conversion en m3.s
        setAttributesOfEntities("q", "RiverReach",
                                reachsToUpdate$cormasId,
                                reachsToUpdate$q)
        
        ####### B. Run WatASit during 24 hours during the irrigation campaign and get irrigtaion #######
        if (i >= date_start_irri){# activate coupling with WatASit in Cormas plateform
            irriDailyDose <-RunWatASit(daily_step = i, input_meteo = input_meteo)
        }
        
        ####### C. Set the irrigation in J2K #######
        j2kSet("surface", c(1,2,3), c(100, 100, 100)) # Mais en utilisant en fait les irriDailyDose ou truc du genre
        # récupérés ci-dessus depuis cormas
        
        ####### D. Run new j2k daily step #######
        if (makeWaterBalance) {storedWater <- rbind(storedWater, j2kWaterStorage())} # To calculate the water balance
        j2kMakeStep() # cette fonction fait un step si on lui donne pas de paramètre
        if (makeWaterBalance) {inOutWater <- rbind(inOutWater, j2kInOutWater())}
        # on peut aussi faire N steps comme ça
        #j2kMakeStep(20)
        # cette fonction est sensée récupérer les valeurs de tous les attributs pour tous les reachs
        # mais pour l'instant ça récupère juste actRD1
        #reachQTable = j2kGet("reach")
        # et celle là récupère juste netrain
        #hruQTable = j2kGet("hru")
        # ce sont ces fonctions qui récupèrent n'importe quel attribut des hrus ou des reachs
        #reachRD1DataFrame = j2kGetOneValueAllReachs('actRD1')
        #hruNetrainDataFrame = j2kGetOneValueAllHrus('netrain')
        
    } # End of time loop
  
names(reach_Runoff_dt)<-reachID
} # End of (with_cormas == T and with_optirrig == F) loop
  
  
###!!!Ci-dessous: TODO: case j2k and WatASit only  
if (with_cormas == F && with_optirrig == T) {}

###!!!Ci-dessous: TODO: case j2k and WatASit only 
if (with_cormas && with_optirrig) {
#J2K on previous years until the first #######
# cmdResult = j2kMakeStep(optirrig_doy_start - 1)
cmdResult = j2kMakeStep(365*57)
reachQTable = j2kGet("reach")

####### 5.3 Run Optirrig-WatASit-J2K simulations #######
if (with_optirrig) {
  cat('\nStarting coupled simulation simulation!!!\n')
  simuProgress <- txtProgressBar(min = cormas_doy_start,
                                 max = cormas_doy_start + cormas_sim_day_nb,
                                 style = 3)
  for (day in optirrig_doy_start:((cormas_doy_start + cormas_sim_day_nb))){ setTxtProgressBar(simuProgress, day)

        #  ####### 5.3.1 Initialize optirrig on day 1 #######
        if (day == optirrig_doy_start) {
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
              } #end of initialization parcel loop
        } #end of initialization if bracket
  }#end of with_optirrig l.236

#  ####### 5.3.2 Simulate #######
        if (day >= optirrig_doy_start){
          inval2_list <- list()
          vect2_list <- list()
          
              if (with_cormas) { if (day >= cormas_doy_start && day <= (cormas_doy_start + cormas_sim_day_nb)) { 
                ####### A. Update Cormas Meteo #######
                P<-input_meteo$P; setAttributesOfEntities("p", "Meteo", 1, as.numeric(P[day])) # Precipitation conditions of the day 
                p_forecast = sum(as.numeric(P[day:(day+2)]), na.rm = TRUE); if (p_forecast > 0) {p_forecast = 1}; setAttributesOfEntities("p_forecast", "Meteo", 1, p_forecast) # Precipitation forecast for the next 3 days
                if (day == 1) {p_cumTenDays = 0}
                if (day == 2) {p_cumTenDays = P[day-1]}
                for (i in 3:10){if (day == i){ p_cumTenDays = sum(as.numeric(P[(day-(i-1)):(day-1)]), na.rm = TRUE) }}
                if (day >= 11) {p_cumTenDays = sum(as.numeric(P[(day-10):(day-1)]), na.rm = TRUE)}
                setAttributesOfEntities("p_cumTenDays", "Meteo", 1, p_cumTenDays) # Calculate cumulative precipitation for the last 10 days
                p_cumTwelveDays = sum(c(p_cumTenDays,as.numeric(P[(day-12):(day-11)])), na.rm = TRUE)
                setAttributesOfEntities("p_cumTwelveDays", "Meteo", 1, p_cumTwelveDays) # Calculate cumulative precipitation for the last 12 days
                p_cumFifteenDays = sum(c(p_cumTwelveDays,as.numeric(P[(day-15):(day-13)])), na.rm = TRUE)
                setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, p_cumFifteenDays) # Calculate cumulative precipitation for the last 15 days
                
                ####### B. Updating river flow #######
                # Getting corespondance table between cormas ids and j2k idReach (ID dans les modules Rj2k)
                # cormasRiverReachs <- getAttributesOfEntities(attributeName = "idReach", "RiverReach")
                
                # TODO: supprimer la ligne suivante, qui est juste pour le test
                # je l'ai mise car on n'a pas l'identifiant de reach dans la version actuelle de watasit
                # (l'idReach de watasit n'existe pas dans le modèle j2k)
                # cormasRiverReachs <- cormasRiverReachs %>% mutate(idReach = 59200)
                
                # Getting flows from J2k
                #TODO: Vérifier que c'est bien la variable runoff qui donne le débit dans les reachs
                j2kReachRunoff <- j2kGetOneValueAllReachs("Runoff") %>%
                  as.data.frame() %>%
                  mutate(Runoff = as.numeric(as.character(Runoff))) %>%
                  mutate(ID = as.numeric(as.character(ID))) %>%
                  tbl_df()
                
                # Updating river flows in WatAsit, assuming that j2k runoff are in liter/days
                if (with_cormas){
                  # reachsToUpdate <- cormasRiverReachs %>%
                  #   rename(cormasId = id,
                  #          ID = idReach) %>%
                  #   inner_join(j2kReachRunoff, by = "ID") %>%
                  #   mutate(q = ( Runoff / 1000 ) / (24 * 3600) )
                  # 
                  # setAttributesOfEntities("q",
                  #                         "RiverReach",
                  #                         reachsToUpdate$cormasId,
                  #                         reachsToUpdate$q)
                
                ####### C. Run coupled simulation of 24 hours #######
                r <- runSimu(duration = 24)
                response <- gettext(r[[2]])
                if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {stop("RUN STOPPED",call.=FALSE)} # To check if runSimu is done
                
                ####### D. Get the farm plots irrigations by parcel ID #######
                fp_idExpl <- getAttributesOfEntities("idExpl","FarmPlot")
                fp_irri <- getAttributesOfEntities("irriDailyDose","FarmPlot")
                fp_irri_df <- data.frame(fp_irri, fp_idExpl); fp_irri_df$day = day;
                irriDailyDose <- fp_irri_df[,1:2]
                              } } # make coupling with Cormas
                
            if (with_optirrig) {
              for (i in 1:length(list_idParcel)){
                cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
                
                if (with_cormas) { if (day >= cormas_doy_start && day <= (cormas_doy_start + cormas_sim_day_nb)) { 
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
          
              ####### E. Set the new crop LAI in WatASit #######
              if (with_cormas) { if (day >= cormas_doy_start && day <= (cormas_doy_start + cormas_sim_day_nb)) {
                setAttributesOfEntities("tt", "Ecrop", as.numeric(list_idParcel), as.numeric(TT_mat[day,])) }}
            }#end of with_optirrig l.329
            
              ####### F. Set the irrigation in J2K #######
              j2kSet("drip", c(1,2,3), c(100, 100, 100)) # Mais en utilisant en fait les irriDailyDose ou truc du genre
              # récupérés ci-dessus depuis cormas
              #TODO La ligne précédente renvoie une erreur chez moi, c'est pour ça que je l'ai commenté..:
              # C'est bon ça roule maintenant
                
              # set actLai dans J2K
              j2kSet('actLaiCom', c(10363, 10362, 8934), c(100, 100, 100))
              print(' ')
              print('actLAI of some HRUs :')
              print(head(j2kGetOneValueAllHrus('actLAI'), 10))
                
              ####### 5.4.6 Simulate the new state of watershed with J2K #######
              # cette fonction fait un step si on lui donne pas de paramètre
              j2kMakeStep()
              # on peut aussi faire N steps comme ça
              #j2kMakeStep(20)
              # cette fonction est sensée récupérer les valeurs de tous les attributs pour tous les reachs
              # mais pour l'instant ça récupère juste actRD1
              #reachQTable = j2kGet("reach")
              # et celle là récupère juste netrain
              #hruQTable = j2kGet("hru")
              # ce sont ces fonctions qui récupèrent n'importe quel attribut des hrus ou des reachs
              #reachRD1DataFrame = j2kGetOneValueAllReachs('actRD1')
              #hruNetrainDataFrame = j2kGetOneValueAllHrus('netrain')
              dailySimQ = j2kGetOneValueAllReachs("Runoff")
              # SimQAtGauge <- simQ[which(as.numeric(simQ[,1])==62200)] #Le reach est manquant avec ces paramètres
              dailySimQAtGauge <- as.numeric(dailySimQ[which(as.numeric(dailySimQ[,1])==59200),2]) #Le reach est manquant avec ces paramètres
              simQ_mat[day] <- dailySimQAtGauge
              
          }#end of if bracket l.265
      }#end of timeloop l.237

}
}#end of with_cormas && with_optirrig conditions

####### 5.4 Run WatASit-Optirrig-J2K coupled simulation from cormas_doy_start #######
# for (day in cormas_doy_start:(cormas_doy_start + cormas_sim_day_nb)) {
  
  ####### 5.4.1 Update Cormas Meteo and flow in riverReachs#######
  # if (with_cormas){
  #   ## Updating meteo
  #   P <- input_meteo$P
  #   setAttributesOfEntities("p", "Meteo", 1, P[day]) # Precipitation conditions of the day
  #   p_forecast = sum(c(P[day:(day +2)]), na.rm = TRUE)
  #   if (p_forecast > 0) {
  #     p_forecast = 1
  #   }
  #   setAttributesOfEntities("p_forecast", "Meteo", 1, p_forecast) # Precipitation forecast for the next 3 days
  #   if (day >= 2) {
  #     p_cumTenDays = sum(c(P[max(1,(day-10)):(day-1)]), na.rm = TRUE)
  #     p_cumFifteenDays = sum(c(p_cumTenDays, P[max(1,(day-15)):max(2,(day-11))]), na.rm = TRUE)
  #   } else {
  #     p_cumTenDays = 0
  #     p_cumFifteenDays = 0
  #   }
  #   setAttributesOfEntities("p_cumTenDays", "Meteo", 1, p_cumTenDays) # Calculate cumulative precipitation for the last 10 days
  #   # setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, p_cumFifteenDays) # Calculate cumulative precipitation for the last 15 days
  #   setAttributesOfEntities("p_cumTwelveDays", "Meteo", 1, p_cumFifteenDays) # Calculate cumulative precipitation for the last 15 days
  # }

    
    

#   ####### 5.4.2 Run coupled simulation of 24 hours #######
#   r <- runSimu(duration = 24)
#  
#   obs1 <- NULL
#   obs2 <- NULL
#   #obs1 <- getAttributesOfEntities("floodAffCounter", "Efarmer")
#   #obs2 <- getAttributesOfEntities("floodActCounter", "Efarmer")
#   if (!((is.null(obs1) | is.null(obs2)))) {
#   obs <- left_join(obs1, obs2, by = "id")
#   obs$day = day
#   # farmers_results <- farmers_results %>%
#     bind_rows(obs)
#   }
# }
#   ####### 5.4.3 Get the state of crops from Cormas #######
#   #idParcel      <- getAttributesOfEntities("idParcel", "Ecrop")
#   #list_idParcel <- idParcel$idParcel
#   #harvestSignal <- getAttributesOfEntities("harvestSignal", "Ecrop")
#   #irriDailyDose <- getAttributesOfEntities("irriDailyDose", "Ecrop")

  ####### 5.4.4 Simulate the new state of crops with Optirrig #######
  # if (with_optirrig) {
  #   if (day != 1) {
  #     inval2_list <- list()
  #     vect2_list <- list()
  #     for (i in 1:length(list_idParcel)) {
  #       cat("Simulation of day", day, "and parcel number", i, "(idParcel =", list_idParcel[i], ")", "\n")
  #       irr[i,day]  <- irriDailyDose$irriDailyDose[i]
  #       irr <- as.matrix(irr)
  #       I1 = irr[i,] # Update irrigation from Cormas
  #       param <- param_frame[i,]
  #       cstes <- cstes_list[i,]
  #       inval <- inval_list[i,]
  #       vect <- vect_list[i,]
  #       optirday = daily_optirr(
  #         param,
  #         meteo,
  #         cstes,
  #         inval,
  #         vect,
  #         I1, # Surface irrigation
  #         I2, # Deep irrigation (buried drip)
  #         day # Time step
  #       )
  #       inval2 = optirday$inval
  #       inval2_list <- rbind(inval2_list, inval2)
  #       inval_list[i,] <- inval2 # News constants
  #       vect2  = optirday$vect
  #       vect2_list <- rbind(vect2_list, vect2)
  #       vect_list[i,] <- vect2 # New vectors
  #     }
  #   }
  # }
  # ####### 5.4.5 Set the irrigation in J2K #######
  # j2kSet("drip", c(1,2,3), c(100, 100, 100)) # Mais en utilisant en fait les irriDailyDose ou truc du genre
  #                                             # récupérés ci-dessus depuis cormas
  # #TODO La ligne précédente renvoie une erreur chez moi, c'est pour ça que je l'ai commenté..:
  # # C'est bon ça roule maintenant
  # 
  # # set actLai dans J2K
  # j2kSet('actLaiCom', c(10363, 10362, 8934), c(100, 100, 100))
  # print(' ')
  # print('actLAI of some HRUs :')
  # print(head(j2kGetOneValueAllHrus('actLAI'), 10))
  # 
  # ####### 5.4.6 Simulate the new state of watershed with J2K #######
  # # cette fonction fait un step si on lui donne pas de paramètre
  # j2kMakeStep()
  # # on peut aussi faire N steps comme ça
  # #j2kMakeStep(20)
  # # cette fonction est sensée récupérer les valeurs de tous les attributs pour tous les reachs
  # # mais pour l'instant ça récupère juste actRD1
  # #reachQTable = j2kGet("reach")
  # # et celle là récupère juste netrain
  # #hruQTable = j2kGet("hru")
  # # ce sont ces fonctions qui récupèrent n'importe quel attribut des hrus ou des reachs
  # #reachRD1DataFrame = j2kGetOneValueAllReachs('actRD1')
  # #hruNetrainDataFrame = j2kGetOneValueAllHrus('netrain')
  # dailySimQ = j2kGetOneValueAllReachs("Runoff")
  # # SimQAtGauge <- simQ[which(as.numeric(simQ[,1])==62200)] #Le reach est manquant avec ces paramètres
  # dailySimQAtGauge <- as.numeric(dailySimQ[which(as.numeric(dailySimQ[,1])==59200),2]) #Le reach est manquant avec ces paramètres
  # simQ_mat[day] <- dailySimQAtGauge
# }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 6. Make water balance [Optionnal] #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (makeWaterBalance){
  storages <- storedWater %>% tbl_df()
  inOut <- inOutWater %>% tbl_df()
  waterSummary <- cbind (storages, inOut) %>% tbl_df() %>% mutate(day = row_number())
  waterSummary <- waterSummary %>% mutate(storage = mps + lps + dps + storedSnow + intercStorage + 
                                            rg1 + rg2 +
                                            reachRD1 + reachRD2 + reachRG1 + reachRG2) %>%
    mutate(inWater = rain + snow) %>%
    mutate(outFlow = outRunoff) %>%
    mutate(outET = eTR) %>%
    mutate(balance = inWater - outFlow - outET) %>%
    mutate(storageNextDay = lead(storage)) %>%
    mutate(massConservation = storageNextDay - (storage + balance)) %>%
    mutate(deltaStock = storageNextDay - storage)
  write.csv(waterSummary, "newWaterSummary.csv", row.names = F)
  plotBalance(storedWater,inOutWater, "waterBalance")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 7. Save simulation results #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TODO: a function to export savings.
if (saveRes) {
  dir.create("save/simulations_cowat/"); dir.create(paste0("save/simulations_cowat/",case_study_name))
  write.csv(reach_Runoff_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_Runoff_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_actRD1_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_actRD1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_actRD2_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_actRD2_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_actRG1_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_actRG1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_actRG2_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_actRG2_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_inRD1_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_inRD1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_inRD2_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_inRD2_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_inRG1_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_inRG1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_inRG2_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_inRG2_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_outRD1_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_outRD1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_outRD2_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_outRD2_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(reach_outRG1_dt, paste0("save/simulations_cowat/",case_study_name,"/","reach_outRG1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_actLAI_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_actLAI_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_CropCoeff_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_CropCoeff_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_netRain_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_netRain_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_netSnow_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_netSnow_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_potET_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_potET_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_actET_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_actET_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_actMPS_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_actMPS_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_actLPS_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_actLPS_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_actDPS_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_actDPS_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_satMPS_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_satMPS_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_satLPS_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_satLPS_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_satSoil_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_satSoil_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_percolation_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_percolation_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_inRD1_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_inRD1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_inRD2_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_inRD2_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_outRD1_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_outRD1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_outRD2_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_outRD2_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_actRG1_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_actRG1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_actRG2_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_actRG2_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_outRG1_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_outRG1_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_outRG2_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_outRG2_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  # write.csv(hru_irrigationTotal_dt, paste0("save/simulations_cowat/",case_study_name,"/","hru_irrigationTotal_dt.csv"), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
}
cat('\n')
j2kStop()
Sys.sleep(3)
killJ2K()
end_time <- Sys.time(); simu_time = end_time - start_time; cat ("................................................................",
                                                                "\n","Simulation time is ", round(simu_time,2), "minutes", "\n")
