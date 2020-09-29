#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R coupler 2.0      ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script runs J2K-WatASit-Optirrig coupled simulations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2020, Jan-June, by
# J. Veyssier -> make superjams, socket methods and Rcoupler
# B. Bonté -> make RCormas methods and Rcoupler
# B. Richard -> make param and climate Rfunctions and Rcoupler
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
case_study_name <- "Aspres_with_cormas_1989-2013"
date_start_hydro <- as.Date("2015-01-01", "%Y-%m-%d") # Attention la date de début de simulation de j2k doit être la mêne que dans le .jam (date modifiée dans juice!)
date_start_crop <- as.Date("2016-10-15", "%Y-%m-%d"); doy_start_crop <- as.numeric(difftime(date_start_crop,date_start_crop,units='days'))
date_start_irri <- as.Date("2017-05-01", "%Y-%m-%d"); doy_start_irri <- as.numeric(difftime(date_start_irri,date_start_crop,units='days'))
date_end_irri <- as.Date("2017-09-30", "%Y-%m-%d"); doy_end_irri <- as.numeric(difftime(date_end_irri,date_start_crop,units='days'))
date_end_simu <- date_end_irri

####### 2.1 Importation of meteo data input for Optirrig and WatASit [COMPULSORY] #######
climate_file_name <- 'climate_buech_2016-2017.csv'
input_meteo <- computeClimateInput(climate_file_name, date_start_crop, date_end_irri)
str(input_meteo)

####### 2.2 Specification for J2K/JAMS #######
hydro_warmup_doy_nb <- as.numeric(difftime(date_start_crop, date_start_hydro,units='days')-1)
jams_file_name <- "cowat.jam"
reachTopologyFileName <- "reach_cor2_delete_duplicate.par"

makeWaterBalance <- F; if (makeWaterBalance) { storedWater <- NULL; inOutWater <-NULL}

####### 2.3 Specification for WatASit/Cormas coupling [COMPULSORY] #######
with_cormas <- T # choose True (T) or False (F)
if (with_cormas) {
modelName = "COWAT"
parcelFile = "WatASit.pcl"
init = "INIT_2017_318x238_upperBuech"
cormas_doy_nb <- as.numeric(difftime(date_end_irri,date_start_irri,units='days'))
scenario <- "TestConnexion" #Choose Baseline ("simultaneous" scenario) or Alternative ("daily slots" scenario)
}

# ####### 2.4 Specification for Optirrig coupling [COMPULSORY] #######
# Unused here

####### 2.5 Activate results saving #######
saveRes <- F #if False -> don't save results if True -> save results


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. WatASit and J2K initialisation ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3.1 Connexion and opening of WatASit model #######
# Open Cormas: dans le répertoire de cormas taper: "wine cormas.exe"
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
r <- activateProbe("flowInRiverReachA", "COWAT")
r <- activateProbe("flowInRiverReachB", "COWAT")
r <- initSimu() # initialize the model

####### 3.5 Get Hrus and reaches IDs that are in WatAsit Model #######
cormasParcelIds <- getAttributesOfEntities("idParcel","FarmPlot") %>%
  tbl_df()

cormasSpatialPlaceIds <- getAttributesOfEntities("idReach","SpatialPlace") %>%
  tbl_df()

correctReachIds <- read.table("superjams/data/J2K_cowat/parameter/step2_streams_new_div_OK2.asc",
                              sep = " ",
                              dec = ".",
                              skip = 6) %>%
  mutate(line = row_number()) %>%
  gather("col", "j2kID", -line) %>% 
  mutate(col = as.numeric(str_remove(col,"V"))) %>%
  arrange(line, col) %>%
  mutate(cormasId = row_number() - 1) %>% #JE NE SAIS PAS POURQUOI!!
  filter(j2kID != 0) %>%
  tbl_df() %>% 
  full_join(cormasSpatialPlaceIds %>% 
              mutate(cormasId = as.numeric(as.character(id)))) %>%
  arrange(cormasId) %>%
  mutate(j2kID = replace_na(j2kID,0))

r <- setAttributesOfEntities("idReach",
                             "SpatialPlace", 
                             correctReachIds$cormasId, 
                             correctReachIds$j2kID)

cormasReachIds <- getAttributesOfEntities("idReach","RiverReach") %>%
  tbl_df()


####### 3.5 Initialize J2K model #######
# On laisse le coupleur lancer JAMS/J2K
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

####### 3.6 Warms-up J2K model #######
# Run j2k on the warming-up simulation period 
nbDays <- as.numeric(difftime(date_start_irri,date_start_hydro,units='days'))
simuProgress <- txtProgressBar(min = 1,
                               max = nbDays,
                               style = 3)
# J2k Warming-up
print("J2k warming-up")
  for (i in 1:nbDays){ 
    #cat("\n","Running step:",i,"\n"); 
    setTxtProgressBar(simuProgress, i)
    # Run step-by-step for balance purpose
    if (makeWaterBalance) {storedWater <- rbind(storedWater, j2kWaterStorage())}
    j2kMakeStep()
    if (makeWaterBalance) {inOutWater <- rbind(inOutWater, j2kInOutWater())}
  }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Run simulations #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat('\nRunning coupled simulation!!!\n')
####### 4.1 Create results dataFrame #######
#TODO

####### 4.2 Run models
  # Run Coupled model on the rest of the simulation period 
  nbDays <- as.numeric(difftime(date_end_irri,date_start_irri,units='days'))
  simuProgress <- txtProgressBar(min = 1,
                                 max =nbDays,
                                 style = 3)
  
    for (i in 1:nbDays){ 
      setTxtProgressBar(simuProgress, i)
        
        ####### A. Getting flow from j2k #######
      #TODO problème: beaucoup de reachs sont dans cormas mais pas dans 
      # les reachs de j2k_cowat_buech_ju_couplage.jam!
      # du coup pour tester, au lieu de prendre le bon code ci-dessous,
      # on prend un autre 
        #reach_Runoff = j2kGetOneValueAllReachs("Runoff", 
        #                                       cormasReachIds %>%
        #                                         select(idReach) %>%
        #                                         pull())
      reach_Runoff = j2kGetOneValueAllReachs("Runoff") %>%
        tbl_df()
      
        ####### B. Updating flows in Cormas #######
        updatedFlows <- cormasReachIds %>%
          mutate(ID = idReach) %>%
          full_join(reach_Runoff, by = "ID") %>%
          mutate(q = (Runoff / 1000) / (24 * 3600)) %>%
          mutate(q = replace_na(q,0))
        
        r <- setAttributesOfEntities("q", "RiverReach",
                                updatedFlows$id,
                                updatedFlows$q)
        
        rainOnParcells <- j2kGetOneValueAllHrus("rain", cormasParcelIds$idParcel)
        r <- setAttributesOfEntities("rain", "FarmPlot",
                                     rainOnParcells$id,
                                     rainOnParcells$netRain)
        ####### B. Run WatASit during 24 hours during the irrigation campaign and get irrigtaion #######
        #TODO
        #if (i >= date_start_irri){# activate coupling with WatASit in Cormas plateform
        # RunWatASit(daily_step = i, input_meteo = input_meteo)
        #}
        r <- runSimu(duration = 1)
        
        ####### C. Set the irrigation in J2K #######
        #TODO
        #surfaceIrri <- getAttributesOfEntities("surfaceIrri", "HRU")
        #j2kSet("surface", hruID[1:length(surfaceIrri$id)] , as.numeric(surfaceIrri$surfaceIrri)) # Mais en utilisant en fait les irriDailyDose ou truc du genre
        # récupérés ci-dessus depuis cormas
        
        ####### D. Run new j2k daily step #######
        if (makeWaterBalance) {storedWater <- rbind(storedWater, j2kWaterStorage())} # To calculate the water balance
        j2kMakeStep() # cette fonction fait un step si on lui donne pas de paramètre
        if (makeWaterBalance) {inOutWater <- rbind(inOutWater, j2kInOutWater())}
    } # End of time loop
  
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
  plotBalance(storedWater,inOutWater, "waterBalanceTest")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 7. Save simulation results #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TODO: a function to export savings.
if (saveRes) {
  dir.create("save/simulations_cowat/"); dir.create(paste0("save/simulations_cowat/",case_study_name))
}
cat('\n')
j2kStop()
Sys.sleep(3)
killJ2K()
end_time <- Sys.time(); simu_time = end_time - start_time; cat ("................................................................",
                                                                "\n","Simulation time is ", round(simu_time,2), "minutes", "\n")
