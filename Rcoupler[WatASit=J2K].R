#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R coupler[WatASit=J2K]      ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script runs J2K-WatASit coupled simulations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2020, Jan-June, by
# J. Veyssier -> make superjams, socket methods
# B. Bonté -> make RCormas methods and Rcoupler script
# B. Richard -> make param and climate Rfunctions and Rcoupler script
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
loadLibraries()

####### 1.4 Core parallelism #######
cores <- parallel:::detectCores(); registerDoParallel(cores-2);

####### 1.5 Read config file #######
config <- readConfigFile()
jamsRootPath<-config[1]; stderrP<-config[2]; stdoutP<-config[3]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Simulation Settings and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### 2.0 Specification of case study name and simulation dates [COMPULSORY] #######
case_study_name <- "Aspres_with_cormas_1989-2013"
date_start_hydro <- as.Date("2015-01-01", "%Y-%m-%d") # Attention la date de début de simulation de j2k doit être la mêne que dans le .jam (date modifiée dans juice!)
date_start_irri <- as.Date("2017-05-01", "%Y-%m-%d"); doy_start_irri <- as.numeric(difftime(date_start_irri,date_start_hydro,units='days'))
date_end_irri <- as.Date("2017-09-30", "%Y-%m-%d"); doy_end_irri <- as.numeric(difftime(date_end_irri,date_start_hydro,units='days'))
date_end_simu <- date_end_irri

####### 2.1 Importation of meteo data input for WatASit [COMPULSORY] #######
climate_file_name <- 'climate_buech_2016-2017.csv'
input_meteo <- computeClimateInput(climate_file_name, date_start_irri, date_end_irri)
str(input_meteo)

####### 2.2 Specification for J2K/JAMS #######
hydro_warmup_doy_nb <- as.numeric(difftime(date_start_irri, date_start_hydro,units='days')-1)
jams_file_name <- "cowat.jam"
reachTopologyFileName <- "reach_cor2_delete_duplicate.par"
makeWaterBalance <- F; if (makeWaterBalance) { storedWater <- NULL; inOutWater <-NULL}

####### 2.3 Specification for WatASit/Cormas [COMPULSORY] #######
with_cormas <- T # choose True (T) or False (F)
if (with_cormas) {
modelName = "COWAT"
parcelFile = "WatASit[1.1.2_COMSES]deasactivateAsas.pcl"
init = "INIT_2017_318x238_upperBuech"
cormas_doy_nb <- as.numeric(difftime(date_end_irri,date_start_irri,units='days'))
# scenario <- "TestConnexion" #Choose Baseline ("simultaneous" scenario) or Alternative ("daily slots" scenario)
scenario <- "BaselineCOWAT"
}

####### 2.4 Activate results saving #######
saveRes <- F #if False -> don't save results if True -> save results


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. WatASit and J2K initialisation ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3.1 Connexion and opening of WatASit model #######
# Open Cormas: dans le répertoire de cormas taper: "wine cormas.exe"
# cormasInVW7dir = cormasRootPath
# setwd(cormasInVW7dir)
# if (!isCormasListening()) {
#   # Open Cormas listenning for instruction
#   system2(
#     'wine',
#       args=c('../bin/win/visual.exe', 'cormas.im' ,'-doit', '"CormasNS.Kernel.Cormas current startWSForR"'),
#     # adding headless successfully launches cormas and the model loading appears to be working
#     # but at some point Rcoupler crashes
#     #args=c('../bin/win/visual.exe', 'cormas.im', '-headless' ,'-doit', '"CormasNS.Kernel.Cormas current startWSForR"'),
#     wait=F, stdout=stdoutP, stderr=stderrP
#   )
#   cat('\n\nWaiting 3 seconds to make sure cormas starts listening...')
#   Sys.sleep(3)
# }
# setwd(wd)
# Ça ouvre une image de cormas avec le modèle chargé mais ne pas regarder
# Dans l'interface principale, aller dans le menu: "simulation/Analysis/cormas<-->R/Sart webservie for R".
# Un petit logo R avec un point vert doit apparaitre.. Le tour est joué.

if (with_cormas) {
r <- openModel(modelName, parcelFile = parcelFile)
r <- setInit(init) # Init method choice
r <- setStep(paste0("R_go",scenario,"Step:")) # Control method choice
r <- activateProbe("flowInRiverReachA", "COWAT") # Probe activation
r <- activateProbe("flowInRiverReachB", "COWAT") # Probe activation
r <- initSimu() # initialize the model
}

####### 3.2 Get Hrus and reaches IDs that are in WatAsit Model #######
if (with_cormas) {
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
  mutate(cormasId = row_number() - 1) %>% #JE NE SAIS PAS POURQUOI il y a un décalage de 1..!
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
}

####### 3.3 Initialize J2K model #######
# On laisse le coupleur lancer JAMS/J2K
initializeJ2K(jamsRootPath, jams_file_name, stdoutP, stderrP, wd)

####### 3.4 Warms-up J2K model #######
# Run j2k on the warming-up simulation period 
print("J2k warming-up")
simuProgress <- txtProgressBar(min = 1,
                               max = hydro_warmup_doy_nb,
                               style = 3)
  for (i in 1:hydro_warmup_doy_nb){ 
    setTxtProgressBar(simuProgress, i)
    # Run step-by-step for water balance purpose
    if (makeWaterBalance) {storedWater <- rbind(storedWater, j2kWaterStorage())}
    j2kMakeStep()
    if (makeWaterBalance) {inOutWater <- rbind(inOutWater, j2kInOutWater())}
  }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Run simulations #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat('\n Running simulation!!!\n')
####### 4.1 Create results dataFrame #######
#TODO

####### 4.2 Run models
  # Run Coupled model on the rest of the simulation period 
  simuProgress <- txtProgressBar(min = 1,
                                 max =doy_end_irri-doy_start_irri,
                                 style = 3)
    for (i in doy_start_irri:doy_end_irri){ 
      setTxtProgressBar(simuProgress, i)
        
  
        ####### A. Getting flow from j2k #######
      reach_Runoff = j2kGetOneValueAllReachs("Runoff") %>%
        tbl_df()
      
      if (with_cormas) {
        ####### B. Updating flows in Cormas #######
        updatedFlows <- cormasReachIds %>%
          mutate(ID = idReach) %>%
          full_join(reach_Runoff, by = "ID") %>%
          # mutate(q = (Runoff / 1000) / (24 * 3600)) %>%
          mutate(q = Runoff / (1000*24*3600)) %>% # Conversion L/j vers m3/s
          mutate(q = replace_na(q,0))
        r <- setAttributesOfEntities("q", "RiverReach",
                                updatedFlows$id,
                                updatedFlows$q)
        rainOnParcells <- j2kGetValuesAllHrus("rain", cormasParcelIds$idParcel) %>%
          tbl_df()
        r <- setAttributesOfEntities("rain", "FarmPlot",
                                     rainOnParcells$ID,
                                     rainOnParcells$rain)
        ####### B. Run WatASit during 24 hours during the irrigation campaign and get irrigtaion #######
        # r <- runSimu(duration = 1)
        r <- runSimu(duration = 24)
        
        ####### C. Set water storage and flow components in J2K #######
        #TODO: update hru surface irri
        #surfaceIrri <- getAttributesOfEntities("irriDailyDose", "Farmplot")
        #j2kSet("surface", hruID[1:length(surfaceIrri$id)] , as.numeric(surfaceIrri$surfaceIrri))
        #TODO: update river reach RD1
        #reachRD1 <- getAttributesOfEntities("dailyQ", "RiverReach")
        #TODO: update htu surface release from canals
        #TODO: update infiltration from canals
        
      } # End of with_cormas
      
        ####### D. Run new j2k daily step and#######
        if (makeWaterBalance) {storedWater <- rbind(storedWater, j2kWaterStorage())} # To calculate the water balance
        j2kMakeStep() # cette fonction fait un step si on lui donne pas de paramètre
        if (makeWaterBalance) {inOutWater <- rbind(inOutWater, j2kInOutWater())}
        
        ####### E. Recover j2k outputs#######
        #TODO
        
        
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
if (saveRes) {
  dir.create("save/simulations_cowat/"); dir.create(paste0("save/simulations_cowat/",case_study_name))
  #TODO: a function to export savings.
}

cat('\n')
j2kStop()
Sys.sleep(3)
killJ2K()
end_time <- Sys.time(); simu_time = end_time - start_time; cat ("................................................................",
                                                                "\n","Simulation time is ", round(simu_time,2), "minutes", "\n")
