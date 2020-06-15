#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R coupler      ################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script makes the WatASit model from Cormas plateform to communicate
# with the J2K model implemented in SuperJams Plateform 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2020, Jan-June, by
# J. Veyssier -> make SuperJams modules and communication Rfunctions
# B. Bonté -> make this script and RCormas
# B. Richard -> make this script and config simulations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list=ls());
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

####### 2.0 Specification of case study and total simulation period #######
case_study_name <- "Aspres"
date_start_sim <- as.Date("2016-10-15", "%Y-%m-%d"); date_end_sim <- as.Date("2017-07-31", "%Y-%m-%d")
# year_sim <- 2017

####### 2.1 Specification for WatASit coupling #######
cormas_sim_day_nb <- 30 * 4
cormas_doy_start <- 198 # day of the year of first step in cormas

####### 2.2 Specification for Optirrig coupling #######
with_optirrig <- F
optirrig_doy_start <- NA # day of the year of first step in Optirrig

####### 2.3 Specification for J2K coupling #######
J2k_doy_start <- 1 # day of the year of first step in J2K
makeWaterBalance <- T
if (makeWaterBalance) {
  storedWater <- NULL
  inOutWater <-NULL
}

####### 2.4 Importation of meteo data input  #######
climate_file_name <- 'climate_buech_2016-2017.csv'
input_meteo <- computeClimateInput(climate_file_name, date_start_sim, date_end_sim)
# input_meteo = read.csv(file.path(script.dirname, 'climatefile/climate_buech_2017.csv'), header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
# meteo = input_meteo[which(input_meteo$year == year_sim),]
str(input_meteo)

####### 2.5 Generation of an Optirrig paramfile for each WatASit plots  #######
list_idParcel <- NULL
if (with_optirrig) {
  list_idParcel <- optiParams(
    paste0(script.dirname, 'paramfiles/'),
    case_study_name,
    'watasit.csv',
    'paramDB.csv',
    'climate_buech_2017.csv',
    year_sim,
    1,
    365,
    'irrig_file_watasit.dat'
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. WatASit initialization and J2K initialisation #######
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
r <- openModel("COWAT", parcelFile="WatASit[2.0_TT].pcl")
# just for test purpose
#r <- openModel("COWAT", parcelFile="WatASit[EMSpaper].pcl")

####### 3.2 Activation of probes about crops (Facultatif: to get data from cormas) #######
# probe_names <- c("abandonedCropEvent", "ASAinquiries", "exceedMaxWithdrawalEvent", "qIntake", "unrespectRestrictionEvent", "sumQOfEwaterReleases", "f1IrrigatedPlotNb", "f2irrigatedPlotNb", "f3irrigatedPlotNb", "f5irrigatedPlotNb", "f6irrigatedPlotNb", "f7irrigatedPlotNb", "f10irrigatedPlotNb", "f11irrigatedPlotNb", "f12irrigatedPlotNb","f14irrigatedPlotNb", "f16irrigatedPlotNb")
# for (i in 1:length(probe_names)) { r <- activateProbe(probe_names[i],"COWAT") }

####### 3.3 Choose of WatASit initial state and time step function (scenarios) #######
# Note that the model is not initialized, we just set the init method..
r <- setInit("INIT_2017_54x44") # Initialization choice
r <- setStep("R_goBaselineStep:") # Scenario choice

####### 3.4 Initialize Cormas model #######
r <- initSimu()

####### 3.5 Initialize J2K model #######
# On laisse le coupleur lancer JAMS/J2K
# On aussi peut lancer J2K manuellement de la manière suivante.
# En étant dans le dossier "superjams" (qui vient de l'archive superjams.zip) :
# java -jar jams-starter.jar -m data/J2K_cowat/j2k_cowat_buech_ju_couplage.jam -n
# et hop ça lance juste le modèle, pas d'interface graphique, pas  d'éditeur de modèle. Pour l'arrêter : CTRL+C .
# S'il s'arrête tout seul au bout de 2 minutes d'inactivité : CTRL+C et on peut le relancer avec la même commande.
#  "Rfunctions/Rj2k.R".

# kill jams if it's running
killJ2K()
setwd(jamsRootPath)
system2(
  'java',
  args=c('-jar', 'jams-starter.jar', '-m', 'data/J2K_cowat/exemple_aspersion_lai.jam', '-n'),
  # args=c('-jar', 'jams-starter.jar', '-m', 'data/J2K_cowat/cowat.jam', '-n'),
  wait=F, stdout=stdoutP, stderr=stderrP
)
cat('\n\nWaiting 3 seconds to make sure J2K coupling module starts listening...')
Sys.sleep(3)
setwd(wd)

cat('\nRunning simulation!!!\n')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Initialization of Optirrig model #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (with_optirrig) {
  #param_frame <- data.frame(); irr <- data.frame()
  #for (i in 1:length(list_idParcel)){
  #  ####### 4.1 Load params of each plot #######
  #  param = read.csv(paste0(wd,'paramfiles/paramfiles_',case_study_name,'/',list_idParcel[i],'/parF', list_idParcel[i],'.csv'), header = TRUE,sep=",",dec = ".",stringsAsFactor=FALSE)
  #
  #  ####### 4.2 Create frame with all parameters #######
  #  param_frame <- rbind(param_frame, param)
  #
  #  ####### 4.3 Create frame with all irrigation vectors  #######
  # I1   = as.vector(meteo$day) ; I1[] = 0; I2 = I1 # I1 is surface irrigation and I2  I2 is deep buried irrigation (I2 is null)
  # irr = rbind(irr,I1)
  #}
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 5. Run simulation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### 5.1 Create results dataFrame #######
crop_results <- data.frame(idParcel = NULL, wsi = NULL, lai = NULL, hi = NULL, cropMaturitySignal = NULL)
farmers_results <- data.frame(id = NULL, day = NULL, nbFloodPlotAffToday = NULL, dosCounter = NULL)
simQ_mat <- matrix()

####### 5.2 Run J2K from 1 DOY to DOY 120 (1er mai) to simulate the new state of the watershed before the irrigation campaign #######  #######
cat('\nStarting pre-simulation with j2K!!!\n')
if (makeWaterBalance) {
  cat('\nComputing each time step water balance may take time!!!\n')
  preSimuProgress <- txtProgressBar(min = 0,
                                 max = cormas_doy_start,
                                 style = 3)
  for (t in 1:(cormas_doy_start - 1)){
    setTxtProgressBar(preSimuProgress, t)
    storedWater <- rbind(storedWater, j2kWaterStorage())
    j2kMakeStep()
    inOutWater <- rbind(inOutWater, j2kInOutWater())
  } 
} else {
  cmdResult = j2kMakeStep(cormas_doy_start - 1)
}
####### 5.3 Run Optirrig simulation without WataSit from 1 DOY to DOY 120 (1er mai) to simulate the new state of crops out of irrigation campaign #######
if (with_optirrig) {
## for (day in optirrig_doy_start:(cormas_doy_start - 1)){
#
#  ####### 5.3.1 Initialize optirrig on day 1 #######
#  if (day == 1) {
#    cstes_list <- list()
#    inval_list <- list()
#    vect_list <- list()
#    for (i in 1:length(list_idParcel)){
#      init <- init_optirr(param_frame[i,], meteo)
#      cstes = init$cstes; cstes_list <- rbind(cstes_list, cstes) # Constants
#      inval = init$inval; inval_list <-  rbind(inval_list, inval) # Calculation values for which the history is not required (list of values)
#      vect  = init$vect ; vect_list <-  rbind(vect_list, vect) # Vector of stored state variables as time series in vectors (list of vectors)
#    }
#  }
#
#  ####### 5.3.2 Simulate Optirrig on other days #######
#  if (day != 1) {
#    inval2_list <- list()
#    vect2_list <- list()
#    for (i in 1:length(list_idParcel)){
#      cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
#      I1 = I2 # I2 is deep irrigation (buried drip), I2 is null
#      param<-param_frame[i,]; cstes<-cstes_list[i,];  inval<-inval_list[i,]; vect<-vect_list[i,]
#      optirday = daily_optirr(param,
#                              meteo,
#                              cstes,
#                              inval,
#                              vect,
#                              I1, # Surface irrigation
#                              I2, # Deep irrigation (buried drip)
#                              day) # Time step
#      inval2 = optirday$inval ; inval2_list <- rbind(inval2_list, inval2) ; inval_list[i,] <- inval2 # New constants
#      vect2  = optirday$vect ; vect2_list <- rbind(vect2_list, vect2) ; vect_list[i,] <- vect2 # New vectors
#    }
#  }
#}
}

cat('\nStarting coupled simulation simulation!!!\n')
simuProgress <- txtProgressBar(min = cormas_doy_start,
                               max = cormas_doy_start + cormas_sim_day_nb,
                               style = 3)

####### 5.4 Run WatASit-Optirrig-J2K coupled simulation from cormas_doy_start #######
for (day in cormas_doy_start:(cormas_doy_start + cormas_sim_day_nb)) {
  setTxtProgressBar(simuProgress, day)
  ####### 5.4.1 Update Cormas Meteo and flow in riverReachs#######
    ## Updating meteo
    P <- input_meteo$P
    setAttributesOfEntities("p", "Meteo", 1, P[day]) # Precipitation conditions of the day
    p_forecast = sum(c(P[day:(day +2)]), na.rm = TRUE)
    if (p_forecast > 0) {
      p_forecast = 1
    }
    setAttributesOfEntities("p_forecast", "Meteo", 1, p_forecast) # Precipitation forecast for the next 3 days
    if (day >= 2) {
      p_cumTenDays = sum(c(P[max(1,(day-10)):(day-1)]), na.rm = TRUE)
      p_cumFifteenDays = sum(c(p_cumTenDays, P[max(1,(day-15)):max(2,(day-11))]), na.rm = TRUE)
    } else {
      p_cumTenDays = 0
      p_cumFifteenDays = 0
    }
    setAttributesOfEntities("p_cumTenDays", "Meteo", 1, p_cumTenDays) # Calculate cumulative precipitation for the last 10 days
    # setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, p_cumFifteenDays) # Calculate cumulative precipitation for the last 15 days
    setAttributesOfEntities("p_cumTwelveDays", "Meteo", 1, p_cumFifteenDays) # Calculate cumulative precipitation for the last 15 days

    ## Updating river flow
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

  ####### 5.4.2 Run coupled simulation of 24 hours #######
  r <- runSimu(duration = 24)
  response <- gettext(r[[2]])
  # To check if runSimu is done
  if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {
    stop("RUN STOPPED", call.=FALSE)
  }
  obs1 <- NULL
  obs2 <- NULL
  #obs1 <- getAttributesOfEntities("floodAffCounter", "Efarmer")
  #obs2 <- getAttributesOfEntities("floodActCounter", "Efarmer")
  if (!((is.null(obs1) | is.null(obs2)))) {
  obs <- left_join(obs1, obs2, by = "id")
  obs$day = day
  farmers_results <- farmers_results %>%
    bind_rows(obs)
  }

  ####### 5.4.3 Get the state of crops from Cormas #######
  #idParcel      <- getAttributesOfEntities("idParcel", "Ecrop")
  #list_idParcel <- idParcel$idParcel
  #harvestSignal <- getAttributesOfEntities("harvestSignal", "Ecrop")
  #irriDailyDose <- getAttributesOfEntities("irriDailyDose", "Ecrop")

  ####### 5.4.4 Simulate the new state of crops with Optirrig #######
  if (with_optirrig) {
    if (day != 1) {
      inval2_list <- list()
      vect2_list <- list()
      for (i in 1:length(list_idParcel)) {
        cat("Simulation of day", day, "and parcel number", i, "(idParcel =", list_idParcel[i], ")", "\n")
        irr[i,day]  <- irriDailyDose$irriDailyDose[i]
        irr <- as.matrix(irr)
        I1 = irr[i,] # Update irrigation from Cormas
        param <- param_frame[i,]
        cstes <- cstes_list[i,]
        inval <- inval_list[i,]
        vect <- vect_list[i,]
        optirday = daily_optirr(
          param,
          meteo,
          cstes,
          inval,
          vect,
          I1, # Surface irrigation
          I2, # Deep irrigation (buried drip)
          day # Time step
        )
        inval2 = optirday$inval
        inval2_list <- rbind(inval2_list, inval2)
        inval_list[i,] <- inval2 # News constants
        vect2  = optirday$vect
        vect2_list <- rbind(vect2_list, vect2)
        vect_list[i,] <- vect2 # New vectors
      }
    }
  }
  ####### 5.4.5 Set the irrigation in J2K #######
  j2kSet("drip", c(1,2,3), c(100, 100, 100)) # Mais en utilisant en fait les irriDailyDose ou truc du genre
                                              # récupérés ci-dessus depuis cormas

  # set actLai dans J2K for test (commented by Bruno B.)
  #j2kSet('actLaiCom', c(10363, 10362, 8934), c(100, 100, 100))
  #print(' ')
  #print('actLAI of some HRUs :')
  #print(head(testRes, 10))

  ####### 5.4.6 Simulate the new state of watershed with J2K #######
  # cette fonction fait un step si on lui donne pas de paramètre
  if (makeWaterBalance) {
    storedWater <- rbind(storedWater, j2kWaterStorage())
    j2kMakeStep()
    inOutWater <- rbind(inOutWater, j2kInOutWater())    
  } else{
    j2kMakeStep()
    # on peut aussi faire N steps comme ça
    #j2kMakeStep(20)
  }
  
  #reachRD1DataFrame = j2kGetOneValueAllReachs('actRD1')
  #hruNetrainDataFrame = j2kGetOneValueAllHrus('netrain')
  #dailySimQ = j2kGetOneValueAllReachs("Runoff")
  # SimQAtGauge <- simQ[which(as.numeric(simQ[,1])==62200)] #Le reach est manquant avec ces paramètres
  #dailySimQAtGauge <- as.numeric(dailySimQ[which(as.numeric(dailySimQ[,1])==59200),2]) #Le reach est manquant avec ces paramètres
  #simQ_mat[day] <- dailySimQAtGauge
}

plotBalance(storedWater,inOutWater)
cat('\n')
j2kStop()
Sys.sleep(3)
killJ2K()
