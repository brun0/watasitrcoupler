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
# Adding new librairies 
#TODO: To put in the library file
library(gridExtra)

  rm(list=ls()); start_time <- Sys.time();
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 1. R Settings #######
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # General settings for R
  source("Rcoupler[WatASit=J2K]-settings.R")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 2. Simulation Settings and inputs #######
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Active cormas N J2K during coupled simu
  with_cormas <- F # For doing only warm-up without cormas for water balance tests of J2k
  with_J2K <- T # If set to false, only the warming up is performed and
                #the same hydrologic day is used for the whole coupled
                # simulation
  
  #Some general settings of the simulation
  # If water balance is made or not (increases simulation time)
  makeWaterBalance <- T; if (makeWaterBalance) { storedWater <- NULL; inOutWater <-NULL ;localStoredWater <- NULL; localInOutWater <-NULL}
  
  # If original big Hrus are used (not hru plot and thus no cormas)
  bigHrus <- T
  
  # If orginial big Hrus are used, link must be made between cormas HRU plots and J2K big HRUS
  if (bigHrus) {
      source("clean-hrus-par.R") # create the plotsInHrus variable telling for each hruPlot in which big hru it is..
    }
  
  ####### 2.0 Specification of case study name and simulation dates [COMPULSORY] #######
  case_study_name <- "Aspres_with_cormas_1989-2013"
  date_start_hydro <- as.Date("2016-01-01", "%Y-%m-%d") # Attention la date de début de simulation de j2k doit être la mêne que dans le .jam (date modifiée dans juice!)
  date_start_crop <- as.Date("2016-10-15", "%Y-%m-%d"); doy_start_crop <- as.numeric(difftime(date_start_crop,date_start_crop,units='days'))
  date_start_irri <- as.Date("2017-05-01", "%Y-%m-%d"); doy_start_irri <- as.numeric(difftime(date_start_irri,date_start_crop,units='days'))
  #date_end_irri <- as.Date("2017-09-30", "%Y-%m-%d"); doy_end_irri <- as.numeric(difftime(date_end_irri,date_start_crop,units='days'))
  date_end_irri <- as.Date("2017-05-15", "%Y-%m-%d")
  doy_end_irri <- as.numeric(difftime(date_end_irri,date_start_crop,units='days'))
  date_end_simu <- date_end_irri
  
  ####### 2.1 Importation of meteo data input for Optirrig and WatASit [COMPULSORY] #######
  climate_file_name <- 'climate_buech_2016-2017.csv'
  input_meteo <- computeClimateInput(climate_file_name, date_start_crop, date_end_irri)
  str(input_meteo)
  
  ####### 2.2 Specification for J2K/JAMS #######
  hydro_warmup_doy_nb <- as.numeric(difftime(date_start_crop, date_start_hydro,units='days')-1)
  jams_file_name <- "cowat_for_new_com_module.jam"
  if (bigHrus) {
    jams_file_name <- "cowat_for_new_com_module-bigHrus.jam"
  }
  reachTopologyFileName <- "reach_cor2_delete_duplicate.par"

  
  ####### 2.3 Specification for WatASit/Cormas coupling [COMPULSORY] #######
  modelName = "COWAT"
  parcelFile = "WatASit.pcl"
  init = "INIT_2017_318x238_upperBuech"
  cormas_doy_nb <- as.numeric(difftime(date_end_irri,date_start_irri,units='days'))
  #scenario <- "TestConnexion" #Choose Baseline ("simultaneous" scenario) or Alternative ("daily slots" scenario)
  scenario <- "BaselineCOWAT"
  
  # ####### 2.4 Specification for Optirrig coupling [COMPULSORY] #######
  # Unused here
  
  ####### 2.5 Activate results saving #######
  saveRes <- F #if False -> don't save results if True -> save results
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 3. WatASit and J2K initialisation ############################
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 3.1 Connexion and opening of WatASit model #######
  # Open Cormas: dans le répertoire de cormas taper: "wine cormas.exe"
  if (with_cormas) {
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
    
    #Choose probes to activate
    r <- activateProbe("flowInRiverReachGB", "COWAT")
    r <- activateProbe("flowInRiverReachPB", "COWAT")
    r <- activateProbe("numberOfFlooldPlotActions", "COWAT")
    r <- activateProbe("numberOfFloodPlots", "COWAT")
    #r <- activateProbe("irriDailyDoseProbe", "FarmPlot")
    # initialize the model
    r <- initSimu() 
  
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
  
  spatialPlacesWithCanals <- getAttributesOfEntities("canalsId","SpatialPlace") %>%
    tbl_df() %>%
    filter(canalsId > 0) 

  spatialPlacesWithCanals <- spatialPlacesWithCanals %>% 
    left_join(getAttributesOfEntities("idHRU","SpatialPlace"), by = "id") %>%
    mutate(id = as.numeric(as.character(id))) %>%
    arrange(id)
  }
  ####### 3.5 Initialize J2K model #######
  # On laisse le coupleur lancer JAMS/J2K
  killJ2K() # kill jams if it's running
  setwd(jamsRootPath)
  system2(
    'java',
    args=c('-jar', 'jams-starter.jar', '-m', paste0('data/J2K_cowat/',jams_file_name), '-n'),
    wait=F, stdout=stdoutP, stderr=stderrP
  )
  cat('\n\nWaiting 5 seconds to make sure J2K coupling module starts listening...')
  Sys.sleep(5)
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
      if (makeWaterBalance) {
          storedWater <- rbind(storedWater, j2kWaterBalanceStorages())
          # Testing local balance. Only during warming-up.
          localStoredWater <- rbind(localStoredWater, j2kLocalWaterBalanceStorages(selectedHrus = c(548, 554, 567, 634, 696))) 
      }
      # Making step by step j2k simu
      j2kMakeStep()
      if (makeWaterBalance) {
          inOutWater <- rbind(inOutWater, j2kWaterBalanceFlows())
          # Testing local balance. Only during warming-up.
          localInOutWater <- rbind(localInOutWater, j2kLocalWaterBalanceFlows(selectedHrus = c(548, 554, 567, 634, 696),
                                                                              lastHru = 567)) 
          }
    }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 4. Run simulations #######
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat('\nRunning coupled simulation!!!\n')
  ####### 4.1 Create results dataFrame #######
  #TODO
  irrigatedFarmPlots <- NULL
  j2KNetRain <- NULL
  reachsOfCormas <- NULL
  inOutCanals <- NULL
  seepage <- NULL

  reach_Runoff = j2kGetOneValueAllReachs("Runoff") %>%
    tbl_df()
    
  ####### 4.2 Run models
    # Run Coupled model on the rest of the simulation period 
    simuProgress <- txtProgressBar(min = doy_start_irri,
                                   max = doy_end_irri,
                                   style = 3)
    
      for (i in (doy_start_irri:doy_end_irri)){ 
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
        if (with_J2K) {
          reach_Runoff = j2kGetOneValueAllReachs("Runoff") %>%
            tbl_df()
        }
          ####### B. Updating flows in Cormas #######
          updatedFlows <- cormasReachIds %>%
            mutate(ID = idReach) %>%
            full_join(reach_Runoff, by = "ID") %>%
            mutate(q = (Runoff / 1000) / (24 * 3600)) %>%
            mutate(q = replace_na(q,0))
          
          r <- setAttributesOfEntities("q", "RiverReach",
                                  updatedFlows$id,
                                  updatedFlows$q)
          
          #Save for posterity
          reachsOfCormas <- reachsOfCormas %>%
            rbind(updatedFlows %>% 
                    mutate(date = i))
          
          #For tests Or to integrate J2k meteo in Cormas model?
          
          #rainOnParcells <- j2kGetValuesAllHrus("rain", cormasParcelIds$idParcel) %>%
          #tbl_df()
          
          #r <- setAttributesOfEntities("rain", "FarmPlot",
          #                             rainOnParcells$ID,
          #                             rainOnParcells$rain)
          ####### C. Run WatASit during 24 hours during the irrigation campaign and get irrigtaion #######
          r <- runSimu(duration = 24)
          
          ####### D. Get the irrigation from Cormas and set it in J2K accordingly#######
          #TODO
          #get liters of irrigation in cormas hruParcels #ATTENTION ÇA FONCTIONNE BIEN CAR ON A GARDÉ QUE LES FARMPlots irrigués
          surfaceIrri <- getAttributesOfEntities("jamsWaterBuffer", "FarmPlot") %>%
            mutate(irriDoseInLitres = jamsWaterBuffer * 1000) %>%
            mutate(id =  as.numeric(as.character(id))) %>%
            filter(irriDoseInLitres > 0) %>%
            mutate(date = i) %>%
            tbl_df()
          
          #Reset cormas buffers
          surfaceIrri <- surfaceIrri %>% 
            arrange(id)
          r <- setAttributesOfEntities("jamsWaterBuffer", "FarmPlot",
                                       surfaceIrri$id, 
                                       vector("numeric", length(surfaceIrri$id)))
          # Save irrigated plot for posterity
          irrigatedFarmPlots <- irrigatedFarmPlots %>% 
            rbind(surfaceIrri)
          
          # Set irrigation of corresponding j2k HRUPLOTs.
          if(bigHrus) {
            # If needed we aggregate the flows in big HRUs
            surfaceIrri <- surfaceIrri %>%
              left_join(plotsInHrus %>% 
                          rename(id = ID) %>%
                          select(id, bigHRUid)) %>%
              mutate(id = bigHRUid) %>%
              group_by(id, date) %>%
              summarise(irriDoseInLitres = sum(irriDoseInLitres)) %>% 
              arrange(id)
          }
          if (with_J2K) {
            j2kSet("surface", 
                 surfaceIrri$id, 
                 surfaceIrri$irriDoseInLitres) 
          }

          ####### E. Get from Cormas the water withdrawed and added from/to the canals and set J2K transfers accordingly#######
          
          #Take water from the reach of the intake and release it 
          # to the reach of the release. 
          
          #Get the water accumulated since last consultation (sotred in entities jamsWaterBuffer variables)
          #Note that the water buffer is in m3 and cormas was run 24 hourly time step
          qFromIntakes <- getAttributesOfEntities("jamsWaterBuffer", "EwaterIntake") %>%
            mutate(id =  as.numeric(as.character(id))) %>%
            mutate(q = jamsWaterBuffer * 1000) %>%
            arrange(id)
          
            qFromReleases <- getAttributesOfEntities("jamsWaterBuffer", "EwaterRelease") %>%
            mutate(id =  as.numeric(as.character(id))) %>%
            mutate(q = jamsWaterBuffer * 1000) %>%
            arrange(id)
          
          #Reset cormas buffers
          r <- setAttributesOfEntities("jamsWaterBuffer", "EwaterIntake",
                                       qFromIntakes$id, vector("numeric", length(qFromIntakes$id))) 
          r <- setAttributesOfEntities("jamsWaterBuffer", "EwaterRelease",
                                       qFromReleases$id,  vector("numeric", length(qFromReleases$id))) 
          
          #Build table q exchanges from reachs to canals to reachs back or to hruplots (other than irrigation)
          qFromIntakes <- qFromIntakes %>% 
            left_join(getAttributesOfEntities("idReach", "EwaterIntake") %>%
                        mutate(id =  as.numeric(as.character(id))), by="id" )
          qFromReleases <- qFromReleases %>% 
            left_join(getAttributesOfEntities("idReach", "EwaterRelease") %>%
                        mutate(id =  as.numeric(as.character(id))), by="id" )
          qFromReleases <- qFromReleases %>% 
            left_join(getAttributesOfEntities("idHRU", "EwaterRelease") %>%
                        mutate(id =  as.numeric(as.character(id))), by="id" )
          
          if(bigHrus) {
            # If needed we aggregate the flows in big HRUs
            qFromReleases <- qFromReleases %>%
              left_join(plotsInHrus %>% 
                          rename(idHRU = ID) %>%
                          select(idHRU, bigHRUid)) %>%
              ungroup() %>%
              mutate(idHRU = bigHRUid) %>%
              group_by(idHRU, idReach) %>%
              summarise(q = sum(q))
            
            qOfTheDay <- qFromIntakes %>% 
              select(-jamsWaterBuffer,-id) %>%
              mutate(waterIn = T) %>%
              mutate(idHRU = 0) %>%
              dplyr::union(qFromReleases %>%
                             mutate(waterIn = F), by= c("id", "waterIn")) %>% 
              mutate(date = i)
          }
          else {
          qOfTheDay <- qFromIntakes %>% 
            select(-jamsWaterBuffer,-id) %>%
            mutate(waterIn = T) %>%
            mutate(idHRU = 0) %>%
            dplyr::union(qFromReleases %>%
                           select(-jamsWaterBuffer,-id) %>%
                    mutate(waterIn = F), by= c("id", "waterIn")) %>% 
            mutate(date = i)
          }
          
          #Save exchanges for posterity
          inOutCanals <- inOutCanals %>%
            rbind(qOfTheDay)
          
          #Set outflow from reachs at waterIntakes
          qsIn <- qOfTheDay %>% 
            filter(waterIn)
          
          #Carrefull, waterIn in cormas -> reachout car de l'eau sort de J2K..
          if (with_J2K) {
            j2kSet("reachout", 
                 qsIn$idReach, 
                 qsIn$q)
          }
          #Set inflows in hrus at waterReases
          qsOut <- qOfTheDay %>% 
            filter(!waterIn)
          
          # We consider that the hru is "flooded"
          if (with_J2K) {
            j2kSet("surface", 
                 qsOut$idHRU, 
                 qsOut$q)
          }
          #Get the water seepage from canals and put it in the rigth HRUS
          qFromSeepage <- getAttributesOfEntities("jamsWaterBuffer", "SpatialPlace") %>%
            mutate(id =  as.numeric(as.character(id))) %>%
            right_join(spatialPlacesWithCanals, by ="id") %>%
            filter(jamsWaterBuffer > 0) %>%
            mutate(q = jamsWaterBuffer * 1000) %>%
            arrange(id) %>%
            filter(idHRU > 0) %>%
            tbl_df()
          
          #reset cormas buffer
          r <- setAttributesOfEntities("jamsWaterBuffer", "SpatialPlace",
                                       qFromSeepage$id,
                                  vector("numeric", length(qFromSeepage$id))) 
          
          #save seepage for posterity
          seepage <- qFromSeepage %>% 
            group_by(canalsId, idHRU) %>%
            summarise(q = sum(q)) %>% 
            mutate(date = i) %>%
            rbind(seepage)
          
          if(bigHrus) {
            # If needed we aggregate the flows in big HRUs
            qFromSeepage <- qFromSeepage %>%
              left_join(plotsInHrus %>% 
                          rename(idHRU = ID) %>%
                          select(idHRU, bigHRUid)) %>%
              ungroup() %>%
              mutate(idHRU = bigHRUid) %>%
              group_by(idHRU) %>%
              summarise(q = sum(q))
          }
          
          #get hru Ids of spatial place
          qFromSeepage <- qFromSeepage %>%
            group_by(idHRU) %>%
            summarise(q = sum(q))
          
          
          #we consider seepage having the same effect as "surface" irrigation at the moment.. 
          #utiliser un module infiltration par seepage?
          if (with_J2K) {
            j2kSet("surface", 
                 qFromSeepage$idHRU, 
                 qFromSeepage$q)
          }
          ####### F. Run new j2k daily step #######
          if (with_J2K) {
            if (makeWaterBalance) {storedWater <- rbind(storedWater, j2kWaterBalanceStorages())} # To calculate the water balance
       
                 j2kMakeStep() # cette fonction fait un step si on lui donne pas de paramètre

            if (makeWaterBalance) {inOutWater <- rbind(inOutWater, j2kWaterBalanceFlows())}
          }
      } # End of time loop
  
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 6. Make water balance [Optionnal] #######
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (makeWaterBalance){
    # Création des tables agrégées organisées pour afficher les graphiques de bilans
    storages <- storedWater %>% tbl_df()
    inOut <- inOutWater %>% tbl_df()
    waterSummary <- cbind (storages, inOut) %>% tbl_df() %>% mutate(day = row_number())
    localStorages <- localStoredWater %>% tbl_df()
    localInOut <- localInOutWater %>% tbl_df()
    localWaterSummary <- cbind (localStorages, localInOut) %>% tbl_df() %>% mutate(day = row_number())
    
    
    waterSummary %>%
      arrange(day) %>%
      #filter(day > nbDays) %>%
      mutate(inWater = rain + snow) %>%
      mutate(outWater = etact + runoff) %>%
      mutate(storage = hruStorage + reachStorage) %>%
      mutate(storageNextDay = lead(storage)) %>%
      mutate(deltaS = storageNextDay - storage) %>%
      mutate(waterBalance =  inWater - outWater) %>%
      mutate(waterLoss = storageNextDay - storage - waterBalance) %>%
      filter(day > 150) %>%
      #mutate(cumWaterLoss = cumsum(waterLoss)) %>%
      ggplot() +
      geom_line(aes(x = day, y = waterLoss))
    
    localWaterSummary %>%
      arrange(day) %>%
      #filter(day > nbDays) %>%
      mutate(inWater = rain + snow) %>%
      mutate(outWater = etact + outflow) %>%
      mutate(storage = hruStorage) %>%
      mutate(storageNextDay = lead(storage)) %>%
      mutate(deltaS = storageNextDay - storage) %>%
      mutate(waterBalance =  inWater - outWater) %>%
      mutate(deltaS = storageNextDay - storage) %>%
      mutate(waterLoss = deltaS - waterBalance) %>%
      filter(day > 150) %>%
      mutate(cumWaterLoss = cumsum(waterLoss)) %>%
      ggplot() +
      geom_line(aes(x = day, y = waterLoss, color = "loss"))
     # geom_line(aes(x = day, y = - outWater, color = "outWater")) +
    #  geom_line(aes(x = day, y = deltaS, color = "deltaS")) + 
     # geom_line(aes(x = day, y = - etact, color = "et")) + 
    #  geom_line(aes(x = day, y = - outflow, color = "outflow")) + 
     # geom_line(aes(x = day, y = inWater, color = "inWater"))  + 
    #geom_line(aes(x = day, y = snow, color = "snow"))  +
     # geom_line(aes(x = day, y = rain, color = "rain")) 
    
    # Graphique intuitif pour voir si le bilan est correct 
    #(à l'echelle du bassin)
    # Pour toute la durée de simulation incluant la pré-chauffe
    
    waterSummary %>%
      mutate(inWater = rain + snow) %>%
      mutate(outWater = etact + runoff) %>%
      mutate(storage = hruStorage + reachStorage) %>%
      mutate(nextday = day + 1) %>%
      ggplot() +
      geom_line(aes(x = day, y = storage, color = "stock")) + 
      geom_line(aes(x = day, y = inWater - outWater, color = "waterBalance")) +
      geom_point(aes(x = nextday, y = storage + inWater - outWater, color = "PastStoragePlusBalance")) +
      ylab("Litres")
    
    # Graphique intuitif pour voir si le bilan est correct 
    #(à l'echelle du bassin)
    # Pour la périodé couplée
    waterSummary %>%
      filter(day > nbDays) %>%
      mutate(inWater = rain + snow) %>%
      mutate(outWater = etact + runoff) %>%
      mutate(storage = hruStorage + reachStorage) %>%
      mutate(nextday = day + 1) %>%
      ggplot() +
      geom_line(aes(x = day, y = storage, color = "stock")) + 
     # geom_line(aes(x = day, y = inWater - outWater, color = "waterBalance")) +
      geom_point(aes(x = nextday, y = storage + inWater - outWater, color = "PastStoragePlusBalance")) +
      ylab("Litres")
    
totalSeepage <- seepage  %>%
      ungroup() %>%
      group_by(date) %>%
      summarise(seepage=sum(q))

  #plot du seepage pendant la période couplée
  totalSeepage %>% 
    ggplot() +
    geom_line(aes(x=date, y =seepage))
  
  
  # Table agrégée du bilan pour le modèle CORMAS
  cormasWaterSummary <- inOutCanals %>%
      tbl_df() %>%
      group_by(waterIn, date) %>%
      summarise(q = sum(q)) %>%
      arrange(date) %>%
      spread(waterIn, q) %>%
      rename(waterInCanal = `TRUE`) %>%
      rename(waterOutCanal = `FALSE`) %>%
      left_join(totalSeepage) %>%
      left_join(irrigatedFarmPlots %>% 
                  group_by(date) %>%
                  summarise(floodedInParcells = sum(irriDoseInLitres)),
                by = "date") %>%
  mutate(floodedInParcells = replace_na(floodedInParcells, 0)) %>%
  mutate(waterLoss = waterInCanal - waterOutCanal - seepage - floodedInParcells)

  # Plot de l'eau "perdue" au sein de Cormas pendant la simulation couplée
  #(Du point de vue de Cormas/watAsit)
  
cormasWaterSummary %>%
  ggplot() +
  geom_line(aes(x= date, y = waterLoss))
  
  # Plot des flux d'eau dans Cormas (du point de vue de cormas)
cormasWaterSummary %>% 
  mutate(waterOut = floodedInParcells + seepage + waterOutCanal) %>%
  gather("flows", "value", -date,-waterLoss) %>%
  ggplot() +
  geom_line(aes(x= date, y = value, color=flows))
    
irrigatedFarmPlots %>%
      tbl_df()
    
# Fonction qui plot les variables des bilans fait par J2k sur l'ensemble de la simu
library(gtable)
library(grid)
plot_bilan <- function (period = c(300,400)){
wL <- waterSummary %>%
      arrange(day) %>%
      #filter(day > nbDays) %>%
      mutate(inWater = rain + snow) %>%
      mutate(outWater = etact + runoff) %>%
      mutate(storage = hruStorage + reachStorage) %>%
      mutate(storageNextDay = lead(storage)) %>%
      mutate(deltaS = storageNextDay - storage) %>%
      mutate(waterBalance =  inWater - outWater) %>%
      mutate(waterLoss = storageNextDay - storage - waterBalance) %>%
      mutate(cumWaterLoss = cumsum(waterLoss)) %>%
      ggplot() +
      coord_cartesian(xlim=period) +
      geom_line(aes(x = day, 
                    color = "waterLoss",
                    y = waterLoss))#+
                    #y=cumWaterLoss
      #geom_line(aes(x = day, y = cumWaterLoss, color = "cummulativeWaterLoss")) +
      #theme(legend.position = "bottom")

wFlows <- waterSummary %>%
  select(day, runoff, rain, snow, etact) %>%
  gather("flow", "value", -day) %>%
  ggplot() +
  geom_line(aes(x = day, y = value, color = flow)) + 
  coord_cartesian(xlim=period) #+
  #theme(legend.position = "bottom")
  
wStocks <- waterSummary %>%
  select(day, hruStorage, reachStorage) %>%
  gather("stock", "value", -day) %>%
  ggplot() +
  coord_cartesian(xlim=period) +
  geom_line(aes(x = day, y = value, color = stock)) #+
  #theme(legend.position = "bottom")
g1 <- ggplotGrob(wL)
g2 <- ggplotGrob(wFlows)
g3 <- ggplotGrob(wStocks)
g <- rbind(g1, g2, g3, 
             size ="first")
grid.newpage()
grid.draw(g)
# %>%
  #ggsave(file =paste0("bilan-test-", period[1], "-",period[2], ".pdf"),
  #       width=21, height=29.7, units="cm")
}
plot_bilan(c(475,500))
plot_bilan(c(1,400))
    #sauvegarde du bilan dans un fichier
    stamp <- format(Sys.time(), "%y%m%d-%H%M%S")
    write.csv(waterSummary, paste0("waterSummary-",stamp,".csv"), row.names = F)
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
