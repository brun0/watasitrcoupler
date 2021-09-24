#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      airJ2KCormasCoupler      ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Couples J2K model with CORMAS model [Richard*Bonté*Veyssier*Braud*Barreteau]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# General settings for R
rm(list=ls()); startTime = Sys.time();
wd <- getwd()
source('Rcoupler[WatASit=J2K]-settings.R')

# Simulation Settings
caseName = 'test'
saveOutputs = F # Comment: Si True les sorties seront enregistrées dans ~airJ2K/superjams/data/.../output

# J2K model settings
#Starting date of cowat_for_new_com_module_GB.jam is 2016-01-01
dateStartJ2K = as.Date('2016-01-01', '%Y-%m-%d') # Comment: Attention la date de début de simulation doit être la même que dans le fichier modèle .jam (à modifier "à la main" dans le .jam)
dateEndJ2K = as.Date('2018-07-31', '%Y-%m-%d') # Comment: Choisir une date de fin inférieure ou égale à celle dans le .jam (ou modifier le .jam)
jamsFileName = 'cowat_for_new_com_module_GB.jam' # Comment: Renseigner le nom du fichier modèle .jam qui est à placer dans le répertoir ~/watasitrcoupler/superjams/data/.../

# CORMAS model settings
dateStartCORMAS = as.Date('2017-05-01', '%Y-%m-%d')
dateEndCORMAS = as.Date('2017-09-30', '%Y-%m-%d')
modelName = "COWAT"
parcelFile = "WatASitAllAsas2.pcl"
init = "INIT_2017_318x238_upperBuech"
control = "R_goBaselineCOWATStep:"

# Initializes CORMAS model
connectCORMAS(modelName, parcelFile, cormasRootPath, stdoutP, stderrP, wd)
probesList = c("flowInRiverReachGB","flowInRiverReachPB","numberOfFlooldPlotActions","numberOfFloodPlots") # Comment: Give the probe names you want to activate
initCORMAS(init, control, modelName, probesList)
cormasReachIds <- getAttributesOfEntities("idReach","RiverReach") %>% as_tibble() # Comment: On récupère les id des reach dans CORMAS
cormasFarmPlotIds <- getAttributesOfEntities("idParcel","FarmPlot") %>% as_tibble() %>% filter(idParcel > 0)# Comment: On récupère les id des farm plots dans CORMAS

# Initializes J2K model
killJ2K() # Comment: Kill JAMS if it's running
initJ2K(jamsRootPath, jamsFileName, stdoutP, stderrP, wd) # Comment: Lance le fichier modèle dans JAMS

# Runs J2K until CORMAS starts 
print('J2K simulation starting...')
simuProgress <- txtProgressBar(min = 1, max =as.numeric(difftime(dateStartCORMAS,dateStartJ2K,units='days')), style = 3) # Comment: Barre de progression
inOutWater = NULL; runoffSelectedReaches = NULL ; reachsOfCormas =NULL # Comment: Variable de stockage des sorties J2K
for (i in 1:as.numeric(difftime(dateStartCORMAS,dateStartJ2K,units='days'))){ # Comment: Boucle temporelle
  setTxtProgressBar(simuProgress, i)
  j2kMakeStep() # Comment: Un seul pas de temps
  #inOutWater <- rbind(inOutWater, j2kWaterBalanceFlows()) 
  # Comment: Enregistrement du CatchmentRunoff et somme des precipitations, ETR et T°C sur l'ensemble des HRUs, il existe aussi une fonction pour sélectionner uniquement certaines HRUs (voir dans Rj2k.R)
  # runoffSelectedReaches <- rbind(runoffSelectedReaches, j2kRunoffSelectedReaches(selectedIDs = c(55000,57800,61000,62200,78200,79400))) # Comment: Enregistrement de un ou plusieurs reachs en particulier TODO: faire une liste des reachs à enregistrés
} # Comment: End of the J2K time loop before the starting of the irrigation campaign

# Runs J2K with CORMAS
print('Ongoing J2K with CORMAS simulation...')
simuProgress <- txtProgressBar(min = 1, max =as.numeric(difftime(dateEndCORMAS,dateStartCORMAS,units='days')), style = 3)
irrigatedFarmPlots <- NULL; j2KNetRain <- NULL; reachsOfCormas <- NULL; inOutCanals <- NULL; seepage <- NULL # Comment: Variable de stockage des sorties couplées
for (i in 1:as.numeric(difftime(dateEndCORMAS,dateStartCORMAS,units='days'))){ # Comment: Boucle temporelle ocouplée
  setTxtProgressBar(simuProgress, i)
  
  reach_Runoff = j2kGetValuesAllReachs(attributes = "Runoff", 
                                       ids = cormasReachIds$idReach) %>% as_tibble() # Comment: Getting reach flow from J2K
  reach_Runoff = j2kGetValuesAllReachs(attributes = "Runoff") %>% as_tibble() 
  
  updatedFlows <- cormasReachIds %>% 
    mutate(ID = idReach) %>%
    full_join(reach_Runoff, by = "ID") %>%
    mutate(q = (Runoff / 1000) / (24 * 3600)) %>%
    mutate(q = replace_na(q,0))
  r <- setAttributesOfEntities("q", "RiverReach", updatedFlows$id, updatedFlows$q) # Comment: Mise à jour des valeurs de débits dans les entites RiverReach de CORMAS
  reachsOfCormas <- reachsOfCormas %>% rbind(updatedFlows %>% mutate(date = i)) # Comment: Enregistrement des débits de CORMAS
  
  actMPS = j2kGetOneValueAllHrus("actMPS") %>% as_tibble() # Comment: Getting actMPS value from HRUs
  updatedActMPS <- cormasFarmPlotIds %>%
    mutate(ID = idParcel) %>%
    full_join(actMPS, by = "ID") %>%
    mutate(actMPS = actMPS / 1000) %>%
    mutate(actMPS = replace_na(actMPS,0))
  r <- setAttributesOfEntities("actMPS", "FarmPlot", updatedActMPS$id, updatedActMPS$actMPS) # Comment: Mise à jour des valeurs de actMPS dans les entites FarmPlot de CORMAS
  
  maxMPS = j2kGetOneValueAllHrus("maxMPS") %>% as_tibble() # Comment: Getting actMPS value from HRUs
  updatedMaxMPS <- cormasFarmPlotIds %>%
    mutate(ID = idParcel) %>%
    full_join(maxMPS, by = "ID") %>%
    mutate(maxMPS = maxMPS / 1000) %>%
    mutate(maxMPS = replace_na(maxMPS,0))
  r <- setAttributesOfEntities("maxMPS", "FarmPlot", updatedMaxMPS$id, updatedMaxMPS$maxMPS)
  
  actET = j2kGetOneValueAllHrus("etact") %>% as_tibble() # Comment: Getting actMPS value from HRUs
  updatedActET <- cormasFarmPlotIds %>%
    mutate(ID = idParcel) %>%
    full_join(actET, by = "ID") %>%
    mutate(actET = (actET / 1000) / (24 * 3600)) %>%
    mutate(actET = replace_na(actET,0))
  r <- setAttributesOfEntities("actET", "FarmPlot", updatedActET$id, updatedActET$actET)
  
  
  maxET = j2kGetOneValueAllHrus("etpot") %>% as_tibble() # Comment: Getting actMPS value from HRUs
  updatedMaxET <- cormasFarmPlotIds %>%
    mutate(ID = idParcel) %>%
    full_join(maxET, by = "ID") %>%
    mutate(maxET = (maxET / 1000) / (24 * 3600)) %>%
    mutate(maxET = replace_na(maxET,0))
  r <- setAttributesOfEntities("maxET", "FarmPlot", updatedMaxET$id, updatedMaxET$maxET)
  
  #TODO: trop lourd trouvez une alternative pour affecter des pluies spatialisées?
  #rainOnParcells <- j2kGetValuesAllHrus("rain", cormasParcelIds$idParcel) %>% as_tibble() # Comment: Getting rain from J2K HRU-plot
  #r <- setAttributesOfEntities("rain", "FarmPlot", rainOnParcells$ID, rainOnParcells$rain) # Comment: Mise à jour des valeurs de pluie dans les entites FarmPlot de CORMAS 
  
  r <- runSimu(duration = 24) # Comment: Run CORMAS model during 24 time step
  
  surfaceIrri <- getAttributesOfEntities("jamsWaterBuffer", "FarmPlot") %>% # Comment: Récupération de l'eau d'irrigation gravitaire
    mutate(irriDoseInLitres = jamsWaterBuffer * 1000) %>% # Comment: Conversion m3 -> L
    mutate(id =  as.numeric(as.character(id))) %>%
    filter(irriDoseInLitres > 0) %>%
    mutate(date = i) %>% as_tibble()
  surfaceIrri <- surfaceIrri %>% arrange(id) # Comment: Reset CORMAS buffers
  r <- setAttributesOfEntities("jamsWaterBuffer", "FarmPlot",
                               surfaceIrri$id, vector("numeric", length(surfaceIrri$id)))
  irrigatedFarmPlots <- irrigatedFarmPlots %>%  rbind(surfaceIrri) # Comment: Enregistrement des irrigations gravitaires
  j2kSet("surface",   # Comment: Set gravity irrigation of corresponding HRU-plots
         surfaceIrri$id, 
         surfaceIrri$irriDoseInLitres) 
  
  aspersionIrri <- getAttributesOfEntities("jamsAspWaterBuffer", "FarmPlot") # Comment: Récupération de l'eau d'irrigation sous pression (NB: on fait l'hypothèse qu'elle est utilisée en aspersion et non en goutte-à-goutte)
  aspersionIrri %>% mutate(aspIrriDoseInLitres = aspersionIrri * 1000) %>% # Comment: Conversion m3 -> L
    mutate(id =  as.numeric(as.character(id))) %>%
    filter(aspIrriDoseInLitres > 0) %>%
    mutate(date = i) %>% as_tibble()
  aspersionIrri <- aspersionIrri %>% arrange(id) # Comment: Reset CORMAS buffers
  r <- setAttributesOfEntities("jamsAspWaterBuffer", "FarmPlot",
                               aspersionIrri$id, vector("numeric", length(aspersionIrri$id)))
  irrigatedFarmPlots <- irrigatedFarmPlots %>%  rbind(aspersionIrri) # Comment: Enregistrement des irrigations par aspersion
  j2kSet("aspersion",   # Comment: Set pressure irrigation of corresponding HRU-plots
         aspersionIrri$id, 
         aspersionIrri$irriDoseInLitres)
  
  qFromIntakes <- getAttributesOfEntities("jamsWaterBuffer", "EwaterIntake") %>%  # Comment: Take water from the reach of the intake...
    mutate(id =  as.numeric(as.character(id))) %>%
    mutate(q = jamsWaterBuffer * 1000) %>% arrange(id)
  qFromReservoirs <- getAttributesOfEntities("jamsWaterBuffer", "Ereservoir") %>%  # ... or of the reservoirs
    mutate(id =  as.numeric(as.character(id))) %>%
    mutate(q = jamsWaterBuffer * 1000) %>% arrange(id)
  qFromReleases <- getAttributesOfEntities("jamsWaterBuffer", "EwaterRelease") %>% #  ... and release it to the reach of the release.
    mutate(id =  as.numeric(as.character(id))) %>%
    mutate(q = jamsWaterBuffer * 1000) %>% arrange(id)
  r <- setAttributesOfEntities("jamsWaterBuffer", "EwaterIntake", # Comment: Reset CORMAS buffers
                               qFromIntakes$id, vector("numeric", length(qFromIntakes$id))) 
  r <- setAttributesOfEntities("jamsWaterBuffer", "Ereservoir", 
                               qFromReservoirs$id, vector("numeric", length(qFromReservoirs$id)))
  r <- setAttributesOfEntities("jamsWaterBuffer", "EwaterRelease",
                               qFromReleases$id,  vector("numeric", length(qFromReleases$id))) 
  #TODO: intégrer l'eau qui alimente les reservoirs (inWater) depuis les reachs pour les ASAs sous pression
  qFromIntakes <- qFromIntakes %>% # Comment: Build table q exchanges from reachs to canals to reachs back or to hruplots (other than irrigation)
    left_join(getAttributesOfEntities("myReachId", "EwaterIntake") %>% mutate(id =  as.numeric(as.character(id))), by="id" )
  qFromReleases <- qFromReleases %>%  
    left_join(getAttributesOfEntities("myReachId", "EwaterRelease") %>%   mutate(id =  as.numeric(as.character(id))), by="id" )
  qFromReleases <- qFromReleases %>% 
    left_join(getAttributesOfEntities("idHRU", "EwaterRelease") %>%  mutate(id =  as.numeric(as.character(id))), by="id" )
  qOfTheDay <- qFromIntakes %>% 
    select(-jamsWaterBuffer,-id) %>%
    mutate(waterIn = T) %>%
    mutate(idHRU = 0) %>%
    dplyr::union(qFromReleases %>%
                   select(-jamsWaterBuffer,-id) %>%
                   mutate(waterIn = F), by= c("id", "waterIn")) %>%  mutate(date = i)
  inOutCanals <- inOutCanals %>% rbind(qOfTheDay) # Comment: Enregistrement des entrées et sorties des canaux
  qsIn <- qOfTheDay %>% filter(waterIn)
  j2kSet("reachout", qsIn$idReach, qsIn$q) # Comment: Set outflow from reachs at waterIntakes
  qsOut <- qOfTheDay %>% filter(!waterIn)
  j2kSet("surface", qsOut$idHRU, qsOut$q) # Comment: Set the flood in J2K (we consider that the hru is "flooded")
  
  qFromSeepage <- getAttributesOfEntities("jamsWaterBuffer", "SpatialPlace") %>% # Comment: Get the water seepage from canals and put it in the rigth HRUS
    mutate(id =  as.numeric(as.character(id))) %>%
    right_join(spatialPlacesWithCanals, by ="id") %>%
    filter(jamsWaterBuffer > 0) %>%
    mutate(q = jamsWaterBuffer * 1000) %>%
    arrange(id) %>%
    filter(idHRU > 0) %>% as_tibble()
  r <- setAttributesOfEntities("jamsWaterBuffer", "SpatialPlace", # Comment: Reset CORMAS buffer
                               qFromSeepage$id,
                               vector("numeric", length(qFromSeepage$id)))
  seepage <- qFromSeepage %>% # Comment: Enregistrement des pertes par infiltration le long des canaux (seepages)
    group_by(canalsId, idHRU) %>%
    summarise(q = sum(q)) %>% 
    mutate(date = i) %>% rbind(seepage)
  qFromSeepage <- qFromSeepage %>% # Comment: Get hru Ids of spatial place
    group_by(idHRU) %>%
    summarise(q = sum(q))
  j2kSet("surface",        # Comment: Set seepages in J2K (we consider seepage having the same effect as "surface" irrigation)
         qFromSeepage$idHRU, 
         qFromSeepage$q)
  
  j2kMakeStep() # Comment: Run new J2K daily step
  inOutWater <- rbind(inOutWater, j2kWaterBalanceFlows()) # Comment: Enregistrement du CatchmentRunoff et somme des precipitations, ETR et T°C sur l'ensemble des HRUs, il existe aussi une fonction pour sélectionner uniquement certaines HRUs (voir dans Rj2k.R)
} # End of J2K-CORMAS time loop

# Runs J2K until the end of simulation period 
print('J2K simulation finishing...')
simuProgress <- txtProgressBar(min = 1, max =as.numeric(difftime(dateEndJ2K,dateEndCORMAS,units='days')), style = 3) # Comment: Barre de progression
for (i in 1:as.numeric(difftime(dateEndJ2K,dateEndCORMAS,units='days'))){ # Comment: Boucle temporelle
  setTxtProgressBar(simuProgress, i)
  j2kMakeStep() 
  inOutWater <- rbind(inOutWater, j2kWaterBalanceFlows()) # Comment: Enregistrement du CatchmentRunoff et somme des precipitations, ETR et T°C sur l'ensemble des HRUs, il existe aussi une fonction pour sélectionner uniquement certaines HRUs (voir dans Rj2k.R)
} # Comment: End of the J2K time loop after the end of the irrigation campaign

# Glimpse to outputs
cat('\n','Glimpse to output file:','\n')
glimpse(inOutWater) 

# Save outputs
if (saveOutputs) {
  write.csv(inOutWater, paste0(file.path(script.dirname, 'output/'),caseName,'_inOutWater.csv'), row.names = F)
  # write.csv(runoffSelectedReaches, paste0(file.path(script.dirname, 'output/'),caseName,'_runoffSelectedReaches.csv'), row.names = F)
}

# Ending airJ2KCormasCoupler
j2kStop(); killJ2K()
endTime = Sys.time(); simuTime = endTime - startTime; cat ('................................................................',
                                                           '\n','Simulation time is ', round(simuTime,2), 'minutes', '\n')