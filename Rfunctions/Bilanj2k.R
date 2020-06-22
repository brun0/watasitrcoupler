# get HRU bilan
j2kWaterStorage <- function() {
  # Eau dans les HRUs (en L)
  ## Eau dans les compartiments du sol
  mps <- sum(as.numeric(j2kGetOneValueAllHrus("actMPS")[,2]))
  lps <- sum(as.numeric(j2kGetOneValueAllHrus("actLPS")[,2]))
  dps <- sum(as.numeric(j2kGetOneValueAllHrus("actDPS")[,2]))
  ## Eau sous forme neigeuse stoquée sur les HRUs
  ## (J'ai l'impression que c'est la bonne variable à prendre) (en L)
  storedSnow <- sum(as.numeric(j2kGetOneValueAllHrus("TotSWE")[,2]))
  ## Eau ou neige bloquée dans le couvert (en L)
  intercStorage <- sum(as.numeric(j2kGetOneValueAllHrus("storedInterceptedWater")[,2]))
  ## Ean dans les napes (en L)
  rg1 <- sum(as.numeric(j2kGetOneValueAllHrus("actRG1")[,2]))
  rg2 <- sum(as.numeric(j2kGetOneValueAllHrus("actRG2")[,2]))
  # Eau dans les reachs (en L)
  ## Artificiellement répartie en 4 stocks pour tracer la provenance
  ## J'ai bien vérifié dans les sources qu'il s'agit bien de l'eau qui reste dans la rivière
  ## (le reste a coulé dans le brin suivant pendant la journée)
  ## NB: il y a un stock que je ne comprends pas qui s'appelle "addInAct"
  ## Description dans le code java: "additional inflow storage inside reach"
  ## Ce stock est relié à aucune variable dans le fichier jams que j'ai et fixé à une valeur 0
  reachRD1 <- j2kGetOneValueAllReachs("actRD1") %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    mutate_all(as.numeric) %>%
    filter(ID != 9999) %>%
    select(actRD1) %>% 
    pull() %>%
    sum()
  reachRD2 <- j2kGetOneValueAllReachs("actRD2") %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    mutate_all(as.numeric) %>%
    filter(ID != 9999) %>%
    select(actRD2) %>% 
    pull() %>%
    sum()
  reachRG1 <- j2kGetOneValueAllReachs("actRG1") %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    mutate_all(as.numeric) %>%
    filter(ID != 9999) %>%
    select(actRG1) %>% 
    pull() %>%
    sum()
  reachRG2 <- j2kGetOneValueAllReachs("actRG2") %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    mutate_all(as.numeric) %>%
    filter(ID != 9999) %>%
    select(actRG2) %>% 
    pull() %>%
    sum()
  #reachAdditional <- sum(as.numeric(j2kGetOneValueAllReachs("actAddIn")[,2]))
  # À ajouter les stocks présents dans les Dams!
  res <- data.frame(mps,lps,dps,storedSnow,intercStorage,rg1,rg2,reachRD1,reachRD2,reachRG1,reachRG2)
  return(res)
}

# Find catchment outlet assuming there are no dams nor lakes and assuming there is 
# only one outlet
catchmentOutletID <- function(modelFolder = "J2K_cowat") {
  reachParFile <- reachTopologyFileName
  reachsFile <- file.path(jamsRootPath, "data", modelFolder, "parameter",reachParFile)
  if(!file.exists(reachsFile)) {
    stop(paste("RUN STOPPED: file ",
      reachParFile, 
      "notFound in dir",
      file.path(jamsRootPath, "data", modelFolder, "parameter")))}
  reachsTable <- read.table(reachsFile, 
                            header = T, 
                            sep="\t", 
                            dec = ".",
                            skip = 4) %>%
    tbl_df() 
   colnames(reachsTable) <- c("ID",	"to.reach",	"length",	"slope",	"sinuosity",	"rough",	"width")
   return(reachsTable %>% 
            filter(to.reach == 9999) %>%
            select(ID) %>%
            pull() %>% 
            first())
}

# get HRU bilan
j2kInOutWater <- function() {
  # Eau entrant dans les HRUs (en L)
  rain <- sum(as.numeric(j2kGetOneValueAllHrus("rain")[,2]))
  snow <- sum(as.numeric(j2kGetOneValueAllHrus("snow")[,2]))
  # Eau sortant des HRUs
  eTR <- sum(as.numeric(j2kGetOneValueAllHrus("etact")[,2]))
  catchmentOutlet <- catchmentOutletID()
  # Eau sortant des reachs
  ## Artificiellement répartie en 4 stocks pour tracer la provenance
  ## J'ai bien vérifié dans les sources qu'il s'agit bien de l'eau qui 
  ## sort des reachs qui n'ont pas de destination (ni autre reach ni dam)
  ## NB1: Toujours cette variable addIn au niveau du catchement fixé à 0 dans les fichiers Jams
  ## NB2: Étrangement la variable RG2 du catchement n'est reliée à rien dans le fichier jams
  ## NB3: la variable CatchmentRunoff semble représenter la somme des écoulements 
  ## (Variable du contextetimeLoop)
  outRD1 <- j2kGetOneValueAllReachs("outRD1") %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    mutate_all(as.numeric) %>%
    filter(ID == catchmentOutlet) %>%
    select(outRD1) %>% 
    pull()
  outRD2 <- j2kGetOneValueAllReachs("outRD2") %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    mutate_all(as.numeric) %>%
    filter(ID == catchmentOutlet) %>%
    select(outRD2) %>% 
    pull()
  outRG1 <- j2kGetOneValueAllReachs("outRG1") %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    mutate_all(as.numeric) %>%
    filter(ID == catchmentOutlet) %>%
    select(outRG1) %>% 
    pull()
  outRunoff <- j2kGetOneValueAllReachs("Runoff") %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    mutate_all(as.numeric) %>%
    filter(ID == catchmentOutlet) %>%
    select(Runoff) %>% 
    pull()
  #outAdditionnal <- sum(as.numeric(j2kGetOneValueAllReachs("catchmentaddIn")[,2]))
  # À ajouter les transferts inter-bassins éventuels, notamment via les barrages!
  res <- data.frame(rain, snow, eTR, outRD1, outRD2, outRG1, outRunoff)
  return(res)
}

plotBalance <- function(storages, inOut, graphName = "water-balance") {
  #storages <- storedWater %>% tbl_df()
  #inOut <- inOutWater %>% tbl_df()
  waterSummary <- cbind (storages, inOut) %>% tbl_df() %>% mutate(day = row_number())
  waterSummary <- waterSummary %>% mutate(storage = mps + lps + dps + storedSnow + intercStorage + 
                      rg1 + rg2 +
                      reachRD1 + reachRD2 + reachRG1 + reachRG2) %>%
    mutate(inWater = rain + snow) %>%
    mutate(outFlow = outRunoff) %>%
    mutate(outET = eTR) %>%
    mutate(balance = inWater - outFlow - outET) %>%
    arrange(day) %>%
    mutate(storageNextDay = lead(storage)) %>%
    mutate(massConservation = storageNextDay - (storage + balance)) %>%
    mutate(deltaStock = storageNextDay - storage)
  
  WS <- waterSummary %>%
    mutate(cumRain = cumsum(rain)) %>%
    mutate(cumSnow = cumsum(snow)) %>%
    mutate(cumInflow = cumRain + cumSnow) %>%
    mutate(soilStorage =  mps + lps + dps + intercStorage) %>%
    mutate(snowStorage = storedSnow) %>%
    mutate(groundWaterStorage =  rg1 + rg2) %>%
    mutate(reachStorage =  reachRD1 + reachRD2 + reachRG1 + reachRG2) %>%
    mutate(cumETR = cumsum(outET)) %>%
    mutate(cumoutFlow = cumsum(outRunoff)) %>% 
    mutate(P = rain + snow)
  
  WSBilan <- WS %>%
    select(P, deltaStock,eTR,outRunoff, day) %>%
    mutate(PmoinsETMoinsD = P - eTR - outRunoff) %>%
    gather("Variable", "VolInL", -day)
  
  WSBilan %>% 
    ggplot() +
    geom_line(aes(color=Variable, x= day, y=VolInL), 
              position = position_jitter(height=0.5e+9)) +
    coord_cartesian(ylim=c(-2e+10, 4e+10)) +
    ggsave(paste0(graphName, "-", 1, ".pdf"), height = 13, width= 19, units ="cm")
  
  WS %>%
    select(rain, 
           snow, 
           snowStorage, 
           soilStorage, 
           groundWaterStorage,
           eTR,
           outFlow,
           reachStorage,
           day) %>%
    gather("Variable", "VolInL", -day) %>%
    ggplot() +
    geom_line(aes(color=Variable, x= day, y=VolInL)) +
    #geom_point(aes(shape=Variable, x= day, y=VolInL)) +
    coord_cartesian(ylim=c(0, 1.5e+11)) +
    ggsave(paste0(graphName, "-hydro-vars.pdf"), height = 13, width= 19, units ="cm")
  
}
