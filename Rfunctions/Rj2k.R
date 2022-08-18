library(httr)
library(xml2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
library(RJSONIO)

genDictElement <- function(name, value) {
    strValue = as.character(value)
    if (startsWith(strValue, '[') || startsWith(strValue, '{')) {
        return(paste0('"', name, '": ', value))
    }
    else {
        return(paste0('"', name, '": "', value, '"'))
    }
}

genTabElement <- function(value) {
  strValue = as.character(value)
  if (startsWith(strValue, '[') || startsWith(strValue, '{')) {
    return(paste0('"', value))
  }
  else {
    return(paste0('"', value, '"'))
  }
}

genJsonDict <- function(names, values) {
    result = "{\n"
    args <- mapply(genDictElement, names, values)
    result <- paste0(result, paste0(c(args), collapse=",\n"))
    result <- paste0(result, "\n}")
    return(result)
}

genJsonTbl <- function(values) {
  result = "[\n"
  args <- mapply(genTabElement, values)
  result <- paste0(result, paste0(c(args), collapse=",\n"))
  result <- paste0(result, "\n]")
  return(result)
}

j2kDictListToDataframe <- function(str) {
    js = fromJSON(str)
    js = lapply(js, function(x) {
        x[sapply(x, is.null)] <- NA
        unlist(x)
    })
    df = do.call("rbind", js) %>%
      as.data.frame(stringsAsFactors = F) %>% 
      mutate_all(as.numeric)
    return(df);
}

j2kDictToDataframe <- function(str) {
  js = fromJSON(str)
  return(t(js) %>% as.data.frame())
}

askJ2K <- function(argNames = c(), argValues = c(), ip, port) {
    payload <- genJsonDict(argNames, argValues)
    result <- POST(paste0("http://", ip, ":", port), body = payload)
    return(list(payload, content(result, encoding='UTF-8')))
}

##################### Here are the end user functions #######################

# tell j2k to make N steps
j2kMakeStep <- function(nbStep=1, ip="localhost", port="9999") {
    res = askJ2K(c("command", "nbStep"), c("step", nbStep), ip, port)
    res = paste("[J2K]", res[[2]])
    return(res)
}

# free j2k model so it runs until the end
j2kFree <- function(ip="localhost", port="9999") {
    res = askJ2K(c("command"), c("free"), ip, port)
    res = paste("[J2K]", res[[2]])
    return(res)
}

# free j2k model so it runs until the end
j2kStop <- function(ip="localhost", port="9999") {
    res = askJ2K(c("command"), c("stop"), ip, port)
    res = paste("[J2K]", res[[2]])
    return(res)
}

# set values for all hrus or all reach
# what parameter can be infiltration, aspersion, drip, surface (keys are HRU ids)
# and also reachin, reachout (keys are reach ids)
# !! all values need to be in litres !!
j2kSet <- function(what, keys, values, ip="localhost", port="9999") {
    if (length(keys) == 0) return("no value to set")
    dict = genJsonDict(keys, values)
    res = askJ2K(c("command", "key", "value"), c("set", what, dict), ip, port)
    res = paste("[J2K]", res[[2]])
    return(res)
}

# get values for all hrus or all reachs
# what can be "hru" or "reach"
#TODO: Attention chez moi cette fonction ne renvoie qu'une valeur
# (la colonne s'appelle actRD1 pour les reach par exemple)
# Maintenant qu'on a des fonctions pour avoir les valeurs une par une
# je propose que cette fonction renvoie toutes les variables pour hru ou reach
# selon ce que l'on choisit.

j2kGet <- function(what, ip="localhost", port="9999") {
    res = askJ2K(c("command", "key"), c("get", what), ip, port)
    df = j2kDictListToDataframe(res[[2]])
    return(df)
}

# get attributes for all hrus
# what can be "netrain", "etpot"...

j2kGetValuesAllHrus <- function(attributes, ids = NULL, ip="localhost", port="9999") {
    res = askJ2K(c("command", "keys", "ids"), c("getHru", genJsonTbl(attributes), genJsonTbl(ids)), ip, port)
    if (startsWith(res[[2]], '[')) {
        df = j2kDictListToDataframe(res[[2]])
        return(df)
    }
    else {
        cat(res[[2]], '\n')
        return(NULL)
    }
}

# For backwards compatibility..
j2kGetOneValueAllHrus <- function(attribute, ids = NULL, ip="localhost", port="9999") {
  return(j2kGetValuesAllHrus(attributes = attribute, ids, ip="localhost", port="9999"))
}

# Get values of attributes sumed on all HRUS
j2kSumedValuesAllHrus <- function(attributes, ip="localhost", port="9999") {
  res = askJ2K(c("command", "keys"), c("getHruSum", genJsonTbl(attributes)), ip, port)
  if (startsWith(res[[2]], '{')) {
    df = j2kDictToDataframe(res[[2]])
    return(df)
  } else {
    cat(res[[2]], '\n')
    return(NULL)
  }
}

# get one attribute for all reachs
# what can be "actRD1", "Runoff"...
j2kGetValuesAllReachs <- function(attributes, ids = NULL, ip="localhost", port="9999") {
  res = askJ2K(c("command", "keys", "ids"), c("getReach", genJsonTbl(attributes), genJsonTbl(ids)), ip, port)
  if (startsWith(res[[2]], '[')) {
    df = j2kDictListToDataframe(res[[2]])
    return(df)
  } else {
    cat(res[[2]], '\n')
    return(NULL)
  }
}

# For backwards compatibility..
j2kGetOneValueAllReachs <- function(attribute, ids = NULL, ip="localhost", port="9999") {
  return(j2kGetValuesAllReachs(attributes = attribute, ids, ip="localhost", port="9999"))
}

# Get values of attributes sumed on all HRUS
j2kSumedValuesAllReachs <- function(attributes, ip="localhost", port="9999") {
  res = askJ2K(c("command", "keys"), c("getReachSum", genJsonTbl(attributes)), ip, port)
  if (startsWith(res[[2]], '{')) {
    df = j2kDictToDataframe(res[[2]])
    return(df)
  } else {
    cat(res[[2]], '\n')
    return(NULL)
  }
}

# get aggregated values for water balance
j2kWaterBalanceStorages <- function(ip="localhost", port="9999") {
  hruStorage <- sum(j2kSumedValuesAllHrus(c("actMPS", "actLPS", "actDPS","TotSWE", "actRG1", "actRG2", "storedInterceptedWater"))) #%>% 
  reachStorage <- sum(j2kSumedValuesAllReachs(c("actRD1", "actRD2", "actRG1",  "actRG2")))
  return(data.frame(hruStorage, reachStorage))
}

j2kWaterBalanceFlows <- function(ip="localhost", port="9999") {
  res = askJ2K(c("command"), c("getCatchmentRunoff"), ip, port)
  runoff <- as.numeric(res[[2]])
  hrusInOut <- j2kSumedValuesAllHrus(c("rain","snow","etact"))
  return(data.frame(runoff, hrusInOut))
}

# get aggregated values for LOCAL water balance Only for HRUS at the head of a basin
j2kLocalWaterBalanceStorages <- function(ip="localhost", port="9999", selectedHrus) {
  hruStorage <- sum(j2kGetValuesAllHrus(attributes = c("actMPS", "actLPS", "actDPS", "TotSWE", "storedInterceptedWater",
                                                       "actRG1", "actRG2"),
                    ids = selectedHrus))
  return(data.frame(hruStorage))
}

j2kLocalWaterBalanceFlows <- function(ip="localhost", port="9999", selectedHrus, lastHru) {
  outflow = sum(j2kGetValuesAllHrus(attributes =c("RD1OUT","RD2OUT","RG1OUT","RG2OUT"), ids = lastHru) %>% select(-ID))
  hrusInOut <-t(colSums(j2kGetValuesAllHrus(attributes =c("rain","snow","etact"), ids = selectedHrus) %>% select(-ID)))
  return(data.frame(cbind(outflow,hrusInOut)))
}




############## UTILS ###############

killJ2K <- function() {
    system2('kill', args=c('-9', "$(ps aux | grep -i 'jams-starter' | grep java | awk '{print $2}')"), wait=F)
}

initJ2K <- function(jamsRootPath, jams_file_name, stdoutP, stderrP, wd) {
  setwd(jamsRootPath)
  system2(
    'java',
    args=c('-jar', 'jams-starter.jar', '-m', paste0('data/J2K_cowat/',jams_file_name), '-n'),
    wait=F, stdout=stdoutP, stderr=stderrP
  )
  cat('\n', 'Waiting 5 seconds to make sure J2K coupling module starts listening...','\n')
  Sys.sleep(5)
  setwd(wd)
}

connectCORMAS <- function(modelName, parcelFile, cormasRootPath, stdoutP, stderrP, wd) {
  setwd(cormasRootPath)
  if (!isCormasListening()) { # Open Cormas listenning for instruction
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
}


initCORMAS <- function(init, control, modelName, probesList) {
  r <- setInit(init) # Init,  note that the model is not initialized, we just set the init method..
  r <- setStep(paste0(control)) # Stepper
  #Choose probes to activate
  for (p in 1:length(probesList)) {
  r <- activateProbe(probesList[p], modelName)
  }
  # initialize the model
  r <- initSimu() 
}

# manageIDs <- function() {
  # cormasParcelIds <- getAttributesOfEntities("idParcel","FarmPlot") %>%  as_tibble()
  # cormasSpatialPlaceIds <- getAttributesOfEntities("idReach","SpatialPlace") %>% as_tibble()
  # # correctReachIds <- read.table("superjams/data/J2K_cowat/parameter/step2_streams_new_div_OK2.asc",
  # correctReachIds <- read.table("superjams/data/J2K_cowat/parameter/step2_streams_new_div2.asc",
  #                               sep = " ",
  #                               dec = ".",
  #                               skip = 6) %>%
  #   mutate(line = row_number()) %>%
  #   gather("col", "j2kID", -line) %>% 
  #   mutate(col = as.numeric(str_remove(col,"V"))) %>%
  #   arrange(line, col) %>%
  #   mutate(cormasId = row_number() - 1) %>% #JE NE SAIS PAS POURQUOI il y a un décalage de 1..!
  #   filter(j2kID != 0) %>%
  #   as_tibble() %>% 
  #   full_join(cormasSpatialPlaceIds %>% 
  #   mutate(cormasId = as.numeric(as.character(id)))) %>%
  #   arrange(cormasId) %>%
  #   mutate(j2kID = replace_na(j2kID,0))
  # 
  # r <- setAttributesOfEntities("idReach",
  #                              "SpatialPlace", 
  #                              correctReachIds$cormasId, 
  #                              correctReachIds$j2kID)
  
  # cormasReachIds <- getAttributesOfEntities("idReach","RiverReach") %>% as_tibble()
  
  # spatialPlacesWithCanals <- getAttributesOfEntities("canalsId","SpatialPlace") %>% as_tibble() %>%
  #   filter(canalsId > 0) 
  # 
  # spatialPlacesWithCanals <- spatialPlacesWithCanals %>% 
  #   left_join(getAttributesOfEntities("idHRU","SpatialPlace"), by = "id") %>%
  #   mutate(id = as.numeric(as.character(id))) %>%
  #   arrange(id)
# }