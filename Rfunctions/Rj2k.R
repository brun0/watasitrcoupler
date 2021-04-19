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


############## UTILS ###############

killJ2K <- function() {
    system2('kill', args=c('-9', "$(ps aux | grep -i 'jams-starter' | grep java | awk '{print $2}')"), wait=F)
}