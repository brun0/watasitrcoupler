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

genJsonDict <- function(names, values) {
    result = "{\n"
    args <- mapply(genDictElement, names, values)
    result <- paste0(result, paste0(c(args), collapse=",\n"))
    result <- paste0(result, "\n}")
    return(result)
}

j2kDictListToDataframe <- function(str) {
    js = fromJSON(str)
    js = lapply(js, function(x) {
        x[sapply(x, is.null)] <- NA
        unlist(x)
    })
    df = do.call("rbind", js)
    return(df);
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
j2kSet <- function(what, keys, values, ip="localhost", port="9999") {
    dict = genJsonDict(keys, values)
    res = askJ2K(c("command", "key", "value"), c("set", what, dict), ip, port)
    res = paste("[J2K]", res[[2]])
    return(res)
}

# get values for all hrus or all reachs
# what can be "hru" or "reach"
j2kGet <- function(what, ip="localhost", port="9999") {
    res = askJ2K(c("command", "key"), c("get", what), ip, port)
    df = j2kDictListToDataframe(res[[2]])
    return(df)
}

# get one attribute for all hrus
# what can be "netrain", "etpot"...
j2kGetOneValueAllHrus <- function(what, ip="localhost", port="9999") {
    res = askJ2K(c("command", "key"), c("getHru", what), ip, port)
    if (startsWith(res[[2]], '[')) {
        df = j2kDictListToDataframe(res[[2]])
        return(df)
    }
    else {
        cat(res[[2]], '\n')
        return(NULL)
    }
}

# get one attribute for all reachs
# what can be "actRD1", "Runoff"...
j2kGetOneValueAllReachs <- function(what, ip="localhost", port="9999") {
    res = askJ2K(c("command", "key"), c("getReach", what), ip, port)
    if (startsWith(res[[2]], '[')) {
        df = j2kDictListToDataframe(res[[2]])
        return(df)
    }
    else {
        cat(res[[2]], '\n')
        return(NULL)
    }
}

# get HRU bilan
j2kBilan <- function(ip="localhost", port="9999") {
    res = askJ2K(c("command"), c("bilan"), ip, port)
    return(as.numeric(res[[2]]))
}

############## UTILS ###############

killJ2K <- function() {
    system2('kill', args=c('-9', "$(ps aux | grep -i 'jams-starter' | grep java | awk '{print $2}')"), wait=F)
}