library(httr)
library(xml2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
library(RJSONIO)

genDictElement <- function(name, value) {
    return(paste0('"', name, '": "', value, '"'))
}

genJsonDict <- function(names, values) {
    result = "{\n"
    args <- mapply(genDictElement, names, values)
    result <- paste0(result, paste0(c(args), collapse=",\n"))
    result <- paste0(result, "\n}")
    return(result)
}

askJ2K <- function(argNames = c(), argValues = c(), ip, port) {
    payload <- genJsonDict(argNames, argValues)
    result <- POST(paste0("http://", ip, ":", port), body = payload)
    return(list(payload, content(result)))
}

j2kMakeStep <- function(nbStep=1, ip="localhost", port="9999") {
    askJ2K(c("command", "nbStep"), c("step", nbStep), ip, port)
}

j2kFree <- function(ip="localhost", port="9999") {
    askJ2K(c("command"), c("free"), ip, port)
}

# what parameter can be infiltration, aspersion, drip, surface (keys are HRU ids)
# and also reachin, reachout (keys are reach ids)
j2kSet <- function(what, keys, values, ip="localhost", port="9999") {
    dict = genJsonDict(keys, values)
    askJ2K(c("command", "key", "value"), c("set", what, dict), ip, port)
}

# what can be "hru" or "reach"
j2kGet <- function(what, ip="localhost", port="9999") {
    res = askJ2K(c("command", "key"), c("get", what), ip, port)
    df = j2kDictListToDataframe(res[[2]])
    return(df)
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