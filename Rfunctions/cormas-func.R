
# Prerequisites -----------------------------------------------------------


# Some parcels needs to be loaded in Cormas:
# copy the Add-ons folder in the cormas folder
# Then load all the parcels below using tools>add-ons>add-on-manager

# Opentalk-HTTP
# Opentalk-Tools-Basics
# Opentalk-Tools-Console
# Opentalk-Tools-Monitor
# CormasWS-runSimu.st
# CormasWS-sayHello.st
# CormasWS-setAttributeofClassvalue.st
# DataTransfR.st
# Root.Smalltalk.CormasNS.Kernel.Entity-rTransfer.st

#Then Cormas needs to be open

#Then you need open the R cormas connection:
#Simulation>analysis>R cormas>start webservice


# Load libraries ----------------------------------------------------------


library(httr)
library(xml2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)


# REQUEST AND FORMATING FUNCTIONS -----------------------------------------


# Function in this section are only used in this file to define R-Cormas users functions


# Global variables --------------------------------------------------------


ADDRESS <- "localhost"

requestHead <- "<SOAP-ENV:Envelope xmlns:ns=\"urn:vwservices\" xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Body>"

requestTail <- 	"</SOAP-ENV:Body> </SOAP-ENV:Envelope>"

#' formatRequest function
#'  Format a http request content that will be understood by cormasWS
#' formatRequest
#' Only used by the askCormas function
#'
#' @param functionName :  A string with an identifier of the cormas function (not the exact name)
#' @param argNames :  A collection of strings with an identifier of the arguments of the cormas function (not the exact name)
#' @param argValues :  A collection of strings of numbers with an identifier of the arguments of the cormas function (not the exact name)
#' @return : The anwser recieved from Cormas formated according to the httr package

formatRequest <- function(functionName, argNames, argValues){
request <- paste0(requestHead, "<ns:",functionName,">")
args <- mapply(includeArg,argNames,argValues)
request <- paste(request, paste(c(args),collapse=" "))
request <- paste0(request, "</ns:",functionName,">",requestTail)
return(request)
}

#' includeArg function
#' Include arguments in a request
#' Just used by formatRequest function, format the arguments in xml understood by cormas web service
#' @param name a string
#' @param value a string or number
#' @return the answer in xml understood by cormas web service

includeArg <- function(name, value){
return <- (paste0("<ns:",name,">",value,"</ns:",name,">"))
}

#' askCormas function
#' Send a http request to cormas
#' send a request to cormas using the formatRequest function
#' @param functionName a string specifying the cormas function identifier
#' @param argNames a collection of string specifying the identifiers of arguments of the function
#' @param argValues a collection of strings or numbers giving the values of the arguments
#' @return in a list the request formated in xml and the answer recieved from cormas

askCormas <- function(functionName, argNames = c(), argValues = c()) {
request <- formatRequest(functionName, argNames, argValues)
result <- POST(paste0("http://",ADDRESS,":4920/CormasWS"), body = request)
return(list(request,result))
}


# R-CORMAS USERS FUNCTIONS ------------------------------------------------
#Functions that the user of the package can use


#' opnModel function
#' Open a cormas model
#' @param modelName a string specifying the name of the cormas model (model folder)
#' @param parcelFile a string specifying name of the file to open without extension
#' @return in a list the request formated in xml and the answer recieved from cormas
#' @examples r <- openModel("ECEC") # opens cormas model

openModel <- function(modelName,parcelFile=paste0(modelName,".pcl")){
return(askCormas("LoadModelFromParcelName",
	argNames=c("modelName","parcelFileName"),
	argValues=c(modelName,parcelFile)))
}

#' setInit function
#' Choose the init function for the next simulation
#' @param initMethod a string specifying the name the init method to use
#' @return in a list the request formated in xml and the answer recieved from cormas
#' @examples r <- setInit("homogeneousEnv") #set the init method "homogeneousEnv" (for Cormas model)

setInit <- function(initMethod = "init"){
return(askCormas("SetInit",
	argNames=c("initMethod"),
	argValues=c(initMethod)))
}


#' setStep function
#' Choose the init function for the next simulation
#' @param stepMethod a string specifying the name the init method to use
#' @return in a list the request formated in xml and the answer recieved from cormas

setStep <- function(stepMethod = "step:"){
return(askCormas("SetStep",
	argNames=c("stepMethod"),
	argValues=c(stepMethod)))
}


#' initSimu function
#' Initialize the simulation
#' @return in a list the request formated in xml and the answer recieved from cormas

initSimu <- function(){
  return(askCormas("SayHello"))
}

#' runSimu function
#' Run the choosen step method in Cormas
#' @param duration an integer specifying the number of steps to run
#' @return in a list the request formated in xml and the answer recieved from cormas

runSimu <- function(duration = 100){
return(askCormas("RunSimu",
	argNames=c("duration"),
	argValues=c(duration)))
}


#' activateProbe function
#' Say to the model to save a given probe that will be recorded during the simulation
#' To use before initialing the simulation
#' @param probeName an string specifying the name of the proble
#' @param className an string specifying the name of the class
#' @return in a list the request formated in xml and the answer recieved from cormas

activateProbe <- function(probeName, className){
return(askCormas("ActivateProbeOfClass",
	argNames=c("probeName", "className"),
	argValues=c(probeName, className)))
}

#' getProbe function
#' Get the values saved for a given probe
#' To use after runing the simulation for a probe that have been saved
#' @param probeName an string specifying the name of the proble
#' @param className an string specifying the name of the class
#' @return in a list the request formated in xml and the answer recieved from cormas

getProbe <- function(probeName, className){
return(askCormas("GetProbeOfClass",
	argNames=c("probName", "className"),
	argValues=c(probeName, className)))
}


#' getNumericProbe function
#' Get the numeric values saved for a given probe
#' To use after runing the simulation for a probe that have been saved
#' @param probeName an string specifying the name of the proble
#' @param className an string specifying the name of the class
#' @return The set of values saved for the selected probe


getNumericProbe <- function(probeName, className){
 answer <- getProbe(probeName, className)[[2]]
 res <- xml_double(xml_contents(xml_find_all(content(answer),
			xpath="//*/ns:result/*")))
	return(res)
}

#' setNumericAttributeValue function
#' Set a numeric parameter value (same value for all instances of a class)
#' To use at the begining of a simulation
#' @param attributeName an string specifying the name of the attribute
#' @param className an string specifying the class of the attribute
#' @param value a number specifying the value
#' @return in a list the request formated in xml and the answer recieved from cormas

setNumericAttributeValue <- function(attributeName, className, value){
return(askCormas("SetAttributeOfClassValue",
	argNames=c("attName", "className", "value"),
	argValues=c(attributeName, className, value)))
}

#' setStringAttributeValue function
#' Set a string value (same value for all instances of a class)
#' To use at the begining of a simulation
#' @param attributeName an string specifying the name of the attribute
#' @param className an string specifying the class of the attribute
#' @param value a string specifying the value
#' @return in a list the request formated in xml and the answer recieved from cormas

setStringAttributeValue <- function(attributeName, className, value){
return(askCormas("SetStringAttributeOfClassValue",
	argNames=c("attName", "className", "value"),
	argValues=c(attributeName, className, value)))
}

#' setAttributesOfEntities function
#' Set numeric or string atrribute values for all entities of a class
#' To use atanytime in a simulation
#' @param attributeName an string specifying the name of the attribute
#' @param className an string specifying the class of the attribute
#' @param entitiesIds a collection of number specifying the ids of all entities of the class
#' @param values a collection of numbers or string specifying the values to set
#' @return the answer recieved from cormas as text (shold be "done" if it worked)

setAttributesOfEntities <- function(attributeName, className, entitiesIds, values){
  #This method uses a special feature of the method CormasWS>>setAttribute:ofClass:value:
  #designed to set values of attribute "attributeName" of all entities of class "className"
  answer <- askCormas("SetAttributeOfClassValue",
                   argNames = c("attName", "className", "value"),
                   argValues = c(paste(className,
                                    attributeName,
                                    paste(as.character(entitiesIds),collapse = ","),
                                    paste(values,collapse = ","),
                                    sep = ";"),
                              "DataTransfRSet",
                              0))[[2]]
  stringRes <- xml_text(xml_find_all(content(answer),xpath=".//ns:result"))
  return(stringRes)
}

#' getAttributesOfEntities function
#' Get numeric or string atrribute values for all entities of a class
#' To use atanytime in a simulation
#' @param attributeName an string specifying the name of the attribute
#' @param className an string specifying the class of the attribute
#' @param num a boolean specifying if you are wating for numeric or string values
#' @return a dataframe with a column id with ids of entities
#' and a column with the name of the requested attribute

getAttributesOfEntities <- function(attributeName, className, num = T){
  #This method uses a special feature of the method CormasWS>>setAttribute:ofClass:value:
  #designed to get values of attribute "attributeName" of all entities of class "className"
  answer <- askCormas("SetAttributeOfClassValue",
                   argNames = c("attName", "className", "value"),
                   argValues = c(paste(className, attributeName, " ", " ", sep=";"),
                               "DataTransfRGet",
                               0))[[2]]
  stringRes <- xml_text(xml_find_all(content(answer),xpath=".//ns:result"))
  idsValues <- strsplit(stringRes, split = ";")[[1]]
  res <- data.frame(id = unlist(strsplit(idsValues[1], split = ",")),
                    value = unlist(strsplit(idsValues[2], split = ",")))
  if (num) {res$value <- as.numeric(as.character(res$value))}
  colnames(res)<- c("id", attributeName)
  return(res)
}



# OPENING VISUALWORKS FROM R ----------------------------------------------
#Not working at the moment, user need to open cormas manually

#' openViwualWorks
#' @param GUI : Boolean

openViwualWorks <- function(GUI = T) {
  if(GUI) {
    system(paste0("$VISUALWORKS",
                  "/bin/linux86/visual ",
                  "$VISUALWORKS",
                  "/cormas/cormasMac-com-work.im > ",
                  " $VISUALWORKS",
                  "/cormas/r-cormas.log &"))
  }
  else {
    system(paste0("$VISUALWORKS",
                  "/bin/linux86/visual ",
                  "$VISUALWORKS",
                  "/cormas/cormasMac-com-work.im",
                  " -headless > ",
                  " $VISUALWORKS",
                  "/cormas/r-cormas.log &"))
  }
}

#' closeVisualWorks function

closeVisualWorks <- function(){
  askCormas("quitVisualWorks")
}
