#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R coupler 2.0      ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script runs J2K-WatASit-Optirrig coupled simulations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2020, Jan-June, by
# J. Veyssier -> make superjams, socket methods and Rcoupler
# B. Bonté -> make RCormas methods and Rcoupler
# B. Richard -> make param and climate Rfunctions and Rcoupler
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  load <- c(require(ConfigParser), require(R.utils), require(RSQLite), require(feather), require(zoo), require (multiplex), require(tidyr), require(ggplot2), require(dplyr), require(doParallel), require(gridExtra)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };
  
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