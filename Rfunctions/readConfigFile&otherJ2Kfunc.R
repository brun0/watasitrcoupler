
#####################################################
loadLibraries = function() {
  load <- c(require(ConfigParser), require(R.utils), require(RSQLite), require(feather), require(zoo), require (multiplex), require(tidyr), require(ggplot2), require(dplyr), require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };
}

#####################################################
readConfigFile = function() {
  
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

return <- c(jamsRootPath,stderrP,stdoutP)
}
#####################################################
initializeJ2K = function(jamsRootPath=jamsRootPath, jams_file_name=jams_file_name, stdoutP=stdoutP, stderrP=stderrP, wd=wd){
  killJ2K() # kill jams if it's running
  setwd(jamsRootPath)
  system2(
    'java',
    args=c('-jar', 'jams-starter.jar', '-m', paste0('data/J2K_cowat/',jams_file_name), '-n'),
    wait=F, stdout=stdoutP, stderr=stderrP
  )
  cat('\n\nWaiting 3 seconds to make sure J2K coupling module starts listening...')
  Sys.sleep(3)
  setwd(wd)
}

