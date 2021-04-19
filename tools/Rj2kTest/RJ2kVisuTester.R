#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R J2k tester file  ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script can be used to test funcitons of Rj2k.R file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2020, Jan-June, by
# J. Veyssier -> make superjams_6, socket methods and Rcoupler v1
# B. Bonté -> make RCormas methods and Rcoupler v1
# B. Richard -> make param and climate Rfunctions and Rcoupler v2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()); start_time <- Sys.time();
ptm <- proc.time()
wd <- getwd()
jams_file_name <- "cowat.jam"
#jams_file_name <- "j2k_cowat_buech_ju_couplage.jam"

#Source fonction file
source("Rfunctions/Rj2k.R")
#source("Rfunctions/Bilanj2k.R")

# load libraries
load <- c(require(ConfigParser), require(R.utils), require(RSQLite), require(feather), require(zoo), require (multiplex), require(tidyr), require(ggplot2), require(dplyr), require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

####### Read config file #######
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


####### Initialize J2K model #######
# On laisse le coupleur lancer JAMS/J2K
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

cat('\nRunning simulation!!!\n')
####### Run J2K model And test your function there#######
duration <- 10

hrusVariables <- NULL
reachsVariables <- NULL

j2kMakeStep(6*30)

simuProgress <- txtProgressBar(min = 1,
                               max = duration,
                               style = 3)
for (t in 1:duration) {
  setTxtProgressBar(simuProgress, t)
  j2kMakeStep(1)
  res1 <- j2kGetValuesAllHrus(c("actMPS", "actLPS", "actDPS","TotSWE", "actRG1", "actRG2", "storedInterceptedWater",
                                "rain", "snow", "etact"))
  hrus_t <- res1 %>% 
    mutate(storedWater = actMPS + actLPS + actDPS + TotSWE + actRG1 + actRG2 + storedInterceptedWater)
  
  res2 <- j2kGetValuesAllReachs(c("actRD1", "actRD2", "actRG1",  "actRG2", "Runoff"))
  reachs_t <- res2 %>% 
    mutate(storedWater = actRD1 + actRD2 + actRG1 + actRG2)

  hrusVariables <- rbind(hrusVariables, 
                         data.frame(hrus_t,t))
  
  reachsVariables <- rbind(reachsVariables, 
                         data.frame(reachs_t,t))
}

j2kStop()
Sys.sleep(3)
killJ2K()
write.csv(hrusVariables, "tools/Rj2kTest/hruVariablesBig.csv",row.names =F)
write.csv(reachsVariables, "tools/Rj2kTest/reachsVariablesBig.csv",row.names =F)
timeSpent <- proc.time() - ptm
write.csv(timeSpent %>% summary(), "tools/Rj2kTest/timeSpentForVisuBig.csv")


hrus <- read.csv("tools/Rj2kTest/hruVariables.csv")
reachs <- read.csv("tools/Rj2kTest/reachsVariables.csv")

hrus %>% 
  tbl_df()

reachs %>% 
  tbl_df()

hrus %>%
  #mutate(storage = actMPS + actLPS + TotSWE + actRG1 + actRG2 + storedInterceptedWater) %>%
  #mutate(precip = rain + snow) %>%
  group_by(t) %>%
  summarise_all(sum) %>%
    ggplot(aes(x= t)) +
    geom_line(aes(y = storedWater, color = "storage")) +
    geom_line(aes(y = rain, color = "rain")) +
    geom_line(aes(y = snow, color = "snow"))
  
reachs %>%
  #mutate(storage = actMPS + actLPS + TotSWE + actRG1 + actRG2 + storedInterceptedWater) %>%
  #mutate(precip = rain + snow) %>%
  group_by(t) %>%
  summarise_all(sum) %>%
  ggplot(aes(x= t)) +
  geom_line(aes(y = storedWater, color = "storage")) +
  geom_line(aes(y = Runoff, color = "Runoff"))
