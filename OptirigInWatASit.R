library(tidyr)
library(ggplot2)
library(dplyr)
# set directories for R-cormas
r_cormas_dir <- "/home/bastien/Documents/2017-2020_These_GEAU/Work_Optirrig/Optirrig/WatASit/WatASit_Rcoupler/RCormas/"
r_optirrig_dir <- "/home/bastien/Documents/2017-2020_These_GEAU/Work_Optirrig/Optirrig/WatASit/WatASit_Rcoupler/OptirrigFunctions/"

############
## Optirrig-daily-step-function##
############

#Fichiers de paramètres

 #NB: meto include pluie météo
runPiloteStep <- function(idParcel, harvestSignal, irriDailyDose) {
  lai = harvestSignal + runif(length(idParcel)) * as.numeric(as.character(idParcel))
  wsi = lai / 30
  hi = lai / 50
  cropMaturitySignal = rbinom(irriDailyDose, 1, 0.5)
  res <- data.frame(idParcel = idParcel, lai = lai, wsi = wsi, hi = hi, cropMaturitySignal = cropMaturitySignal)
  return(res)
}

#####################
## RUN Cormas cowat##
#####################

######################
#### Init model ######
######################
setwd(r_cormas_dir)
#load the cormas functions
source("R/cormas-func.R")
 
# r <- openModel("COWAT", parcelFile="WatASit[v5.9.3]_bb.pcl")
r <- openModel("COWAT", parcelFile="WatASit[v8].pcl")
 
# Activate probes about crops (Facultatif: to get data from cormas)
r <- activateProbe("idParcelProbe","Ecrop")
r <- activateProbe("harvestSignalProbe","Ecrop")
r <- activateProbe("wsiProbe","Ecrop")
r <- activateProbe("laiProbe","Ecrop")
r <- activateProbe("hiProbe","Ecrop")
r <- activateProbe("irriDailyDoseProbe","Ecrop")
r <- activateProbe("cropMaturitySignal","Ecrop")

# r <- Choose initial state and time step function (scenarios)
r <- setInit("INIT_2017_54x44")
r <- setStep("goHourlyWithoutTurnStep:")

#Set simulation duration
simDayNb <- 5

##############################
##### Simulate coupled model###
##############################
days <- c()
riverFlows <- c()

#Initialise simulation
r <- initSimu()

#Create a dataFrame
# cropResults <- data.frame(id = 0, lai = 0, hi = 0, irrigation =0 )
#cropResults <- data.frame(idParcel = 0, harvestSignal = 0, wsi = 0, lai = 0, hi = 0, irriDailyDose = 0)
cropResults <- data.frame(idParcel = NULL, wsi = NULL, lai = NULL, hi = NULL, irriDailyDose = NULL, cropMaturitySignal = NULL, day = NULL )

#Run a simulation of simDayNb days
for (day in 1:simDayNb){

  ## Simulate 24 hours of cormas
  r <- runSimu(duration = 24)

  # Get the state of crops
  idParcel <- getAttributesOfEntities("idParcel", "Ecrop")
  harvestSignal <- getAttributesOfEntities("harvestSignal", "Ecrop")
  irriDailyDose <- getAttributesOfEntities("irriDailyDose", "Ecrop")

  #Simulate the new state of crops with Optirrig
  newCropState <- runPiloteStep(idParcel$idParcel, harvestSignal$harvestSignal, irriDailyDose$irriDailyDose)

  #Set the new state of crops in cormas
  # setAttributesOfEntities("harvestSignal", "Ecrop", newCropState$idParcel, newCropState$harvestSignal)
  setAttributesOfEntities("wsi", "Ecrop", idParcel$id, newCropState$wsi)
  setAttributesOfEntities("lai", "Ecrop", idParcel$id, newCropState$lai)
  setAttributesOfEntities("hi", "Ecrop", idParcel$id, newCropState$wsi)
  setAttributesOfEntities("cropMaturiySignal", "Ecrop", idParcel$id, newCropState$cropMaturitySignal)
  # setAttributesOfEntities("irriD", "Ecrop", newCropState$id, newCropState$hi)

  #Save results in R (even though they are also in cormas)
  newCropState$irrigation <- irriDailyDose$values
  newCropState$day <- day
  cropResults <- rbind(cropResults, newCropState )
}

#Observe the evolution of parcells states in Cormas..
# To do in Cormas
cropResults %>% 
  tbl_df()

# Just to see that parcells has different values of lais:

cropResults %>% 
  ggplot() +
  geom_line(aes(x = day, y = lai, color=id))

cropResults %>% 
  ggplot() +
  geom_line(aes(x = day, y = hi, color=id))



