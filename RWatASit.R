#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R coupler      ################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script pilot the WatASit model in Cormas plateform
# to test simulations for EMS 2020 paper without Optirrig
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2019, October, by
# B. Bonté -> make RCormas function to get/set Cormas attributes/probes
# B. Richard -> make the script
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list=ls()); sessionInfo(); start_time <- Sys.time()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1. R Settings #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1.1 Load functions #######
for(FileName in list.files("Rfunctions/", pattern="\\.[Rr]$")){ source(file.path("Rfunctions/",FileName)); }

####### 1.2 Load libraries #######
load <- c(require(zoo), require (multiplex), require(tidyr),require(ggplot2),require(dplyr),require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

####### 1.3 Core parallelism #######
cores <- parallel:::detectCores(); registerDoParallel(cores-2);


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Simulation Settings and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2.1 Specification of case study, year and duration #######
case_study_name <- "Aspres"
date_start_sim <- as.Date("2017-05-01"); date_end_sim <- as.Date("2017-08-01") #date_end_sim <- as.Date("2017-10-31")

####### 2.2 Importation of meteo data input  #######
data_meteo      = read.csv(paste0('climatefile/climate_buech_2017.csv'), header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
dates       <- as.Date(data_meteo$date,"%Y-%m-%d")
input_meteo <- zoo(data_meteo, dates); input_meteo <- window(input_meteo, start = date_start_sim, end = date_end_sim); str(input_meteo)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. WatASit initialization #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3.1 Connexion and opening of WatASit model #######
r <- openModel("COWAT", parcelFile="WatASit[EMSpaper].pcl")
 
####### 3.2 Activation of probes about crops (Facultatif: to get data from cormas) #######
probe_names <- c("abandonedCropEvent", "ASAinquiries", "exceedMaxWithdrawalEvent", "qIntake", "unrespectRestrictionEvent", "sumQOfEwaterReleases", "f1IrrigatedPlotNb", "f2IrrigatedPlotNb", "f3IrrigatedPlotNb", "f5IrrigatedPlotNb", "f6IrrigatedPlotNb", "f7IrrigatedPlotNb", "f10IrrigatedPlotNb", "f11IrrigatedPlotNb", "f12IrrigatedPlotNb","f14IrrigatedPlotNb", "f16IrrigatedPlotNb")

####### 3.3 Choose of WatASit initial state and time step function (scenarios) #######
r <- setInit("R_INIT_2017_54x44") # Initialization choice
r <- setStep("R_goBaselineStep:") # Scenario choice


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. WatASit simulation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4.1 Create results dataFrame #######
r <- initSimu() #initialize Cormas simulation

####### 4.2 Create results dataFrame #######
farmers_results <- data.frame(id = NULL, day = NULL, nbFloodPlotAffToday = NULL, dosCounter = NULL) #dosCounter = DoSomethingElse action counter
crops_results <- data.frame(id = NULL, day = NULL, idParcel = NULL, irriDailyDose = NULL, harvestSignal = NULL)
  
####### 4.3 Run WatASit during the irrigation campaign #######
for (day in 1:dim(input_meteo)[1]){ cat('Simulation of day:', day, "/", dim(input_meteo)[1],"....",input_meteo$date[day],"...................", "\n")
      ####### 4.3.1 Update Cormas Meteo #######
      P<-input_meteo$P; setAttributesOfEntities("p", "Meteo", 1, as.numeric(P[day])) # Precipitation conditions of the day 
      p_forecast = sum(as.numeric(P[day:(day+2)]), na.rm = TRUE); if (p_forecast > 0) {p_forecast = 1}; setAttributesOfEntities("p_forecast", "Meteo", 1, p_forecast) # Precipitation forecast for the next 3 days
      if (day == 1) {p_cumTenDays = 0}
      if (day == 2) {p_cumTenDays = P[day-1]}
      for (i in 3:10){if (day == i){ p_cumTenDays = sum(as.numeric(P[(day-(i-1)):(day-1)]), na.rm = TRUE) }}
      if (day >= 11) {p_cumTenDays = sum(as.numeric(P[(day-10):(day-1)]), na.rm = TRUE)}
      setAttributesOfEntities("p_cumTenDays", "Meteo", 1, p_cumTenDays) # Calculate cumulative precipitation for the last 10 days
      if (day <= 11) {p_cumFifteenDays = p_cumTenDays}
      if (day == 12) {p_cumFifteenDays = sum(c(p_cumTenDays,as.numeric(P[day-11])), na.rm = TRUE)}
      for (i in 13:15){if (day == i){ p_cumFifteenDays = sum(c(p_cumTenDays,as.numeric(P[(day-(i-1)):(day-1)])), na.rm = TRUE) }}
      if (day >= 16) {p_cumFifteenDays = sum(c(p_cumTenDays,as.numeric(P[(day-15):(day-1)])), na.rm = TRUE)}
      setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, p_cumFifteenDays) # Calculate cumulative precipitation for the last 15 days

     ####### 4.3.2 Run WatASit simulation of 24 hours #######
     r <- runSimu(duration = 24)
     response <- gettext(r[[2]])
     if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {stop("RUN STOPPED",call.=FALSE)} # To check if runSimu is done
     
     ####### 4.3.3 Get the farmers results from Cormas #######
     f_obs1 <- getAttributesOfEntities("nbFloodPlotAffToday","Efarmer")
     f_obs2 <- getAttributesOfEntities("dosCounter","Efarmer")
     f_obs <- left_join(f_obs1, f_obs2, by = "id")
     f_obs$day = day
     farmers_results <- farmers_results %>% bind_rows(f_obs)
     
     ####### 4.3.4 Get the crop results from Cormas #######
     c_obs1 <- getAttributesOfEntities("idParcel", "Ecrop")
     c_obs2 <- getAttributesOfEntities("irriDailyDose", "Ecrop")
     c_obs3 <- getAttributesOfEntities("harvestSignal", "Ecrop")
     c_obs <- left_join(c_obs1, c_obs2, c_obs3, by = "id")
     c_obs$day = day
     crops_results <- crops_results %>% bind_rows(c_obs)
     
  ####### 5.2 Set the new state of crops in cormas #######
  mock_wsi <- runif(length(c_obs1$idParcel), 0, 1)
  mock_cropMaturitySignal <- floor(runif(length(c_obs1$idParcel), 0, 1.1))
  mock_newCropStates <- data.frame(c_obs1$idParcel, mock_wsi, mock_cropMaturitySignal)
  setAttributesOfEntities("wsi", "Ecrop", c_obs1$idParcel, mock_newCropStates$mock_wsi)
  setAttributesOfEntities("cropMaturitySignal", "Ecrop",c_obs1$idParcel, mock_newCropStates$mock_cropMaturitySignal)

  ####### 5.3 Save results in R (even though they are also in cormas) #######
  # f_results <- rbind(f_results, farmers_results)
  # c_results <- rbind(c_results, crops_results)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 6. Observe the evolution of coupled dynamics #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 abandonedCropEvent <- getNumericProbe("abandonedCropEvent","COWAT")
 ASAinquiries <- getNumericProbe("ASAinquiries","COWAT")
 exceedMaxWithdrawalEvent <- getNumericProbe("exceedMaxWithdrawalEvent","COWAT")
 qIntake <- getNumericProbe("qIntake","COWAT")
 unrespectRestrictionEvent <- getNumericProbe("unrespectRestrictionEvent","COWAT")
 f1IrrigatedPlotNb <- getNumericProbe("f1IrrigatedPlotNb","COWAT")
 f2IrrigatedPlotNb <- getNumericProbe("f2IrrigatedPlotNb","COWAT")
 f3IrrigatedPlotNb <- getNumericProbe("f3IrrigatedPlotNb","COWAT")
 f5IrrigatedPlotNb <- getNumericProbe("f5IrrigatedPlotNb","COWAT")
 f6IrrigatedPlotNb <- getNumericProbe("f6IrrigatedPlotNb","COWAT")
 f7IrrigatedPlotNb <- getNumericProbe("f7IrrigatedPlotNb","COWAT")
 f10IrrigatedPlotNb <- getNumericProbe("f10IrrigatedPlotNb","COWAT")
 f11IrrigatedPlotNb <- getNumericProbe("f11IrrigatedPlotNb","COWAT")
 f12IrrigatedPlotNb <- getNumericProbe("f12IrrigatedPlotNb","COWAT")
 f14IrrigatedPlotNb <- getNumericProbe("f14IrrigatedPlotNb","COWAT")
 f16IrrigatedPlotNb <- getNumericProbe("f16IrrigatedPlotNb","COWAT")

# cropResults %>% 
#   tbl_df() # To do in Cormas
# 
# cropResults %>% 
#   ggplot() +
#   geom_line(aes(x = day, y = lai, color=id)) # Just to see that parcells has different values of lais:
# 
# cropResults %>% 
#   ggplot() +
#   geom_line(aes(x = day, y = hi, color=id))

 end_time <- Sys.time(); simu_time = end_time - start_time; cat ("Simulation time is ", round(simu_time,2), "minutes", "\n")