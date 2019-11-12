#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R WatASit      ################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script pilot the WatASit model in Cormas plateform
# to test simulations for EMS 2020 paper without Optirrig
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2019, October, by
# B. Bonté -> make RCormas function to get/set Cormas attributes/probes
# B. Richard -> make this script
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list=ls()); sessionInfo(); start_time <- Sys.time()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1. R Settings #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1.1 Load functions [REQUIRED] #######
for(FileName in list.files("Rfunctions/", pattern="\\.[Rr]$")){ source(file.path("Rfunctions/",FileName)); }

####### 1.2 Load libraries [REQUIRED] #######
load <- c(require(zoo), require (multiplex), require(tidyr),require(ggplot2),require(dplyr),require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

####### 1.3 Core parallelism [OPTIONAL] #######
cores <- parallel:::detectCores(); registerDoParallel(cores-2);


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Simulation Settings and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2.1 Specification of case study, year and duration [REQUIRED] #######
case_study_name <- "Aspres"
date_start_sim <- as.Date("2017-05-01", "%Y-%m-%d"); date_end_sim <- as.Date("2017-05-31", "%Y-%m-%d")

####### 2.2 Importation of meteo data input  [REQUIRED] #######
data_meteo      = read.csv(paste0('climatefile/climate_buech_2017.csv'), header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
dates       <- as.Date(data_meteo$date, "%Y-%m-%d");
input_meteo <- zoo(data_meteo, dates); input_meteo <- window(input_meteo, start = date_start_sim, end = date_end_sim); str(input_meteo)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. WatASit initialization #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3.1 Connexion and opening of WatASit model [REQUIRED] #######
r <- openModel("COWAT", parcelFile="WatASit[EMSpaper].pcl")
 
####### 3.2 Activation of Cormas probes [OPTIONAL] #######
# probe_names <- c("abandonedCropEvent", "ASAinquiries", "exceedMaxWithdrawalEvent", "qIntake", "unrespectRestrictionEvent", "sumQOfEwaterReleases", "f1IrrigatedPlotNb", "f2IrrigatedPlotNb", "f3IrrigatedPlotNb", "f5IrrigatedPlotNb", "f6IrrigatedPlotNb", "f7IrrigatedPlotNb", "f10IrrigatedPlotNb", "f11IrrigatedPlotNb", "f12IrrigatedPlotNb","f14IrrigatedPlotNb", "f16IrrigatedPlotNb")

####### 3.3 Choose of WatASit initial state and time step function (scenarios) [REQUIRED] #######
r <- setInit("INIT_2017_54x44") # Initialization choice
r <- setStep("R_goBaselineStep:") # Scenario choice


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. WatASit simulation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4.1 Init Cormas simulation plateform [REQUIRED] #######
r <- initSimu()

####### 4.2 Create results dataFrame [OPTIONAL] #######
farmers_results <- data.frame(id = NULL, day = NULL, nbFloodPlotAffToday = NULL, dosCounter = NULL) #dosCounter = DoSomethingElse action counter
crops_results <- data.frame(id = NULL, day = NULL, labelNb = NULL, irriDailyDose = NULL)
ind_results <- data.frame()
  
####### 4.3 Run WatASit during the irrigation campaign [REQUIRED] #######
for (day in 1:dim(input_meteo)[1]){ 
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

     ####### 4.3.2 Run WatASit simulation of 24 hours [REQUIRED] #######
     r <- runSimu(duration = 24)
     response <- gettext(r[[2]])
     if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {stop("RUN STOPPED",call.=FALSE)} # To check if runSimu is done
     
     ####### 4.3.3 Get the farmers results from Cormas [OPTIONAL] #######
     f_obs1 <- getAttributesOfEntities("nbFloodPlotAffToday","Efarmer") # Number of FloodPlot affordance by farmer
     f_obs2 <- getAttributesOfEntities("dosCounter","Efarmer") # Number of DoSomethingElse action by farmer
     f_obs <- left_join(f_obs1, f_obs2, by = "id"); f_obs$day = day
     farmers_results <- farmers_results %>% bind_rows(f_obs)
  
     ####### 4.3.4 Get the crop results from Cormas [OPTIONAL] #######
     c_obs1 <- getAttributesOfEntities("labelNb", "Ecrop")
     c_obs2 <- getAttributesOfEntities("irriDailyDose", "Ecrop")
     c_obs <- left_join(c_obs1, c_obs2, by = "id"); c_obs$day = day
     crops_results <- crops_results %>% bind_rows(c_obs)
     
     ####### 4.3.5 Get the simulation indicators from Cormas [OPTIONAL] #######
     ind1 <- getAttributesOfEntities("nb", "TotalAbandonedCropNb");
     ind2 <- getAttributesOfEntities("inquiries", "IrrigatorAssociation")
     ind3 <- getAttributesOfEntities("nb", "ExceedMaxWithdrawal")
     ind4 <- getAttributesOfEntities("q", "EwaterIntake")
     ind5 <- getAttributesOfEntities("nb", "UnrespectRestrictionNb")
     ind <- data.frame(day, abandoned = ind1$nb, inquiries = ind2$inquiries, exceed = ind3$nb, q_cumec = ind4$q, unrespect = ind5$nb)
     ind_results <- rbind(ind_results, ind)
     cat('Simulation of day:', day, "/", dim(input_meteo)[1],"....",input_meteo$date[day],"....P[mm/day] =",P[day], "....P_forecast[Yes=1/No=0] =",p_forecast, "....ASAinquiries =",ind2$inquiries, "....cropAbandoned =",ind1$nb, "....Qintake[cumec] =",ind4$q, "....unrespectIrrigations =",ind5$nb, "\n")
     
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 5. Set the new state of crops in cormas [REQUIRED] #######
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mock_wsi <- runif(length(c_obs1$id), 0, 1)
  mock_cropMaturitySignal <- floor(runif(length(c_obs1$id), 0, 1.1))
  mock_newCropStates <- data.frame(c_obs1$id, mock_wsi, mock_cropMaturitySignal)
  setAttributesOfEntities("wsi", "Ecrop", c_obs1$id, mock_newCropStates$mock_wsi)
  setAttributesOfEntities("cropMaturitySignal", "Ecrop", c_obs1$id, mock_newCropStates$mock_cropMaturitySignal)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 6. Calculate simulation time [OPTIONAL] #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
end_time <- Sys.time(); simu_time = end_time - start_time; cat ("................................................................",
"\n","Simulation time is ", round(simu_time,2), "minutes", "\n")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 7. Plot simulation results [OPTIONAL] #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f1_res <- subset.data.frame(farmers_results, id == 1)
f2_res <- subset.data.frame(farmers_results, id == 2)
f3_res <- subset.data.frame(farmers_results, id == 3)
f4_res <- subset.data.frame(farmers_results, id == 4)
f5_res <- subset.data.frame(farmers_results, id == 5)
f6_res <- subset.data.frame(farmers_results, id == 6)
f7_res <- subset.data.frame(farmers_results, id == 7)
f8_res <- subset.data.frame(farmers_results, id == 8)
f9_res <- subset.data.frame(farmers_results, id == 9)
f10_res <- subset.data.frame(farmers_results, id == 10) ## le 11eme?

c1_res <- subset.data.frame(crops_results, labelNb == 1)

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

 