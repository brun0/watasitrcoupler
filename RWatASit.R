#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R WatASit      ################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script pilot the WatASit multi-agent model in Cormas plateform
# to test simulations for EMS 2020 paper without Optirrig
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2019, October, by
# B. Bonté -> make RCormas function to get/set Cormas attributes/probes
# B. Richard -> make this script
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# You must initialize Cormas before run this script
rm(list=ls()); sessionInfo(); start_time <- Sys.time()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1. R Settings #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1.1 Load functions [REQUIRED] #######
for(FileName in list.files("Rfunctions/", pattern="\\.[Rr]$")){ source(file.path("Rfunctions/",FileName)); }

####### 1.2 Load libraries [REQUIRED] #######
load <- c(require(gridExtra), require(RColorBrewer), require(zoo), require (multiplex), require(tidyr),require(ggplot2),require(dplyr),require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

####### 1.3 Core parallelism [OPTIONAL] #######
cores <- parallel:::detectCores(); registerDoParallel(cores-2);


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Simulation Settings and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2.1 Specification of case study, year and duration [REQUIRED] #######
case_study_name <- "Aspres"
date_start_sim <- as.Date("2017-05-01", "%Y-%m-%d"); date_end_sim <- as.Date("2017-08-31", "%Y-%m-%d")

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
farmers_aff <- data.frame()
farmers_act <- data.frame()
crops_results <- data.frame()
meadow_results <- data.frame()
forage_results <- data.frame()
winterCereal_results <- data.frame()
springCereal_results <- data.frame()
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
     
     ####### 4.3.3 Get the farmers affordances and actions from Cormas [OPTIONAL] #######
     f_aff1 <- getAttributesOfEntities("askAffCounter","Efarmer") 
     f_aff2 <- getAttributesOfEntities("doSEAffCounter","Efarmer") 
     f_aff3 <- getAttributesOfEntities("floodAffCounter","Efarmer") 
     f_aff4 <- getAttributesOfEntities("floodDRAffCounter","Efarmer") 
     f_aff5 <- getAttributesOfEntities("mowingAffCounter","Efarmer") 
     f_aff6 <- getAttributesOfEntities("pickingAffCounter","Efarmer")
     f_aff7 <- getAttributesOfEntities("pressingAffCounter","Efarmer")
     f_aff8 <- getAttributesOfEntities("reapingAffCounter","Efarmer")
     f_aff9 <- getAttributesOfEntities("swathingAffCounter","Efarmer")
     f_aff10 <- getAttributesOfEntities("teddingAffCounter","Efarmer")
     f_idExpl <- getAttributesOfEntities("idExpl","Efarmer")
     f_aff <- data.frame(f_aff1, f_aff2, f_aff3, f_aff4, f_aff5, f_aff6, f_aff7, f_aff8, f_aff9, f_aff10, f_idExpl); f_aff$day = day;
     farmers_aff <- farmers_aff %>% rbind(f_aff)
     f_act1 <- getAttributesOfEntities("askActCounter","Efarmer") 
     f_act2 <- getAttributesOfEntities("doSEActCounter","Efarmer") 
     f_act3 <- getAttributesOfEntities("floodActCounter","Efarmer") 
     f_act4 <- getAttributesOfEntities("floodDRActCounter","Efarmer") 
     f_act5 <- getAttributesOfEntities("mowingActCounter","Efarmer") 
     f_act6 <- getAttributesOfEntities("pickingActCounter","Efarmer")
     f_act7 <- getAttributesOfEntities("pressingActCounter","Efarmer")
     f_act8 <- getAttributesOfEntities("reapingActCounter","Efarmer")
     f_act9 <- getAttributesOfEntities("swathingActCounter","Efarmer")
     f_act10 <- getAttributesOfEntities("teddingActCounter","Efarmer")
     f_act <- data.frame(f_act1, f_act2, f_act3, f_act4, f_act5, f_act6, f_act7, f_act8, f_act9, f_act10, f_idExpl); f_act$day = day
     farmers_act <- farmers_act %>% rbind(f_act)
  
     ####### 4.3.4 Get the crop results from Cormas [OPTIONAL] #######
     ####### 4.3.4.1  By parcel ID [OPTIONAL] #######
     c_obs1 <- getAttributesOfEntities("labelNb", "Ecrop")
     c_obs2 <- getAttributesOfEntities("irriDailyDose", "Ecrop")
     c_obs3 <- getAttributesOfEntities("lai", "Ecrop")
     c_obs4 <- getAttributesOfEntities("wsi", "Ecrop")
     c_obs5 <- getAttributesOfEntities("hi", "Ecrop")
     c_obs6 <- getAttributesOfEntities("daysFromLastIrrigation", "Ecrop")
     c_obs7 <- getAttributesOfEntities("abandonedState", "Ecrop")
     c_obs8 <- getAttributesOfEntities("irriState", "Ecrop")
     c_obs9 <- getAttributesOfEntities("harvestSignal", "Ecrop")
     c_obs10 <- getAttributesOfEntities("cropMaturitySignal", "Ecrop")
     c_obs11 <- getAttributesOfEntities("idParcel", "Ecrop")
     c_obs12<- getAttributesOfEntities("idExpl", "Ecrop")
     c_obs13<- getAttributesOfEntities("unrespectIrrigationCounter", "Ecrop")
     c_obs <- data.frame(c_obs1, c_obs2, c_obs3, c_obs4, c_obs5, c_obs6, c_obs7, c_obs8, c_obs9, c_obs10, c_obs11, c_obs12, c_obs13); c_obs$day = day
     crops_results <- crops_results %>% rbind(c_obs)
     
     ####### 4.3.4.2  By crop ID [OPTIONAL] #######
     meadow_idExpl <- getAttributesOfEntities("idExpl", "Emeadow")
     meadow_wsi <- getAttributesOfEntities("wsi", "Emeadow")
     meadow_lai <- getAttributesOfEntities("lai", "Emeadow")
     forage_idExpl <- getAttributesOfEntities("idExpl", "Eforage")
     forage_wsi <- getAttributesOfEntities("wsi", "Eforage")
     forage_lai <- getAttributesOfEntities("lai", "Eforage")
     winterCereal_idExpl <- getAttributesOfEntities("idExpl", "EwinterCereal")
     winterCereal_wsi <- getAttributesOfEntities("wsi", "EwinterCereal")
     winterCereal_lai <- getAttributesOfEntities("lai", "EwinterCereal")
     springCereal_idExpl <- getAttributesOfEntities("idExpl", "EspringCereal")
     springCereal_wsi <- getAttributesOfEntities("wsi", "EspringCereal")
     springCereal_lai <- getAttributesOfEntities("lai", "EspringCereal")
     meadow <- data.frame(meadow_wsi, meadow_lai, meadow_idExpl)
     meadow <- meadow %>% group_by(idExpl) %>% summarise(mean_wsi = mean(wsi), mean_lai = mean(lai), n = n()); meadow$day = day
     meadow_results <- meadow_results %>% rbind(meadow)
     forage <- data.frame(forage_wsi, forage_lai, forage_idExpl)
     forage <- forage %>% group_by(idExpl) %>% summarise(mean_wsi = mean(wsi), mean_lai = mean(lai), n = n()); forage$day = day
     forage_results <- forage_results %>% rbind(forage)
     winterCereal <- data.frame(winterCereal_wsi, winterCereal_lai, winterCereal_idExpl)
     winterCereal <- winterCereal %>% group_by(idExpl) %>% summarise(mean_wsi = mean(wsi), mean_lai = mean(lai), n = n()); winterCereal$day = day
     winterCereal_results <- winterCereal_results %>% rbind(winterCereal)
     springCereal <- data.frame(springCereal_wsi, springCereal_lai, springCereal_idExpl)
     springCereal <- springCereal %>% group_by(idExpl) %>% summarise(mean_wsi = mean(wsi), mean_lai = mean(lai), n = n()); springCereal$day = day
     springCereal_results <- springCereal_results %>% rbind(springCereal)
     
     ####### 4.3.5 Get the simulation indicators from Cormas [OPTIONAL] #######
     ind1 <- getAttributesOfEntities("q", "EwaterIntake")
     ind2 <- getAttributesOfEntities("inquiries", "IrrigatorAssociation")
     ind3 <- getAttributesOfEntities("nb", "TotalAbandonedCropNb");
     ind4 <- getAttributesOfEntities("nb", "UnrespectRestrictionNb")
     ind5 <- getAttributesOfEntities("totalRelease", "RiverReach")
     ind <- data.frame(day, q_cumec = ind1$q, inquiries = ind2$inquiries, abandoned = ind3$nb, unrespect = ind4$nb, release_cumec = ind5$totalRelease)
     ind_results <- rbind(ind_results, ind)
     cat('Simulation of day:', day, "/", dim(input_meteo)[1],"....",input_meteo$date[day],"....P[mm/day] =",P[day], "....P_cumTenDays[mm/day] =",p_cumTenDays,"\n")
     
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 5. Set the new state of crops in cormas [REQUIRED] #######
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mock_wsi <- runif(length(c_obs1$id), 0, 1)
  mock_cropMaturitySignal <- floor(runif(length(c_obs1$id), 0, 1.2))
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
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
is.numeric0 <- function(x) {
  identical(x, numeric(0))
}
####### 7.1 Get the farmers affordances and actions [OPTIONAL] #######
farmers_aff <- farmers_aff %>% select(-id.1,-id.2,-id.3,-id.4,-id.5,-id.6,-id.7,-id.8,-id.9,-id.10)
farmers_act <- farmers_act %>% select(-id.1,-id.2,-id.3,-id.4,-id.5,-id.6,-id.7,-id.8,-id.9,-id.10)
parcels_by_farm <- crops_results %>% select(id, idExpl, day) #To identify the number of farmplots (in ASA) by farmer
parcels_nb_farm1 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 1)))[1]
parcels_nb_farm2 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 2)))[1]
parcels_nb_farm3 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 3)))[1]
parcels_nb_farm4 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 4)))[1]
parcels_nb_farm5 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 5)))[1]
parcels_nb_farm6 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 6)))[1]
parcels_nb_farm7 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 7)))[1]
parcels_nb_farm8 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 8)))[1]
parcels_nb_farm9 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 9)))[1]
parcels_nb_farm10 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 10)))[1]
parcels_nb_farm11 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 11)))[1]
parcels_nb_farm12 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 12)))[1]
parcels_nb_farm13 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 13)))[1]
parcels_nb_farm14 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 14)))[1]
parcels_nb_farm15 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 15)))[1]
parcels_nb_farm16 <- dim(subset.data.frame(parcels_by_farm, (day == 1) & (idExpl == 16)))[1]
farmers_aff[which(farmers_aff$idExpl == 1),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 1),(2:11)] / parcels_nb_farm1
farmers_aff[which(farmers_aff$idExpl == 2),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 2),(2:11)] / parcels_nb_farm2
farmers_aff[which(farmers_aff$idExpl == 3),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 3),(2:11)] / parcels_nb_farm3
farmers_aff[which(farmers_aff$idExpl == 4),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 4),(2:11)] / parcels_nb_farm4
farmers_aff[which(farmers_aff$idExpl == 5),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 5),(2:11)] / parcels_nb_farm5
farmers_aff[which(farmers_aff$idExpl == 6),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 6),(2:11)] / parcels_nb_farm6
farmers_aff[which(farmers_aff$idExpl == 7),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 7),(2:11)] / parcels_nb_farm7
farmers_aff[which(farmers_aff$idExpl == 8),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 8),(2:11)] / parcels_nb_farm8
farmers_aff[which(farmers_aff$idExpl == 9),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 9),(2:11)] / parcels_nb_farm9
farmers_aff[which(farmers_aff$idExpl == 10),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 10),(2:11)] / parcels_nb_farm10
farmers_aff[which(farmers_aff$idExpl == 11),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 11),(2:11)] / parcels_nb_farm11
farmers_aff[which(farmers_aff$idExpl == 12),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 12),(2:11)] / parcels_nb_farm12
farmers_aff[which(farmers_aff$idExpl == 13),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 13),(2:11)] / parcels_nb_farm13
farmers_aff[which(farmers_aff$idExpl == 14),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 14),(2:11)] / parcels_nb_farm14
farmers_aff[which(farmers_aff$idExpl == 15),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 15),(2:11)] / parcels_nb_farm15
farmers_aff[which(farmers_aff$idExpl == 16),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 16),(2:11)] / parcels_nb_farm16
# farmers_act[which(farmers_act$idExpl == 1),(2:11)] <- farmers_act[which(farmers_act$idExpl == 1),(2:11)] / parcels_nb_farm1
# farmers_act[which(farmers_act$idExpl == 2),(2:11)] <- farmers_act[which(farmers_act$idExpl == 2),(2:11)] / parcels_nb_farm2
# farmers_act[which(farmers_act$idExpl == 3),(2:11)] <- farmers_act[which(farmers_act$idExpl == 3),(2:11)] / parcels_nb_farm3
# farmers_act[which(farmers_act$idExpl == 4),(2:11)] <- farmers_act[which(farmers_act$idExpl == 4),(2:11)] / parcels_nb_farm4
# farmers_act[which(farmers_act$idExpl == 5),(2:11)] <- farmers_act[which(farmers_act$idExpl == 5),(2:11)] / parcels_nb_farm5
# farmers_act[which(farmers_act$idExpl == 6),(2:11)] <- farmers_act[which(farmers_act$idExpl == 6),(2:11)] / parcels_nb_farm6
# farmers_act[which(farmers_act$idExpl == 7),(2:11)] <- farmers_act[which(farmers_act$idExpl == 7),(2:11)] / parcels_nb_farm7
# farmers_act[which(farmers_act$idExpl == 8),(2:11)] <- farmers_act[which(farmers_act$idExpl == 8),(2:11)] / parcels_nb_farm8
# farmers_act[which(farmers_act$idExpl == 9),(2:11)] <- farmers_act[which(farmers_act$idExpl == 9),(2:11)] / parcels_nb_farm9
# farmers_act[which(farmers_act$idExpl == 10),(2:11)] <- farmers_act[which(farmers_act$idExpl == 10),(2:11)] / parcels_nb_farm10
# farmers_act[which(farmers_act$idExpl == 11),(2:11)] <- farmers_act[which(farmers_act$idExpl == 11),(2:11)] / parcels_nb_farm11
# farmers_act[which(farmers_act$idExpl == 12),(2:11)] <- farmers_act[which(farmers_act$idExpl == 12),(2:11)] / parcels_nb_farm12
# farmers_act[which(farmers_act$idExpl == 13),(2:11)] <- farmers_act[which(farmers_act$idExpl == 13),(2:11)] / parcels_nb_farm13
# farmers_act[which(farmers_act$idExpl == 14),(2:11)] <- farmers_act[which(farmers_act$idExpl == 14),(2:11)] / parcels_nb_farm14
# farmers_act[which(farmers_aff$idExpl == 15),(2:11)] <- farmers_act[which(farmers_act$idExpl == 15),(2:11)] / parcels_nb_farm15
# farmers_act[which(farmers_act$idExpl == 16),(2:11)] <- farmers_act[which(farmers_act$idExpl == 16),(2:11)] / parcels_nb_farm16


####### 7.2 Get the crops state by farmer [OPTIONAL] #######
forage_results <- as.data.frame(forage_results)
forage_results <- subset.data.frame(forage_results, (idExpl != 1) & (idExpl != 4) & (idExpl != 8) & (idExpl != 9) & (idExpl != 13) & (idExpl != 15))
meadow_results <- as.data.frame(meadow_results)
meadow_results <- subset.data.frame(meadow_results, (idExpl != 1) & (idExpl != 4) & (idExpl != 8) & (idExpl != 9) & (idExpl != 13) & (idExpl != 15))
winterCereal_results <- as.data.frame(winterCereal_results)
winterCereal_results <- subset.data.frame(winterCereal_results, (idExpl != 1) & (idExpl != 4) & (idExpl != 8) & (idExpl != 9) & (idExpl != 13) & (idExpl != 15))
springCereal_results <- as.data.frame(springCereal_results)
springCereal_results <- subset.data.frame(springCereal_results, (idExpl != 1) & (idExpl != 4) & (idExpl != 8) & (idExpl != 9) & (idExpl != 13) & (idExpl != 15))

####### 7.3 Get the crops state by parcel [OPTIONAL] #######
crops_results_end <- subset.data.frame(crops_results, day == length(unique(crops_results$day)))
crops_results_end[which(crops_results_end$idExpl==2),24] <- 1
crops_results_end[which(crops_results_end$idExpl==3),24] <- 2
crops_results_end[which(crops_results_end$idExpl==5),24] <- 3
crops_results_end[which(crops_results_end$idExpl==6),24] <- 4
crops_results_end[which(crops_results_end$idExpl==7),24] <- 5
crops_results_end[which(crops_results_end$idExpl==10),24] <- 6
crops_results_end[which(crops_results_end$idExpl==11),24] <- 7
crops_results_end[which(crops_results_end$idExpl==12),24] <- 8
crops_results_end[which(crops_results_end$idExpl==14),24] <- 9
crops_results_end[which(crops_results_end$idExpl==16),24] <- 10
abandoned_plots <- crops_results_end %>% select(id, abandonedState, idExpl)
unrespect_plots <- crops_results_end %>% select(id, unrespectIrrigationCounter, idExpl)

####### 7.4 Plot the simulation results [OPTIONAL] #######
id_names <- c(
  `2` = " Farmer 1",
  `3` = " Farmer 2",
  `5` = " Farmer 3",
  `6` = " Farmer 4",
  `7` = " Farmer 5",
  `10` = " Farmer 6",
  `11` = " Farmer 7",
  `12` = " Farmer 8",
  `14` = " Farmer 9",
  `16` = " Farmer 10"
)

####### 7.4.1 Plot the farmers'affordances [OPTIONAL] #######
aff <- farmers_aff %>%
  gather(Affordance,value, askAffCounter,doSEAffCounter,floodAffCounter, floodDRAffCounter, mowingAffCounter, pickingAffCounter, pressingAffCounter, reapingAffCounter, swathingAffCounter, teddingAffCounter) %>%
  ggplot(aes(x=day, y=value, colour=Affordance)) +
  facet_wrap(~ factor(idExpl, levels=c('2','3','5','6', '7', '10', '11', '12', '14', '16')), scales = "fixed", ncol = 5, labeller = as_labeller(id_names)) +
  geom_line() + xlim(c(0, max(farmers_aff$day))) + ylim(c(0, max(farmers_aff$askAffCounter))) +
  scale_color_manual(name="Affordance", 
                     labels = c("Ask more water at plot", 
                                "Do something else", 
                                "Flood plot", 
                                "Flood plot during restriction", 
                                "Mowing",
                                "Picking & storing",
                                "Pressing",
                                "Reaping & storing",
                                "Swathing",
                                "Tedding"), 
                     values = c("askAffCounter"="#984ea3", 
                                "doSEAffCounter"="#000000", 
                                "floodAffCounter"="#377eb8", 
                                "floodDRAffCounter"="#e41a1c", 
                                "mowingAffCounter"="#00441b",
                                "pickingAffCounter"="#006d2c",
                                "pressingAffCounter"="#238b45",
                                "reapingAffCounter"="#41ab5d",
                                "swathingAffCounter"="#74c476",
                                "teddingAffCounter"="#a1d99b")) +
  theme_bw() +
  theme(legend.box = "vertical",  legend.position = c(0.5,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
          panel.grid = element_line(),
          panel.background = element_blank(),
          legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
          plot.title = element_text(hjust = 0.5)) +
  labs(x = paste(" ",sep = ""), y = paste("Number of affordances / Number of farm plots", sep = ""))
# legend <- get_legend(aff)
  
####### 7.4.2 Plot the farmers'actions [OPTIONAL] #######
act <- farmers_act %>%
  gather(Action,value, askActCounter,doSEActCounter,floodActCounter, floodDRActCounter, mowingActCounter, pickingActCounter, pressingActCounter, reapingActCounter, swathingActCounter, teddingActCounter) %>%
  ggplot(aes(x=day, y=value, colour=Action)) +
  facet_wrap(~ factor(idExpl, levels=c('2','3','5','6', '7', '10', '11', '12', '14', '16')), scales = "fixed", ncol = 5, labeller = as_labeller(id_names)) +
  geom_line() + xlim(c(0, max(farmers_act$day))) + ylim(c(0, max(farmers_act$askActCounter))) +
  scale_color_manual(name="Action", 
                     labels = c("Ask more water at plot", 
                                "Do something else", 
                                "Flood plot", 
                                "Flood plot during restriction", 
                                "Mowing",
                                "Picking & storing",
                                "Pressing",
                                "Reaping & storing",
                                "Swathing",
                                "Tedding"), 
                     values = c("askActCounter"="#984ea3", 
                                "doSEActCounter"="#000000", 
                                "floodActCounter"="#377eb8", 
                                "floodDRActCounter"="#e41a1c", 
                                "mowingActCounter"="#00441b",
                                "pickingActCounter"="#006d2c",
                                "pressingActCounter"="#238b45",
                                "reapingActCounter"="#41ab5d",
                                "swathingActCounter"="#74c476",
                                "teddingActCounter"="#a1d99b")) +
  theme_bw() +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste("DOY",sep = ""), y = paste("Number of actions / Number of farm plots", sep = ""))

x11()
aff <- aff + theme(legend.position="none")
act <- act + theme(legend.position="none")
grid.arrange(aff,act,ncol=1, nrow = 2) # Ajouter la légende à la main
savePlot(filename = paste0("save/affordances_plot.png"), device = dev.cur())

####### 7.4.3 Plot the crop results [OPTIONAL] #######
forage_results[which(forage_results$idExpl==2),1] <- 1
forage_results[which(forage_results$idExpl==3),1] <- 2
forage_results[which(forage_results$idExpl==5),1] <- 3
forage_results[which(forage_results$idExpl==6),1] <- 4
forage_results[which(forage_results$idExpl==7),1] <- 5
forage_results[which(forage_results$idExpl==10),1] <- 6
forage_results[which(forage_results$idExpl==11),1] <- 7
forage_results[which(forage_results$idExpl==12),1] <- 8
forage_results[which(forage_results$idExpl==14),1] <- 9
forage_results[which(forage_results$idExpl==16),1] <- 10
f <- ggplot(forage_results, aes(x=day, y=mean_wsi, colour= factor(idExpl))) +
  geom_line() + xlim(c(0, max(forage_results$day))) + ylim(c(0, max(forage_results$mean_wsi))) +
  theme_bw() +
  scale_color_manual(name="Farmer ID", 
                     labels = c("1", 
                                "2", 
                                "4", 
                                "5", 
                                "7",
                                "8",
                                "9",
                                "10"), 
                     values = c("1"="#6A3D9A", 
                                "2"="#CAB2D6", 
                                "3"="#FF7F00", 
                                "4"="#FDBF6F", 
                                "5"="#E31A1C",
                                "6"="#FB9A99",
                                "7"="#33A02C",
                                "8" ="#B2DF8A",
                                "9"="#1F78B4",
                                "10"="#A6CEE3")) +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste(" ",sep = ""), y = paste("Mean WSI", sep = "")) + ggtitle("Forage")
# legend_f <- get_legend(f)

meadow_results[which(meadow_results$idExpl==2),1] <- 1
meadow_results[which(meadow_results$idExpl==3),1] <- 2
meadow_results[which(meadow_results$idExpl==5),1] <- 3
meadow_results[which(meadow_results$idExpl==6),1] <- 4
meadow_results[which(meadow_results$idExpl==7),1] <- 5
meadow_results[which(meadow_results$idExpl==10),1] <- 6
meadow_results[which(meadow_results$idExpl==11),1] <- 7
meadow_results[which(meadow_results$idExpl==12),1] <- 8
meadow_results[which(meadow_results$idExpl==14),1] <- 9
meadow_results[which(meadow_results$idExpl==16),1] <- 10
m <- ggplot(meadow_results, aes(x=day, y=mean_wsi, colour= factor(idExpl))) +
  geom_line() + xlim(c(0, max(meadow_results$day))) + ylim(c(0, max(meadow_results$mean_wsi))) +
  theme_bw() +
  scale_color_manual(name="Farmer ID", 
                     labels = c("1", 
                                "2", 
                                "3", 
                                "5",
                                "6"), 
                     values = c("1"="#6A3D9A", 
                                "2"="#CAB2D6", 
                                "3"="#FF7F00", 
                                "4"="#FDBF6F", 
                                "5"="#E31A1C",
                                "6"="#FB9A99",
                                "7"="#33A02C",
                                "8" ="#B2DF8A",
                                "9"="#1F78B4",
                                "10"="#A6CEE3")) +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste(" ",sep = ""), y = paste("Mean WSI", sep = "")) + ggtitle("Meadow")
# legend_m <- get_legend(m)

winterCereal_results[which(winterCereal_results$idExpl==2),1] <- 1
winterCereal_results[which(winterCereal_results$idExpl==3),1] <- 2
winterCereal_results[which(winterCereal_results$idExpl==5),1] <- 3
winterCereal_results[which(winterCereal_results$idExpl==6),1] <- 4
winterCereal_results[which(winterCereal_results$idExpl==7),1] <- 5
winterCereal_results[which(winterCereal_results$idExpl==10),1] <- 6
winterCereal_results[which(winterCereal_results$idExpl==11),1] <- 7
winterCereal_results[which(winterCereal_results$idExpl==12),1] <- 8
winterCereal_results[which(winterCereal_results$idExpl==14),1] <- 9
winterCereal_results[which(winterCereal_results$idExpl==16),1] <- 10
wc <- ggplot(winterCereal_results, aes(x=day, y=mean_wsi, colour= factor(idExpl))) +
  geom_line() + xlim(c(0, max(winterCereal_results$day))) + ylim(c(0, max(winterCereal_results$mean_wsi))) +
  theme_bw() +
  scale_color_manual(name="Farmer ID", 
                     labels = c("2", 
                                "4", 
                                "5", 
                                "7",
                                "8",
                                "9",
                                "10"), 
                     values = c("1"="#6A3D9A", 
                                "2"="#CAB2D6", 
                                "3"="#FF7F00", 
                                "4"="#FDBF6F", 
                                "5"="#E31A1C",
                                "6"="#FB9A99",
                                "7"="#33A02C",
                                "8" ="#B2DF8A",
                                "9"="#1F78B4",
                                "10"="#A6CEE3")) +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste(" ",sep = ""), y = paste("Mean WSI", sep = "")) + ggtitle("Winter cereal")
# legend_wc <- get_legend(wc)

springCereal_results[which(springCereal_results$idExpl==2),1] <- 1
springCereal_results[which(springCereal_results$idExpl==3),1] <- 2
springCereal_results[which(springCereal_results$idExpl==5),1] <- 3
springCereal_results[which(springCereal_results$idExpl==6),1] <- 4
springCereal_results[which(springCereal_results$idExpl==7),1] <- 5
springCereal_results[which(springCereal_results$idExpl==10),1] <- 6
springCereal_results[which(springCereal_results$idExpl==11),1] <- 7
springCereal_results[which(springCereal_results$idExpl==12),1] <- 8
springCereal_results[which(springCereal_results$idExpl==14),1] <- 9
springCereal_results[which(springCereal_results$idExpl==16),1] <- 10
sc <- ggplot(springCereal_results, aes(x=day, y=mean_wsi, colour= factor(idExpl))) +
  geom_line() + xlim(c(0, max(springCereal_results$day))) + ylim(c(0, max(springCereal_results$mean_wsi))) +
  theme_bw()   +
  scale_color_manual(name="Farmer ID", 
                                  labels = c("2", 
                                             "4", 
                                             "5", 
                                             "7",
                                             "8",
                                             "9",
                                             "10"), 
                                  values = c("1"="#6A3D9A", 
                                             "2"="#CAB2D6", 
                                             "3"="#FF7F00", 
                                             "4"="#FDBF6F", 
                                             "5"="#E31A1C",
                                             "6"="#FB9A99",
                                             "7"="#33A02C",
                                             "8" ="#B2DF8A",
                                             "9"="#1F78B4",
                                             "10"="#A6CEE3")) +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste("DOY",sep = ""), y = paste("Mean WSI", sep = "")) + ggtitle("Spring cereal")
# legend_sc <- get_legend(sc)

# x11()
# grid.arrange(legend_f,legend_m,legend_wc,legend_sc, ncol=4, nrow=1, widths=c(4,4,4,4)) #Pour ajouter une légende à la main
f <- f + theme(legend.position="none")
m <- m + theme(legend.position="none")
wc <- wc + theme(legend.position="none")
sc <- sc + theme(legend.position="none")
x11()
grid.arrange(f,m,wc,sc, ncol=1, nrow=4, widths=c(2.5)) #Rest à ajouter une légende à la main
savePlot(filename = paste0("save/wsi_plot.png"), device = dev.cur())

####### 7.4.4 Plot the indicator results [OPTIONAL] #######
q <- ggplot(ind_results, aes(x=day, y=q_cumec)) +
  geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$q_cumec))) +
  theme_bw()  +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste(" ",sep = ""), y = paste("Q [cumec]", sep = ""))

inquiries <- ggplot(ind_results, aes(x=day, y=inquiries)) +
  geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$inquiries))) +
  theme_bw()  +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste(" ",sep = ""), y = paste("Farmer inquiries", sep = ""))

unrespect <- ggplot(ind_results, aes(x=day, y=unrespect)) +
  geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$unrespect))) +
  theme_bw()  +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste(" ",sep = ""), y = paste("Irrigation under restriction", sep = ""))

abandoned <- ggplot(ind_results, aes(x=day, y=abandoned)) +
  geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$abandoned))) +
  theme_bw()  +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste(" ",sep = ""), y = paste("Abandoned plot", sep = ""))

release <- ggplot(ind_results, aes(x=day, y=release_cumec)) +
  geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$release_cumec))) +
  theme_bw()  +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste("DOY",sep = ""), y = paste("Release [cumec]", sep = ""))

q <- q + theme(legend.position="none")
inquiries <- inquiries + theme(legend.position="none")
unrespect <- unrespect + theme(legend.position="none")
abandoned <- abandoned + theme(legend.position="none")
release <- release + theme(legend.position="none")
x11()
grid.arrange(q,inquiries,unrespect,abandoned,release, ncol=1, nrow=5, widths=c(2.5)) #Reste à ajouter une légende à la main
savePlot(filename = paste0("save/indicators_plot.png"), device = dev.cur())

####### 7.4.5 Map the indicator results [OPTIONAL] #######
require(maptools) ; require(raster)
rpg=readShapePoly('save/watasit.shp');
proj=CRS("+init=epsg:2154")
rpg$ID_PARCEL<-as.numeric(as.character(rpg$ID_PARCEL)) #transformation en "numeric", car c'est importé comme un "factor"
rpg$SURF_PARC<-as.numeric(as.character(rpg$SURF_PARC))
rpg$CODE_CULTU<-as.numeric(as.character(rpg$CLASS_CULT))
rpg$CODE_GROUP<-as.numeric(as.character(rpg$CODE_GROUP))
rpg$TYPE<-as.numeric(as.character(rpg$ID_TYPE))
rpg$ID_EXPL<-as.numeric(as.character(rpg$ID_FARM))
rpg$EPAIS<-as.numeric(as.character(rpg$EPAIS))
rpg$MAXSTORAGE<-as.numeric(as.character(rpg$MAXSTORAGE))
rpg$RU<-as.numeric(as.character(rpg$ARGILE))
rpg$LIMON<-as.numeric(as.character(rpg$LIMON))
rpg$SABLE<-as.numeric(as.character(rpg$SABLE))
rpg$CONDHYDRO<-as.numeric(as.character(rpg$CONDHYDRO))
rpg$FILTSOIL<-as.numeric(as.character(rpg$FILTSOIL))
rpg$TYPESOL<-as.numeric(as.character(rpg$ID_TYPESOI))
rpg$CODE_ASA<-as.numeric(as.character(rpg$ID_ASA))
projection(rpg)<-proj
class(rpg)
# x11(); plot(rpg)
# text(coordinates(rpg), labels=rpg$ID_EXPL,font=2,cex=0.5)
rpg_df<-as.data.frame(rpg)
rpg_df[which(rpg_df$ID_EXPL==1),6] <- NA
rpg_df[which(rpg_df$ID_EXPL==2),6] <- 1
rpg_df[which(rpg_df$ID_EXPL==3),6] <- 2
rpg_df[which(rpg_df$ID_EXPL==4),6] <- NA
rpg_df[which(rpg_df$ID_EXPL==5),6] <- 3
rpg_df[which(rpg_df$ID_EXPL==6),6] <- 4
rpg_df[which(rpg_df$ID_EXPL==7),6] <- 5
rpg_df[which(rpg_df$ID_EXPL==8),6] <- NA
rpg_df[which(rpg_df$ID_EXPL==9),6] <- NA
rpg_df[which(rpg_df$ID_EXPL==10),6] <- 6
rpg_df[which(rpg_df$ID_EXPL==11),6] <- 7
rpg_df[which(rpg_df$ID_EXPL==12),6] <- 8
rpg_df[which(rpg_df$ID_EXPL==13),6] <- NA
rpg_df[which(rpg_df$ID_EXPL==14),6] <- 9
rpg_df[which(rpg_df$ID_EXPL==15),6] <- NA
rpg_df[which(rpg_df$ID_EXPL==16),6] <- 10
rpg_df$abandoned <- rpg_df$ID_PARCEL
for (i in 1:length(rpg_df$ID_PARCEL)){
  id_plot <- rpg_df[i,1] 
  id <- which(abandoned_plots[,1] == id_plot)
  state <- abandoned_plots[id,2]
  state <- sum(state); if(state>0){state ==1}
  if (is.numeric0(state)==F)
  rpg_df[i,24] <- state
}
rpg_df[which(rpg_df$abandoned==2),24] <- 1
rpg = SpatialPolygonsDataFrame(as(rpg, "SpatialPolygons"), data=rpg_df, match.ID=F)
# x11(); plot(rpg)
# text(coordinates(rpg), labels=rpg$abandoned,font=2,cex=0.5)
rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 0] <- "#ffffff"
rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 1] <- "#ef8a62"
x11(); plot(rpg, col=rpg@data$COLOUR, axes = T)
title(main = "Irrigated crop state at the end of the 2017 campaign", sub = "(Fingers are farmer ID, parcels with no ID are not part \n of farms using the collective irrigation network)")
text(coordinates(rpg), labels=rpg$ID_EXPL,font=2,cex=0.5)
legend("topright",   # location of legend
       legend = levels(factor(rpg@data$abandoned, levels=c('Not abandoned','Abandoned'))), # categories or elements to render in
       # the legend
       fill = c("#ffffff","#ef8a62")) # color palette to use to fill objects in legend.
savePlot(filename = paste0("save/abandonedCrops_plot.png"), device = dev.cur())
