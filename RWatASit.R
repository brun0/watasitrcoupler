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
load <- c(require(png), require(RColorBrewer), require(zoo), require (multiplex), require(tidyr),require(ggplot2),require(dplyr),require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

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
# farmers_aff <- data.frame(id = NULL, day = NULL, askAff = NULL, dosCounter = NULL)
# crops_results <- data.frame(id = NULL, day = NULL, labelNb = NULL, irriDailyDose = NULL)
farmers_aff <- data.frame()
farmers_act <- data.frame()
# farmers_results <- data.frame()
crops_results <- data.frame()
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
     f_aff <- data.frame(f_aff1, f_aff2, f_aff3, f_aff4, f_aff5, f_aff6, f_aff7, f_aff8, f_aff9, f_aff10, by = "id"); f_aff$day = day
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
     f_act <- data.frame(f_act1, f_act2, f_act3, f_act4, f_act5, f_act6, f_act7, f_act8, f_act9, f_act10, by = "id"); f_act$day = day
     farmers_act <- farmers_act %>% rbind(f_act)
     # f_results <- data.frame(f_aff1, f_aff2, f_aff3, f_aff4, f_aff5, f_aff6, f_aff7, f_aff8, f_aff9, f_aff10, f_act1, f_act2, f_act3, f_act4, f_act5, f_act6, f_act7, f_act8, f_act9, f_act10, by = "id"); f_results$day = day
     # farmers_results <- farmers_results %>% rbind(f_results)
     
  
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
####### 7.1 Get the farmers affordances and actions [OPTIONAL] #######
farmers_aff <- farmers_aff %>% select(-id.1,-id.2,-id.3,-id.4,-id.5,-id.6,-id.7,-id.8,-id.9,-by)
farmers_act <- farmers_act %>% select(-id.1,-id.2,-id.3,-id.4,-id.5,-id.6,-id.7,-id.8,-id.9,-by)

####### 7.2 Get the crops state [OPTIONAL] #######
c1_res <- subset.data.frame(crops_results, labelNb == 1) # Toutes les crops de type 1
c2_res <- subset.data.frame(crops_results, labelNb == 2)
c3_res <- subset.data.frame(crops_results, labelNb == 3)
c4_res <- subset.data.frame(crops_results, labelNb == 4)
c5_res <- subset.data.frame(crops_results, labelNb == 5)
c6_res <- subset.data.frame(crops_results, labelNb == 6)
c7_res <- subset.data.frame(crops_results, labelNb == 7)
c8_res <- subset.data.frame(crops_results, labelNb == 8)

####### 7.3 Plot the simulation results [OPTIONAL] #######
id_names <- c(
  `1` = " Farmer 1",
  `2` = " Farmer 2",
  `3` = " Farmer 3",
  `4` = " Farmer 4",
  `5` = " Farmer 5",
  `6` = " Farmer 6",
  `7` = " Farmer 7",
  `8` = " Farmer 8",
  `9` = " Farmer 9",
  `10` = " Farmer 10"
)

#img <- png::readPNG("/index.png"); g_pic <- rasterGrob(img, interpolate=TRUE)

x11()
farmers_aff %>%
  gather(Affordance,value, askAffCounter,doSEAffCounter,floodAffCounter, floodDRAffCounter, mowingAffCounter, pickingAffCounter, pressingAffCounter, reapingAffCounter, swathingAffCounter, teddingAffCounter) %>%
  ggplot(aes(x=day, y=value, colour=Affordance)) +
  facet_wrap(~ factor(id, levels=c('1','2','3','4', '5', '6', '7', '8', '9', '10')), scales = "fixed", ncol = 2, labeller = as_labeller(id_names)) +
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
                     values = c("askAffCounter"="#6A3D9A", 
                                "doSEAffCounter"="#CAB2D6", 
                                "floodAffCounter"="#FF7F00", 
                                "floodDRAffCounter"="#FDBF6F", 
                                "mowingAffCounter"="#E31A1C",
                                "pickingAffCounter"="#FB9A99",
                                "pressingAffCounter"="#33A02C",
                                "reapingAffCounter"="#B2DF8A",
                                "swathingAffCounter"="#1F78B4",
                                "teddingAffCounter"="#A6CEE3")) +
  theme_bw() +
  theme(legend.box = "vertical",  legend.position = c(1.25,0.5), plot.margin = unit(c(2,14,2,2), "lines"),
          panel.grid = element_line(),
          panel.background = element_blank(),
          legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
          plot.title = element_text(hjust = 0.5)) +
  labs(x = paste("DOY",sep = ""), y = paste("Number of affordance", sep = ""))
  

x11()
farmers_act %>%
  gather(Action,value, askActCounter,doSEActCounter,floodActCounter, floodDRActCounter, mowingActCounter, pickingActCounter, pressingActCounter, reapingActCounter, swathingActCounter, teddingActCounter) %>%
  ggplot(aes(x=day, y=value, colour=Action)) +
  facet_wrap(~ factor(id, levels=c('1','2','3','4', '5', '6', '7', '8', '9', '10')), scales = "fixed", ncol = 2, labeller = as_labeller(id_names)) +
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
                     values = c("askActCounter"="#6A3D9A", 
                                "doSEActCounter"="#CAB2D6", 
                                "floodActCounter"="#FF7F00", 
                                "floodDRActCounter"="#FDBF6F", 
                                "mowingActCounter"="#E31A1C",
                                "pickingActCounter"="#FB9A99",
                                "pressingActCounter"="#33A02C",
                                "reapingActCounter"="#B2DF8A",
                                "swathingActCounter"="#1F78B4",
                                "teddingActCounter"="#A6CEE3")) +
  theme_bw() +
  theme(legend.box = "vertical",  legend.position = c(1.25,0.5), plot.margin = unit(c(2,14,2,2), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste("DOY",sep = ""), y = paste("Number of action", sep = ""))
 