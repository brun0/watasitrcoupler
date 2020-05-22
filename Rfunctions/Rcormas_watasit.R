#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######      Function to pilot WatASit with RCormas      #########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2020, April, by B. Richard
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RCormas = function(case_study_name, day, DOY_start_coupling, DOY_stop_coupling, input_meteo, modelName, parcelFile, init, scenario) { 
  
  if (day == DOY_start_coupling) { print("OKKKKK")
    ####### 1 Connexion and opening of WatASit model #######
    r <- openModel(modelName, parcelFile = parcelFile)
    r <- setInit(init) # Initialization choice
   
    ####### 2 Choose of WatASit initial state and time step function (scenarios) #######
    r <- setStep(paste0("R_go",scenario,"Step:"))  #scenario <- "Baseline" or "Alternative"
    r <- initSimu() #initialize Cormas simulation
  }
  
  if (day <=  DOY_stop_coupling) {
  ####### 4 Update Cormas Meteo #######
  P<-input_meteo$P; setAttributesOfEntities("p", "Meteo", 1, as.numeric(P[day])) # Precipitation conditions of the day 
  p_forecast = sum(as.numeric(P[day:(day+2)]), na.rm = TRUE); if (p_forecast > 0) {p_forecast = 1}; setAttributesOfEntities("p_forecast", "Meteo", 1, p_forecast) # Precipitation forecast for the next 3 days
  if (day == 1) {p_cumTenDays = 0}
  if (day == 2) {p_cumTenDays = P[day-1]}
  for (i in 3:10){if (day == i){ p_cumTenDays = sum(as.numeric(P[(day-(i-1)):(day-1)]), na.rm = TRUE) }}
  if (day >= 11) {p_cumTenDays = sum(as.numeric(P[(day-10):(day-1)]), na.rm = TRUE)}
  setAttributesOfEntities("p_cumTenDays", "Meteo", 1, p_cumTenDays) # Calculate cumulative precipitation for the last 10 days
  p_cumTwelveDays = sum(c(p_cumTenDays,as.numeric(P[(day-12):(day-11)])), na.rm = TRUE)
  setAttributesOfEntities("p_cumTwelveDays", "Meteo", 1, p_cumTwelveDays) # Calculate cumulative precipitation for the last 12 days
  p_cumFifteenDays = sum(c(p_cumTwelveDays,as.numeric(P[(day-15):(day-13)])), na.rm = TRUE)
  setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, p_cumFifteenDays) # Calculate cumulative precipitation for the last 15 days
  
  ####### 5 Run coupled simulation of 24 hours #######
  r <- runSimu(duration = 24)
  response <- gettext(r[[2]])
  if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {stop("RUN STOPPED",call.=FALSE)} # To check if runSimu is done
  
  ####### 6 Get the farm plots irrigations by parcel ID #######
  fp_idExpl <- getAttributesOfEntities("idExpl","FarmPlot")
  fp_irri <- getAttributesOfEntities("irriDailyDose","FarmPlot")
  fp_irri_df <- data.frame(fp_irri, fp_idExpl); fp_irri_df$day = day;
  
  }
  
  return(fp_irri_df[,1:2])
  }
  