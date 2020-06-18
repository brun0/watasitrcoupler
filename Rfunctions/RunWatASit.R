#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######      Function to run WatASit from R     #########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2020, April, by B. Richard
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RunWatASit = function(daily_step, input_meteo) { 
  
  ####### A Update Cormas Meteo #######
  P<-input_meteo$P; setAttributesOfEntities("p", "Meteo", 1, as.numeric(P[daily_step])) # Precipitation conditions of the day 
  p_forecast = sum(as.numeric(P[daily_step:(daily_step+2)]), na.rm = TRUE); if (p_forecast > 0) {p_forecast = 1}; setAttributesOfEntities("p_forecast", "Meteo", 1, p_forecast) # Precipitation forecast for the next 3 days
  if (daily_step == 1) {p_cumTenDays = 0}
  if (daily_step == 2) {p_cumTenDays = P[daily_step-1]}
  for (i in 3:10){if (daily_step == i){ p_cumTenDays = sum(as.numeric(P[(daily_step-(i-1)):(daily_step-1)]), na.rm = TRUE) }}
  if (daily_step >= 11) {p_cumTenDays = sum(as.numeric(P[(daily_step-10):(daily_step-1)]), na.rm = TRUE)}
  setAttributesOfEntities("p_cumTenDays", "Meteo", 1, p_cumTenDays) # Calculate cumulative precipitation for the last 10 days
  p_cumTwelveDays = sum(c(p_cumTenDays,as.numeric(P[(daily_step-12):(daily_step-11)])), na.rm = TRUE)
  setAttributesOfEntities("p_cumTwelveDays", "Meteo", 1, p_cumTwelveDays) # Calculate cumulative precipitation for the last 12 days
  p_cumFifteenDays = sum(c(p_cumTwelveDays,as.numeric(P[(daily_step-15):(daily_step-13)])), na.rm = TRUE)
  setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, p_cumFifteenDays) # Calculate cumulative precipitation for the last 15 days
  
  ####### B Run coupled simulation of 24 hours #######
  r <- runSimu(duration = 24)
  response <- gettext(r[[2]])
  if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {stop("RUN STOPPED",call.=FALSE)} # To check if runSimu is done
  
  ####### C Get the farm plots irrigations by parcel ID #######
  fp_idExpl <- getAttributesOfEntities("idExpl","FarmPlot")
  fp_irri <- getAttributesOfEntities("irriDailyDose","FarmPlot")
  fp_irri_df <- data.frame(fp_irri, fp_idExpl); fp_irri_df$day = daily_step;
  
  }
  
  return(fp_irri_df[,1:2])

  