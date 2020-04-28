#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######      Function to generate Optirrig climate input      #########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2020, Feb, by B. Richard
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

computeClimateInput <- function(climateFileName, date_start_sim, date_end_sim){
  
  data_meteo      = read.csv(paste0('climatefile/',climateFileName), header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
  dates       <- as.Date(data_meteo$Date, "%Y-%m-%d");
  input_meteo <- zoo(data_meteo, dates); input_meteo <- window(input_meteo, start = date_start_sim, end = date_end_sim); glimpse(input_meteo)
  input_meteo <- as.data.frame(input_meteo); glimpse(input_meteo)
  input_meteo$Date <- as.Date(input_meteo$Date, "%Y-%m-%d");
  input_meteo$year <- as.numeric(as.character(input_meteo$year))
  input_meteo$month <- as.numeric(as.character(input_meteo$month))
  input_meteo$day <- as.numeric(as.character(input_meteo$day))
  input_meteo$doy <- as.numeric(as.character(input_meteo$doy))
  input_meteo$P <- as.numeric(as.character(input_meteo$P))
  input_meteo$etp <- as.numeric(as.character(input_meteo$etp))
  input_meteo$Rg <- as.numeric(as.character(input_meteo$Rg))
  input_meteo$T <- as.numeric(as.character(input_meteo$T))
  return(input_meteo)
  
}