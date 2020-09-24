#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      RWatASit[SESMOpaper]      ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script pilots the WatASit ABM (Richard et al., 2020)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2017-2020, by
# B. Bonté -> make RCormas function to get/set Cormas attributes/probes
# B. Richard -> make this script and associated R funcions and
# generate case study data and parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 0. Cleaning & time reset #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all.names =T)); start_time <- Sys.time()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1. Settings #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1.1 Load functions #######
for(FileName in list.files("Rfunctions/", pattern="\\.[Rr]$")){ source(file.path("Rfunctions/",FileName))}

####### 1.2 Load libraries #######
load <- c(require(zoo), require (multiplex), require(tidyr),require(dplyr),require(doParallel))
if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); }

####### 1.3 Core parallelism [OPTIONAL] #######
cores <- parallel:::detectCores(); registerDoParallel(cores-1);

####### 1.4 Activate results saving [OPTIONAL] #######
saveRes <- T # Choose T or F. If F -> no saving, if T -> saving activated


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Config and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2.1 General #######
simulation_name <- "Aspres_baseline_noRespect_cor_timeWindow10-16h" # To be specified
date_start_sim <- as.Date("2017-05-01", "%Y-%m-%d") # To be specified
date_end_sim <- as.Date("2017-09-30", "%Y-%m-%d") # To be specified

####### 2.2 Importation of meteo input #######
climate_file_name <- 'climate_buech_2016-2017.csv' # Specified a file that keeps similar format (see example in climate_file folder)
# climate_file_name <- "climate_buech_2005-2017.csv"
input_meteo <- computeClimateInput(climate_file_name, date_start_sim, date_end_sim)
DOY_start = 1; DOY_stop = dim(input_meteo)[1]

####### 2.3 Model setting #######
model_name = "COWAT" # Cormas model name o be specified
parcel_file = "WatASit[1.1.2_CoMSES].pcl" # Cormas parcel file To be specified
init_method = "INIT_2017_54x44" # Cormas init method to be specified
scenario <- "Baseline" # Choose "BaselineRespect" (no coordination) or "AlternativeRespect" (coordination) and farmers respect irrigation restrictions, or "Baseline" or "Alternative when farmers do not respect irrigation restrictions


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. Run WatASit #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (day in DOY_start:DOY_stop){
  if (day == DOY_start) {
    ####### 3.1 Connexion and opening of WatASit model #######
    r <- openModel(model_name, parcel_file)
    r <- setInit(init_method) # Initialization choice
    
    ####### 3.2 Choose of WatASit initial state and time step function (scenarios) #######
    r <- setStep(paste0("R_go",scenario,"Step:"))  #scenario <- "Baseline" or "Alternative"
    r <- initSimu() #initialize Cormas simulation
  }
  
  ####### 3.3 Make 24 steps simulation (= 1 day) #######
    ####### Update Cormas Meteo #######
    P<-input_meteo$P; setAttributesOfEntities("p", "Meteo", 1, as.numeric(P[day])) # Precipitation conditions of the day 
    if (day >= 2) {
      p_cumSevenDays = sum(c(P[max(1,(day-7)):(day-1)]), na.rm = TRUE)
      p_cumTwelveDays = sum(c(P[max(1,(day-12)):(day-1)]), na.rm = TRUE)
    } else {
      p_cumSevenDays = 0
      p_cumTwelveDays = 0
    }
    setAttributesOfEntities("p_cumSevenDays", "Meteo", 1, p_cumSevenDays)
    setAttributesOfEntities("p_cumTwelveDays", "Meteo", 1, p_cumTwelveDays)
    
    ####### Run simulation of 24 hours #######
    cat("Simulation of day",day, "Precip = ",P[day]," mm","\n")
    r <- runSimu(duration = 24)
    response <- gettext(r[[2]])
    if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {stop("RUN STOPPED",call.=FALSE)} # To check if runSimu is done
  
  ####### 3.4 Get the farm plots irrigations by parcel ID #######
  fp_idExpl <- getAttributesOfEntities("idExpl","FarmPlot")
  fp_irri <- getAttributesOfEntities("irriDailyDose","FarmPlot")
  fp_ask <- getAttributesOfEntities("askActCounter","FarmPlot") #ajout 3/8/2020
  fp_flood <- getAttributesOfEntities("floodActCounter","FarmPlot") #ajout 3/8/2020
  fp_floodDR <- getAttributesOfEntities("floodDRActCounter","FarmPlot") #ajout 3/8/2020
  fp_askAff <- getAttributesOfEntities("askAffCounter","FarmPlot") #ajout 11/8/2020
  fp_floodAff <- getAttributesOfEntities("floodAffCounter","FarmPlot") #ajout 11/8/2020
  fp_floodDRAff <- getAttributesOfEntities("floodDRAffCounter","FarmPlot") #ajout 11/8/2020
  c_label <- getAttributesOfEntities("labelNb", "Ecrop") #ajout 20/8/2020
  c_daysFromLastIrrigation <- getAttributesOfEntities("daysFromLastIrrigation", "Ecrop") #ajout 20/8/2020
  c_abandonedState <- getAttributesOfEntities("abandonedState", "Ecrop") #ajout 20/8/2020
  c_irriState <- getAttributesOfEntities("irriState", "Ecrop") #ajout 20/8/2020
  fp_irri_df <- data.frame(fp_irri, fp_idExpl, fp_ask, fp_flood, fp_floodDR, fp_askAff, fp_floodAff, fp_floodDRAff, c_daysFromLastIrrigation,
                           c_abandonedState, c_irriState); fp_irri_df$day = day;  #modif 3/8/2020; 11/8/2020
  dailyRes <- fp_irri_df[,c(1,2,4,6,8,10,12,14,16,18,20,22,23)]
  if (day == 1) {
    mat = matrix(NA, ncol = length(dailyRes$id), nrow = dim(input_meteo)[1]); colnames(mat)<-dailyRes$id
    askAct_mat = floodAct_mat = floodDRAct_mat = askAff_mat = floodAff_mat = floodDRAff_mat = daysFromLastIrrigation_mat = abandonedState_mat = irriState_mat = mat 
    fp_codeAsa <- getAttributesOfEntities("codeAsa","FarmPlot")
    fp_dt<- data.frame(fp_idExpl, codeAsa = fp_codeAsa[,2], cropLabel = c_label[,2])
    }
  for (i in 1:length(dailyRes$id)){
      askAct_mat[day,i] <- dailyRes[i,4]
      floodAct_mat[day,i] <- dailyRes[i,5]
      floodDRAct_mat[day,i] <- dailyRes[i,6]
      askAff_mat[day,i] <- dailyRes[i,7]
      floodAff_mat[day,i] <- dailyRes[i,8]
      floodDRAff_mat[day,i] <- dailyRes[i,9]
      daysFromLastIrrigation_mat[day,i] <- dailyRes[i,10]
      abandonedState_mat[day,i] <- dailyRes[i,11]
      irriState_mat[day,i] <- dailyRes[i,12]
  }
} 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Save simulation results #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (saveRes) {
  dir.create("save/simulations_WatASit_SESMO/"); dir.create(paste0("save/simulations_WatASit_SESMO/",simulation_name))
  saveResultsWatASit(fp_dt, askAct_mat, floodAct_mat, floodDRAct_mat, askAff_mat, floodAff_mat, floodDRAff_mat, daysFromLastIrrigation_mat,
                     abandonedState_mat, irriState_mat, simulation_name)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### End of simulation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
end_time <- Sys.time(); simu_time = end_time - start_time; cat ("................................................................",
                                                                "\n","Simulation time is ", round(simu_time,2), "minutes", "\n")
warnings()