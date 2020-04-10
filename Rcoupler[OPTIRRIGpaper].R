#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R coupler OPTIRRIG paper      ######################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script makes the WatASit model from Cormas plateform to communicate 
# with the Optirrig model implemented in R Software
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2019, October, by
# B. Bonté -> make RCormas function to get/set Cormas attributes/probes
# M. Delmas -> make adapted daily Optirrig function, adapt it for 
# meadows
# B. Richard -> make work together, make optiParams funcion and
# generate Optirrig climate file with specific R script
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# You must initialize Cormas before run this script
rm(list=ls()); sessionInfo(); start_time <- Sys.time()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1. R Settings #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1.1 Load functions [REQUIRED] #######
for(FileName in list.files("Rfunctions/", pattern="\\.[Rr]$")){ source(file.path("Rfunctions/",FileName)); }
is.numeric0 <- function(x) {
  identical(x, numeric(0))
}
####### 1.2 Load libraries [REQUIRED] #######
load <- c(require(matrixStats), require(gridExtra), require(RColorBrewer), require(zoo), require (multiplex), require(tidyr),require(ggplot2),require(dplyr),require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

####### 1.3 Core parallelism [OPTIONAL] #######
cores <- parallel:::detectCores(); registerDoParallel(cores-2);


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Simulation Settings and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2.1 Specification of case study, year and duration [REQUIRED] #######
case_study_name <- "Aspres"
date_start_sim <- as.Date("2016-10-15", "%Y-%m-%d"); date_end_sim <- as.Date("2017-10-14", "%Y-%m-%d")

####### 2.2 Importation of meteo data input  [REQUIRED] #######
data_meteo      = read.csv('climatefile/climate_buech_2016-2017.csv', header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
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
str(input_meteo)

####### 2.3 Generation of an Optirrig paramfile for each WatASit plots  #######
# list_idParcel <- optiParams('paramfiles/', case_study_name, 'watasit_winterCereals.csv', 'paramDBAllCereals.csv','climate_buech_2016-2017.csv', as.numeric(format(date_end_sim,"%Y")), 1, 365, 'irrig_file_watasit.dat')
list_idParcel <- optiParams('paramfiles/', case_study_name, 'watasit_allIrrigatedCereals.csv', 'paramDBAllCereals.csv','climate_buech_2016-2017.csv', as.numeric(format(date_end_sim,"%Y")), 1, 365, 'irrig_file_noirrigation.dat')


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
####### A - Optirrig without irrigation #######
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. Initialization of Optirrig model #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
param_frame <- data.frame(); irr <- data.frame()
for (i in 1:length(list_idParcel)){
  ####### 3.1 Load params of each plot #######
  param = read.csv(paste0('paramfiles/paramfiles_',case_study_name,'/',list_idParcel[i],'/parF', list_idParcel[i],'.csv'), header = TRUE,sep=",",dec = ".",stringsAsFactor=FALSE)
  
  ####### 3.2 Create frame with all parameters #######
  param_frame <- rbind(param_frame, param)
  
  ####### 3.3 Create frame with all irrigation vectors  #######
 I1   = as.vector(input_meteo$day) ; I1[] = 0; I2 = I1 # I1 is surface irrigation and I2  I2 is deep buried irrigation (I2 is null)
 irr = rbind(irr,I1)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Run simulation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4.1 Run Optirrig simulation without WataSit from October, 15 2016 (DOY 1) to May, 01, 2017 (DOY 120+78=198) to simulate the new state of crops out of irrigation campaign #######
  ####### 4.1.1 Initialize optirrig on day 1 #######
    day = 1
    cstes_list <- list()
    inval_list <- list()
    vect_list <- list()
    lai_list <- list()
    wsi_list <- list()
    etr_list<- list()
    etm_list <- list()
    tt_list <- list()
    tt_p_list <- list()
    lai_p_list <- list()
    tdm_list <- list()
    tdm_p_list <- list()
    
    for (i in 1:length(list_idParcel)){
      init <- init_optirr(param_frame[i,], input_meteo)
      cstes = init$cstes; cstes_list <- rbind(cstes_list, cstes) # Constants
      inval = init$inval; inval_list <-  rbind(inval_list, inval) # Calculation values for which the history is not required (list of values)
      vect  = init$vect ; vect_list <-  rbind(vect_list, vect) # Vector of stored state variables as time series in vectors (list of vectors)
    }
  
  ####### 4.1.2 Simulate Optirrig on other days #######
  for (day in 2:dim(input_meteo)[1]){
    inval2_list <- list()
    vect2_list <- list()
    for (i in 1:length(list_idParcel)){
      cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
      # I1 = I2 # I2 is deep irrigation (buried drip), I2 is null
      param<-param_frame[i,]; cstes<-cstes_list[i,];  inval<-inval_list[i,]; vect<-vect_list[i,]
      optirday = daily_optirr(param,
                              input_meteo,
                              cstes,
                              inval,
                              vect,
                              I1, # Surface irrigation
                              I2, # Deep irrigation (buried drip)
                              day) # Time step
      # inval2 = optirday$inval ; inval2_list <- rbind(inval2_list, inval2) ; inval_list[i,] <- inval2 # New constants
      # vect2  = optirday$vect ; vect2_list <- rbind(vect2_list, vect2) ; vect_list[i,] <- vect2 # New vectors
      inval2 = optirday$inval ; inval2_list[[i]] <- inval2 ; inval_list[i,] <- inval2 # New constants
      vect2  = optirday$vect ; vect2_list[[i]] <- vect2 ; vect_list[i,] <- vect2 # New vectors
      lai_list[[i]] <- vect2$LAI; wsi_list[[i]]<- vect2$Sw_lai; etr_list[[i]]<-vect2$ETR
      etm_list[[i]]<-vect2$ETM; tt_list[[i]]<- vect2$TT; tt_p_list[[i]]<-vect2$TT_p; lai_p_list[[i]]<- vect2$LAI_p
      tdm_list[[i]]<- vect2$TDM; tdm_p_list[[i]]<- vect2$TDM_p
    }
  } 

    ####### 4.1.3 Savings #######  
  # res_without_irrig <- vect2_list; write.csv(res_without_irrig,"save/simulations_iEMSs2020/res_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
  lai_without_irrig <- lai_list#; write.csv(lai_without_irrig,"save/simulations_iEMSs2020/lai_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n") 
  wsi_without_irrig <- wsi_list#; write.csv(wsi_without_irrig,"save/simulations_iEMSs2020/wsi_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n") 
  etr_without_irrig <- etr_list#; write.csv(etr_without_irrig,"save/simulations_iEMSs2020/etr_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n") 
  etm_without_irrig <- etm_list#; write.csv(etm_without_irrig,"save/simulations_iEMSs2020/etm_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n") 
  tt_without_irrig <- tt_list#; write.csv(tt_without_irrig,"save/simulations_iEMSs2020/tt_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n") 
  tt_p_without_irrig <- tt_p_list#; write.csv(tt_p_without_irrig,"save/simulations_iEMSs2020/tt_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n") 
  lai_p_without_irrig <- lai_p_list#; write.csv(lai_p_without_irrig,"save/simulations_iEMSs2020/lai_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n") 
  tdm_without_irrig <- tdm_list#; write.csv(tdm_without_irrig,"save/simulations_iEMSs2020/tdm_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n") 
  tdm_p_without_irrig <- tdm_p_list#; write.csv(tdm_p_without_irrig,"save/simulations_iEMSs2020/tdm_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n") 
  
# mean_lai_without_irrig <- sapply( lai_without_irrig, FUN=function(items) {mean( unlist(items))})    
lai_without_irrig_allplots <-sapply(lai_without_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)})
write.csv(lai_without_irrig_allplots,"save/simulations_iEMSs2020/lai_without_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_lai_without_irrig <- rowMeans(lai_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_lai_without_irrig,"save/simulations_iEMSs2020/row_mean_lai_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_lai_without_irrig <- rowMins(lai_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_lai_without_irrig,"save/simulations_iEMSs2020/row_min_lai_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_lai_without_irrig <- rowMaxs(lai_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_lai_without_irrig,"save/simulations_iEMSs2020/row_max_lai_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

lai_without_irrig_wintercereals <-lai_without_irrig_allplots[,c(2,3,4,6,7,8,9,10,11,12,14,15,17,19,20,21,22,23,24,25)]
row_mean_lai_without_irrig_wintercereals <- rowMeans(lai_without_irrig_wintercereals) #Moyenne de l'ensemble des parcelles par jour
lai_without_irrig_springcereals <-lai_without_irrig_allplots[,c(1,5,16,18)]
row_mean_lai_without_irrig_springcereals <- rowMeans(lai_without_irrig_springcereals) #Moyenne de l'ensemble des parcelles par jour
lai_without_irrig_corn <-lai_without_irrig_allplots[,c(13, 13)]
row_mean_lai_without_irrig_corn <- rowMeans(lai_without_irrig_corn) #Moyenne de l'ensemble des parcelles par jour

wsi_without_irrig_allplots <-sapply(wsi_without_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(wsi_without_irrig_allplots,"save/simulations_iEMSs2020/wsi_without_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_wsi_without_irrig <- rowMeans(wsi_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_wsi_without_irrig,"save/simulations_iEMSs2020/row_mean_wsi_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_wsi_without_irrig <- rowMins(wsi_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_wsi_without_irrig,"save/simulations_iEMSs2020/row_min_wsi_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_wsi_without_irrig <- rowMaxs(wsi_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_wsi_without_irrig,"save/simulations_iEMSs2020/row_max_wsi_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

wsi_without_irrig_wintercereals <-wsi_without_irrig_allplots[,c(2,3,4,6,7,8,9,10,11,12,14,15,17,19,20,21,22,23,24,25)]
row_mean_wsi_without_irrig_wintercereals <- rowMeans(wsi_without_irrig_wintercereals) #Moyenne de l'ensemble des parcelles par jour
wsi_without_irrig_springcereals <-wsi_without_irrig_allplots[,c(1,5,16,18)]
row_mean_wsi_without_irrig_springcereals <- rowMeans(wsi_without_irrig_springcereals) #Moyenne de l'ensemble des parcelles par jour
wsi_without_irrig_corn <-wsi_without_irrig_allplots[,c(13, 13)]
row_mean_wsi_without_irrig_corn <- rowMeans(wsi_without_irrig_corn) #Moyenne de l'ensemble des parcelles par jour

etr_without_irrig_allplots <-sapply(etr_without_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(etr_without_irrig_allplots,"save/simulations_iEMSs2020/etr_without_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_etr_without_irrig <- rowMeans(etr_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_etr_without_irrig,"save/simulations_iEMSs2020/row_mean_etr_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_etr_without_irrig <- rowMins(etr_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_etr_without_irrig,"save/simulations_iEMSs2020/row_min_etr_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_etr_without_irrig <- rowMaxs(etr_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_etr_without_irrig,"save/simulations_iEMSs2020/row_max_etr_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

etm_without_irrig_allplots <-sapply(etm_without_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(etm_without_irrig_allplots,"save/simulations_iEMSs2020/etm_without_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_etm_without_irrig <- rowMeans(etm_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_etm_without_irrig,"save/simulations_iEMSs2020/row_mean_etm_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_etm_without_irrig <- rowMins(etm_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_etm_without_irrig,"save/simulations_iEMSs2020/row_mins_etm_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_etm_without_irrig <- rowMaxs(etm_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_etm_without_irrig,"save/simulations_iEMSs2020/row_max_etm_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tt_without_irrig_allplots <-sapply(tt_without_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tt_without_irrig_allplots,"save/simulations_iEMSs2020/tt_without_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tt_without_irrig <- rowMeans(tt_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tt_without_irrig,"save/simulations_iEMSs2020/row_mean_tt_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tt_without_irrig <- rowMins(tt_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_tt_without_irrig,"save/simulations_iEMSs2020/row_min_tt_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tt_without_irrig <- rowMaxs(tt_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tt_without_irrig,"save/simulations_iEMSs2020/row_max_tt_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tt_p_without_irrig_allplots <-sapply(tt_p_without_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tt_p_without_irrig_allplots,"save/simulations_iEMSs2020/tt_p_without_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tt_p_without_irrig <- rowMeans(tt_p_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tt_p_without_irrig,"save/simulations_iEMSs2020/row_mean_tt_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tt_p_without_irrig <- rowMins(tt_p_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_tt_p_without_irrig,"save/simulations_iEMSs2020/row_min_tt_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tt_p_without_irrig <- rowMaxs(tt_p_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tt_p_without_irrig,"save/simulations_iEMSs2020/row_max_tt_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

lai_p_without_irrig_allplots <-sapply(lai_p_without_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(lai_p_without_irrig_allplots,"save/simulations_iEMSs2020/lai_p_without_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_lai_p_without_irrig <- rowMeans(lai_p_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_lai_p_without_irrig,"save/simulations_iEMSs2020/row_mean_lai_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_lai_p_without_irrig <- rowMins(lai_p_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_lai_p_without_irrig,"save/simulations_iEMSs2020/row_min_lai_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_lai_p_without_irrig <- rowMaxs(lai_p_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_lai_p_without_irrig,"save/simulations_iEMSs2020/row_max_lai_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tdm_without_irrig_allplots <-sapply(tdm_without_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tdm_without_irrig_allplots,"save/simulations_iEMSs2020/tdm_without_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tdm_without_irrig <- rowMeans(tdm_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tdm_without_irrig,"save/simulations_iEMSs2020/row_mean_tdm_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tdm_without_irrig <- rowMins(tdm_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tdm_without_irrig,"save/simulations_iEMSs2020/row_mean_tdm_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tdm_without_irrig <- rowMaxs(tdm_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tdm_without_irrig,"save/simulations_iEMSs2020/row_max_tdm_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tdm_p_without_irrig_allplots <-sapply(tdm_p_without_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tdm_p_without_irrig_allplots,"save/simulations_iEMSs2020/tdm_p_without_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tdm_p_without_irrig <- rowMeans(tdm_p_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tdm_p_without_irrig,"save/simulations_iEMSs2020/row_mean_tdm_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tdm_p_without_irrig <- rowMins(tdm_p_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_tdm_p_without_irrig,"save/simulations_iEMSs2020/row_min_tdm_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tdm_p_without_irrig <- rowMaxs(tdm_p_without_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tdm_p_without_irrig,"save/simulations_iEMSs2020/row_max_tdm_p_without_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

X11(); plot(as.Date(input_meteo$Date),row_mean_lai_without_irrig, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Leaf Area Index");
lines(as.Date(input_meteo$Date),row_min_lai_without_irrig, col="black", lty = "dotted", lwd=1); lines(as.Date(input_meteo$Date),row_max_lai_without_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_lai_without_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(3,1), col=c("red", "blue","black"),
       legend = c("Max", "Mean", "Min"))
savePlot(filename = "save/simulations_iEMSs2020/lai.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_lai_without_irrig_wintercereals, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Leaf Area Index");
lines(as.Date(input_meteo$Date),row_mean_lai_without_irrig_springcereals, col="black", lty = "dotted", lwd=1); lines(as.Date(input_meteo$Date),row_mean_lai_without_irrig_corn, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_lai_without_irrig_wintercereals,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(3,1), col=c("red", "blue","black"),
       legend = c("Corn", "WinterCereals", "SpringCereals"))
# savePlot(filename = "save/simulations_iEMSs2020/lai_comp_cereals.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_without_irrig, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");
lines(as.Date(input_meteo$Date),row_min_wsi_without_irrig, col="black", lty = "dotted", lwd=1); lines(as.Date(input_meteo$Date),row_max_wsi_without_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_wsi_without_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("bottomleft", lty=c(3,1), col=c("red", "blue","black"),
       legend = c("Max", "Mean", "Min"))
savePlot(filename = "save/simulations_iEMSs2020/wsi.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_without_irrig_wintercereals, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");
lines(as.Date(input_meteo$Date),row_mean_wsi_without_irrig_springcereals, col="black", lty = "dotted", lwd=1); lines(as.Date(input_meteo$Date),row_mean_wsi_without_irrig_corn, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_wsi_without_irrig_wintercereals,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("bottomleft", lty=c(3,1), col=c("red", "blue","black"),
       legend = c("Corn", "WinterCereals", "SpringCereals"))
# savePlot(filename = "save/simulations_iEMSs2020/wsi_comp_cereals.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_etr_without_irrig, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Evapotranspiration");
lines(as.Date(input_meteo$Date),row_mean_etm_without_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_etr_without_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Max (ETM)", "Real (ETR"))
savePlot(filename = "save/simulations_iEMSs2020/et.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_tt_without_irrig, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Temperature sum (degree-day)");
lines(as.Date(input_meteo$Date),row_mean_tt_p_without_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_tt_without_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Potential", "Real"))
savePlot(filename = "save/simulations_iEMSs2020/tt.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_lai_without_irrig, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Leaf Area Index");
lines(as.Date(input_meteo$Date),row_mean_lai_p_without_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_lai_without_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Potential", "Real"))
savePlot(filename = "save/simulations_iEMSs2020/lai_comp.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_tdm_without_irrig, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Dry matter content");
lines(as.Date(input_meteo$Date),row_mean_tdm_p_without_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_tdm_without_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Potential", "Real"))
savePlot(filename = "save/simulations_iEMSs2020/tdm.png", device = dev.cur())

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
####### B - Optirrig with irrigation every 15 days #######
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

list_idParcel <- optiParams('paramfiles/', case_study_name, 'watasit_allIrrigatedCereals.csv', 'paramDBAllCereals.csv','climate_buech_2016-2017.csv', as.numeric(format(date_end_sim,"%Y")), 1, 365, 'irrig_file_irrigation15d.dat')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. Initialization of Optirrig model #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
param_frame <- data.frame(); irr <- data.frame()
for (i in 1:length(list_idParcel)){
  ####### 3.1 Load params of each plot #######
  param = read.csv(paste0('paramfiles/paramfiles_',case_study_name,'/',list_idParcel[i],'/parF', list_idParcel[i],'.csv'), header = TRUE,sep=",",dec = ".",stringsAsFactor=FALSE)
  
  ####### 3.2 Create frame with all parameters #######
  param_frame <- rbind(param_frame, param)
  
  ####### 3.3 Create frame with all irrigation vectors  #######
  I1   = as.vector(input_meteo$day) ; I1[] = 0; I2 = I1 # I1 is surface irrigation and I2 is deep buried irrigation (I2 is null)
  I1[200] = 44; I1[215] = 44; I1[230] = 44; I1[245] = 44; I1[260] = 44; I1[275] = 44; I1[290] = 44; I1[305] = 44; I1[320] = 44;
  irr = rbind(irr,I1)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Run simulation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4.1 Run Optirrig simulation without WataSit from October, 15 2016 (DOY 1) to May, 01, 2017 (DOY 120+78=198) to simulate the new state of crops out of irrigation campaign #######
####### 4.1.1 Initialize optirrig on day 1 #######
day = 1
cstes_list <- list()
inval_list <- list()
vect_list <- list()
lai_list <- list()
wsi_list <- list()
etr_list<- list()
etm_list <- list()
tt_list <- list()
tt_p_list <- list()
lai_p_list <- list()
tdm_list <- list()
tdm_p_list <- list()
for (i in 1:length(list_idParcel)){
  init <- init_optirr(param_frame[i,], input_meteo)
  cstes = init$cstes; cstes_list <- rbind(cstes_list, cstes) # Constants
  inval = init$inval; inval_list <-  rbind(inval_list, inval) # Calculation values for which the history is not required (list of values)
  vect  = init$vect ; vect_list <-  rbind(vect_list, vect) # Vector of stored state variables as time series in vectors (list of vectors)
}

####### 4.1.2 Simulate Optirrig on other days #######
for (day in 2:dim(input_meteo)[1]){
  inval2_list <- list()
  vect2_list <- list()
  for (i in 1:length(list_idParcel)){
    cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
    # I1 = I2 # I2 is deep irrigation (buried drip), I2 is null
    param<-param_frame[i,]; cstes<-cstes_list[i,];  inval<-inval_list[i,]; vect<-vect_list[i,]
    optirday = daily_optirr(param,
                            input_meteo,
                            cstes,
                            inval,
                            vect,
                            I1, # Surface irrigation
                            I2, # Deep irrigation (buried drip)
                            day) # Time step
    # inval2 = optirday$inval ; inval2_list <- rbind(inval2_list, inval2) ; inval_list[i,] <- inval2 # New constants
    # vect2  = optirday$vect ; vect2_list <- rbind(vect2_list, vect2) ; vect_list[i,] <- vect2 # New vectors
    inval2 = optirday$inval ; inval2_list[[i]] <- inval2 ; inval_list[i,] <- inval2 # New constants
    vect2  = optirday$vect ; vect2_list[[i]] <- vect2 ; vect_list[i,] <- vect2 # New vectors
    lai_list[[i]] <- vect2$LAI; wsi_list[[i]]<- vect2$Sw_lai; etr_list[[i]]<-vect2$ETR
    etm_list[[i]]<-vect2$ETM; tt_list[[i]]<- vect2$TT; tt_p_list[[i]]<-vect2$TT_p; lai_p_list[[i]]<- vect2$LAI_p
    tdm_list[[i]]<- vect2$TDM; tdm_p_list[[i]]<- vect2$TDM_p
  }
} 
####### 4.1.3 Savings #######
lai_with_irrig <- lai_list
wsi_with_irrig <- wsi_list 
etr_with_irrig <- etr_list
etm_with_irrig <- etm_list
tt_with_irrig <- tt_list
tt_p_with_irrig <- tt_p_list 
lai_p_with_irrig <- lai_p_list
tdm_with_irrig <- tdm_list
tdm_p_with_irrig <- tdm_p_list
  
lai_with_irrig_allplots <-sapply(lai_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)})
write.csv(lai_with_irrig_allplots,"save/simulations_iEMSs2020/lai_with_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_lai_with_irrig <- rowMeans(lai_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_lai_with_irrig,"save/simulations_iEMSs2020/row_mean_lai_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_lai_with_irrig <- rowMins(lai_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_lai_with_irrig,"save/simulations_iEMSs2020/row_min_lai_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_lai_with_irrig <- rowMaxs(lai_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_lai_with_irrig,"save/simulations_iEMSs2020/row_max_lai_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

wsi_with_irrig_allplots <-sapply(wsi_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(wsi_with_irrig_allplots,"save/simulations_iEMSs2020/wsi_with_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_wsi_with_irrig <- rowMeans(wsi_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_wsi_with_irrig,"save/simulations_iEMSs2020/row_mean_wsi_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_wsi_with_irrig <- rowMins(wsi_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_wsi_with_irrig,"save/simulations_iEMSs2020/row_min_wsi_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_wsi_with_irrig <- rowMaxs(wsi_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_wsi_with_irrig,"save/simulations_iEMSs2020/row_max_wsi_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

etr_with_irrig_allplots <-sapply(etr_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(etr_with_irrig_allplots,"save/simulations_iEMSs2020/etr_with_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_etr_with_irrig <- rowMeans(etr_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_etr_with_irrig,"save/simulations_iEMSs2020/row_mean_etr_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_etr_with_irrig <- rowMins(etr_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_etr_with_irrig,"save/simulations_iEMSs2020/row_min_etr_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_etr_with_irrig <- rowMaxs(etr_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_etr_with_irrig,"save/simulations_iEMSs2020/row_max_etr_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

etm_with_irrig_allplots <-sapply(etm_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(etm_with_irrig_allplots,"save/simulations_iEMSs2020/etm_with_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_etm_with_irrig <- rowMeans(etm_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_etm_with_irrig,"save/simulations_iEMSs2020/row_mean_etm_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_etm_with_irrig <- rowMins(etm_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_etm_with_irrig,"save/simulations_iEMSs2020/row_mins_etm_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_etm_with_irrig <- rowMaxs(etm_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_etm_with_irrig,"save/simulations_iEMSs2020/row_max_etm_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tt_with_irrig_allplots <-sapply(tt_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tt_with_irrig_allplots,"save/simulations_iEMSs2020/tt_with_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tt_with_irrig <- rowMeans(tt_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tt_with_irrig,"save/simulations_iEMSs2020/row_mean_tt_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tt_with_irrig <- rowMins(tt_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_tt_with_irrig,"save/simulations_iEMSs2020/row_min_tt_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tt_with_irrig <- rowMaxs(tt_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tt_with_irrig,"save/simulations_iEMSs2020/row_max_tt_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tt_p_with_irrig_allplots <-sapply(tt_p_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tt_p_with_irrig_allplots,"save/simulations_iEMSs2020/tt_p_with_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tt_p_with_irrig <- rowMeans(tt_p_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tt_p_with_irrig,"save/simulations_iEMSs2020/row_mean_tt_p_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tt_p_with_irrig <- rowMins(tt_p_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_tt_p_with_irrig,"save/simulations_iEMSs2020/row_min_tt_p_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tt_p_with_irrig <- rowMaxs(tt_p_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tt_p_with_irrig,"save/simulations_iEMSs2020/row_max_tt_p_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

lai_p_with_irrig_allplots <-sapply(lai_p_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(lai_p_with_irrig_allplots,"save/simulations_iEMSs2020/lai_p_with_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_lai_p_with_irrig <- rowMeans(lai_p_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_lai_p_with_irrig,"save/simulations_iEMSs2020/row_mean_lai_p_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_lai_p_with_irrig <- rowMins(lai_p_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_lai_p_with_irrig,"save/simulations_iEMSs2020/row_min_lai_p_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_lai_p_with_irrig <- rowMaxs(lai_p_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_lai_p_with_irrig,"save/simulations_iEMSs2020/row_max_lai_p_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tdm_with_irrig_allplots <-sapply(tdm_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tdm_with_irrig_allplots,"save/simulations_iEMSs2020/tdm_with_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tdm_with_irrig <- rowMeans(tdm_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tdm_with_irrig,"save/simulations_iEMSs2020/row_mean_tdm_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tdm_with_irrig <- rowMins(tdm_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tdm_with_irrig,"save/simulations_iEMSs2020/row_mean_tdm_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tdm_with_irrig <- rowMaxs(tdm_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tdm_with_irrig,"save/simulations_iEMSs2020/row_max_tdm_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tdm_p_with_irrig_allplots <-sapply(tdm_p_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tdm_p_with_irrig_allplots,"save/simulations_iEMSs2020/tdm_p_with_irrig_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tdm_p_with_irrig <- rowMeans(tdm_p_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tdm_p_with_irrig,"save/simulations_iEMSs2020/row_mean_tdm_p_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tdm_p_with_irrig <- rowMins(tdm_p_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_tdm_p_with_irrig,"save/simulations_iEMSs2020/row_min_tdm_p_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tdm_p_with_irrig <- rowMaxs(tdm_p_with_irrig_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tdm_p_with_irrig,"save/simulations_iEMSs2020/row_max_tdm_p_with_irrig.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

X11(); plot(as.Date(input_meteo$Date),row_mean_lai_with_irrig, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Leaf Area Index");
lines(as.Date(input_meteo$Date),row_min_lai_with_irrig, col="black", lty = "dotted", lwd=1); lines(as.Date(input_meteo$Date),row_max_lai_with_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_lai_with_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(3,1), col=c("red", "blue","black"),
       legend = c("Max", "Mean", "Min"))
savePlot(filename = "save/simulations_iEMSs2020/lai_irrig.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_with_irrig, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");
lines(as.Date(input_meteo$Date),row_min_wsi_with_irrig, col="black", lty = "dotted", lwd=1); lines(as.Date(input_meteo$Date),row_max_wsi_with_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_wsi_with_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("bottomleft", lty=c(3,1), col=c("red", "blue","black"),
       legend = c("Max", "Mean", "Min"))
savePlot(filename = "save/simulations_iEMSs2020/wsi_irrig.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_etr_with_irrig, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Evapotranspiration");
lines(as.Date(input_meteo$Date),row_mean_etm_with_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_etr_with_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Max (ETM)", "Real (ETR"))
savePlot(filename = "save/simulations_iEMSs2020/et_irrig.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_tt_with_irrig, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Temperature sum (degree-day)");
lines(as.Date(input_meteo$Date),row_mean_tt_p_with_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_tt_with_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Potential", "Real"))
savePlot(filename = "save/simulations_iEMSs2020/tt_irrig.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_lai_with_irrig, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Leaf Area Index");
lines(as.Date(input_meteo$Date),row_mean_lai_p_with_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_lai_with_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Potential", "Real"))
savePlot(filename = "save/simulations_iEMSs2020/lai_comp_irrig.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_tdm_with_irrig, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Dry matter content");
lines(as.Date(input_meteo$Date),row_mean_tdm_p_with_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_tdm_with_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Potential", "Real"))
savePlot(filename = "save/simulations_iEMSs2020/tdm_irrig.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_lai_with_irrig, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Leaf Area Index");
lines(as.Date(input_meteo$Date),row_mean_lai_without_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_lai_with_irrig,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Not irrigated", "Irrigated"))
savePlot(filename = "save/simulations_iEMSs2020/lai_irrig_vs_not.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_without_irrig, type = "l", lwd=1.5, , lty = "dotted", axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");
lines(as.Date(input_meteo$Date),row_mean_wsi_with_irrig, col="blue", lwd=1.5)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_wsi_without_irrig,type="l",lwd=1.5,col="red", lty = "dotted"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("bottomleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Not irrigated", "Irrigated"))
savePlot(filename = "save/simulations_iEMSs2020/wsi_irrig_vs_not.png", device = dev.cur())



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
####### C - Optirrig with WatASit #######
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. Initialization of Optirrig model #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
param_frame <- data.frame(); irr <- data.frame()
for (i in 1:length(list_idParcel)){
  ####### 3.1 Load params of each plot #######
  param = read.csv(paste0('paramfiles/paramfiles_',case_study_name,'/',list_idParcel[i],'/parF', list_idParcel[i],'.csv'), header = TRUE,sep=",",dec = ".",stringsAsFactor=FALSE)
  
  ####### 3.2 Create frame with all parameters #######
  param_frame <- rbind(param_frame, param)
  
  ####### 3.3 Create frame with all irrigation vectors  #######
  I1   = as.vector(input_meteo$day) ; I1[] = 0; I2 = I1 # I1 is surface irrigation and I2  I2 is deep buried irrigation (I2 is null)
  irr = rbind(irr,I1)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Run simulation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4.1 Run Optirrig simulation without WataSit from October, 15 2016 (DOY 1) to May, 01, 2017 (DOY 120+78=198) to simulate the new state of crops out of irrigation campaign #######
####### 4.1.1 Initialize optirrig on day 1 #######
day = 1
cstes_list <- list()
inval_list <- list()
vect_list <- list()
lai_list <- list()
wsi_list <- list()
etr_list<- list()
etm_list <- list()
tt_list <- list()
tt_p_list <- list()
lai_p_list <- list()
tdm_list <- list()
tdm_p_list <- list()

for (i in 1:length(list_idParcel)){
  init <- init_optirr(param_frame[i,], input_meteo)
  cstes = init$cstes; cstes_list <- rbind(cstes_list, cstes) # Constants
  inval = init$inval; inval_list <-  rbind(inval_list, inval) # Calculation values for which the history is not required (list of values)
  vect  = init$vect ; vect_list <-  rbind(vect_list, vect) # Vector of stored state variables as time series in vectors (list of vectors)
}

####### 4.1.2 Simulate Optirrig on other days #######
for (day in 2:197){
  inval2_list <- list()
  vect2_list <- list()
  for (i in 1:length(list_idParcel)){
    cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
    # I1 = I2 # I2 is deep irrigation (buried drip), I2 is null
    param<-param_frame[i,]; cstes<-cstes_list[i,];  inval<-inval_list[i,]; vect<-vect_list[i,]
    optirday = daily_optirr(param,
                            input_meteo,
                            cstes,
                            inval,
                            vect,
                            I1, # Surface irrigation
                            I2, # Deep irrigation (buried drip)
                            day) # Time step
    # inval2 = optirday$inval ; inval2_list <- rbind(inval2_list, inval2) ; inval_list[i,] <- inval2 # New constants
    # vect2  = optirday$vect ; vect2_list <- rbind(vect2_list, vect2) ; vect_list[i,] <- vect2 # New vectors
    inval2 = optirday$inval ; inval2_list[[i]] <- inval2 ; inval_list[i,] <- inval2 # New constants
    vect2  = optirday$vect ; vect2_list[[i]] <- vect2 ; vect_list[i,] <- vect2 # New vectors
    lai_list[[i]] <- vect2$LAI; wsi_list[[i]]<- vect2$Sw_lai; etr_list[[i]]<-vect2$ETR
    etm_list[[i]]<-vect2$ETM; tt_list[[i]]<- vect2$TT; tt_p_list[[i]]<-vect2$TT_p; lai_p_list[[i]]<- vect2$LAI_p
    tdm_list[[i]]<- vect2$TDM; tdm_p_list[[i]]<- vect2$TDM_p
  }
} 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 5. WatASit initialization #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 5.1 Connexion and opening of WatASit model [REQUIRED] #######
r <- openModel("COWAT", parcelFile="WatASit[OPTIRRIGpaper].pcl")

####### 5.2 Activation of Cormas probes [OPTIONAL] #######
# probe_names <- c("abandonedCropEvent", "ASAinquiries", "exceedMaxWithdrawalEvent", "qIntake", "unrespectRestrictionEvent", "sumQOfEwaterReleases", "f1IrrigatedPlotNb", "f2IrrigatedPlotNb", "f3IrrigatedPlotNb", "f5IrrigatedPlotNb", "f6IrrigatedPlotNb", "f7IrrigatedPlotNb", "f10IrrigatedPlotNb", "f11IrrigatedPlotNb", "f12IrrigatedPlotNb","f14IrrigatedPlotNb", "f16IrrigatedPlotNb")

####### 5.3 Choose of WatASit initial state and time step function (scenarios) [REQUIRED] #######
r <- setInit("INIT_2017_54x44") # Initialization choice
scenario <- "Baseline" # Choose scenario
# scenario <- "Alternative"
r <- setStep(paste0("R_go",scenario,"Step:"))
r <- initSimu() #initialize Cormas simulation

####### 5.4 Create results dataFrame #######
farmers_aff <- data.frame()
farmers_act <- data.frame()
crops_results <- data.frame()
meadow_results <- data.frame()
forage_results <- data.frame()
winterCereal_results <- data.frame()
springCereal_results <- data.frame()
ind_results <- data.frame()
farmplots_aff <- data.frame()
farmplots_act <- data.frame()
fp_irrigations <- data.frame()

####### 5.5 Run WatASit-Optirrig coupled simulation from DOY 198 (1er mai) during the irrigation campaign #######
for (day in 198:dim(input_meteo)[1]){
# for (day in 11:(10 + cormas_sim_day_nb)){
      ####### 5.5.1 Update Cormas Meteo #######
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
  
    ####### 5.5.2 Run coupled simulation of 24 hours #######
     r <- runSimu(duration = 24)
     response <- gettext(r[[2]])
     if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {stop("RUN STOPPED",call.=FALSE)} # To check if runSimu is done
     
     ####### 5.5.3 Get the farmers affordances and actions from Cormas [OPTIONAL] #######
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
     
     ####### 5.5.4 Get the farm plots affordances and actions from Cormas [OPTIONAL] #######
     fp_aff1 <- getAttributesOfEntities("askAffCounter","FarmPlot") 
     fp_aff2 <- getAttributesOfEntities("doSEAffCounter","FarmPlot") 
     fp_aff3 <- getAttributesOfEntities("floodAffCounter","FarmPlot") 
     fp_aff4 <- getAttributesOfEntities("floodDRAffCounter","FarmPlot") 
     fp_aff5 <- getAttributesOfEntities("mowingAffCounter","FarmPlot") 
     fp_aff6 <- getAttributesOfEntities("pickingAffCounter","FarmPlot")
     fp_aff7 <- getAttributesOfEntities("pressingAffCounter","FarmPlot")
     fp_aff8 <- getAttributesOfEntities("reapingAffCounter","FarmPlot")
     fp_aff9 <- getAttributesOfEntities("swathingAffCounter","FarmPlot")
     fp_aff10 <- getAttributesOfEntities("teddingAffCounter","FarmPlot")
     fp_idExpl <- getAttributesOfEntities("idExpl","FarmPlot")
     fp_aff <- data.frame(fp_aff1, fp_aff2, fp_aff3, fp_aff4, fp_aff5, fp_aff6, fp_aff7, fp_aff8, fp_aff9, fp_aff10, fp_idExpl); fp_aff$day = day;
     farmplots_aff <- farmplots_aff %>% rbind(fp_aff)
     
     fp_act1 <- getAttributesOfEntities("askActCounter","FarmPlot") 
     fp_act2 <- getAttributesOfEntities("doSEActCounter","FarmPlot") 
     fp_act3 <- getAttributesOfEntities("floodActCounter","FarmPlot") 
     fp_act4 <- getAttributesOfEntities("floodDRActCounter","FarmPlot") 
     fp_act5 <- getAttributesOfEntities("mowingActCounter","FarmPlot") 
     fp_act6 <- getAttributesOfEntities("pickingActCounter","FarmPlot")
     fp_act7 <- getAttributesOfEntities("pressingActCounter","FarmPlot")
     fp_act8 <- getAttributesOfEntities("reapingActCounter","FarmPlot")
     fp_act9 <- getAttributesOfEntities("swathingActCounter","FarmPlot")
     fp_act10 <- getAttributesOfEntities("teddingActCounter","FarmPlot")
     fp_act <- data.frame(fp_act1, fp_act2, fp_act3, fp_act4, fp_act5, fp_act6, fp_act7, fp_act8, fp_act9, fp_act10, fp_idExpl); fp_act$day = day;
     farmplots_act <- farmplots_act %>% rbind(fp_act)
     
     ####### 5.5.4 Get the farm plots affordances and actions from Cormas [OPTIONAL] #######
     fp_irri <- getAttributesOfEntities("irriDailyDose","FarmPlot")
     fp_irri_df <- data.frame(fp_irri, fp_idExpl); fp_irri_df$day = day;
     fp_irrigations<- fp_irrigations %>% rbind(fp_irri_df)
     
     ####### 5.5.5 Get the crop results from Cormas [OPTIONAL] #######
     ####### 5.5.5.1  By parcel ID [OPTIONAL] #######
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
     c_obs14<- getAttributesOfEntities("failed_irrigations_classA", "Ecrop")
     c_obs15<- getAttributesOfEntities("failed_irrigations_classB", "Ecrop")
     c_obs16<- getAttributesOfEntities("failed_irrigations_classC", "Ecrop")
     c_obs17<- getAttributesOfEntities("failed_irrigations_classD", "Ecrop")
     c_obs <- data.frame(c_obs1, c_obs2, c_obs3, c_obs4, c_obs5, c_obs6, c_obs7, c_obs8, c_obs9, c_obs10, c_obs11, c_obs12, c_obs13, c_obs14, c_obs15, c_obs16, c_obs17); c_obs$day = day
     crops_results <- crops_results %>% rbind(c_obs)
     
     ####### 5.5.5.2  By crop ID [OPTIONAL] #######
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
     
     ####### 5.5.6 Get the simulation indicators from Cormas [OPTIONAL] #######
     ind1 <- getAttributesOfEntities("q", "EwaterIntake")
     ind2 <- getAttributesOfEntities("inquiries", "IrrigatorAssociation")
     ind3 <- getAttributesOfEntities("nb", "TotalAbandonedCropNb");
     ind4 <- getAttributesOfEntities("nb", "UnrespectRestrictionNb")
     ind5 <- getAttributesOfEntities("totalRelease", "RiverReach")
     ind <- data.frame(day, q_cumec = ind1$q, inquiries = ind2$inquiries, abandoned = ind3$nb, unrespect = ind4$nb, release_cumec = ind5$totalRelease)
     ind_results <- rbind(ind_results, ind)
      
      ####### 5.5.7 Simulate the new state of crops with Optirrig #######
                if (day != 1) { inval2_list <- list(); vect2_list <- list();   lai_day_list <- list();  wsi_day_list <- list()
                  # for (i in 1:length(c_obs11$idParcel)){
                for (i in 1:length(list_idParcel)){
                    cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
                    irr[i,day]  <- fp_irri$irriDailyDose[i]; irr<-as.matrix(irr); I1 =irr[i,] # Update irrigation from Cormas
                    param<-param_frame[i,]; cstes<-cstes_list[i,]; inval<-inval_list[i,]; vect<-vect_list[i,]
                    optirday = daily_optirr(param,
                                            input_meteo,
                                            cstes,
                                            inval,
                                            vect,
                                            I1, # Surface irrigation
                                            I2, # Deep irrigation (buried drip)
                                            day) # Time step
                    inval2 = optirday$inval ; inval2_list[[i]] <- inval2 ; inval_list[i,] <- inval2 # New constants
                    vect2  = optirday$vect ; vect2_list[[i]] <- vect2 ; vect_list[i,] <- vect2 # New vectors
                    lai_list[[i]] <- vect2$LAI; wsi_list[[i]]<- vect2$Sw_lai
                    lai_day_list[[i]] <- vect2$LAI[day]; wsi_day_list[[i]]<- vect2$Sw_lai[day]; etr_list[[i]]<-vect2$ETR
                    etm_list[[i]]<-vect2$ETM; tt_list[[i]]<- vect2$TT; tt_p_list[[i]]<-vect2$TT_p; lai_p_list[[i]]<- vect2$LAI_p
                    tdm_list[[i]]<- vect2$TDM; tdm_p_list[[i]]<- vect2$TDM_p
                    
                  }
                } 
     
      
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 6.Get new crop state from Optirrig simulations #######
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ####### 6.1 Get new crop states #######
    new_wsi_day <- sapply(wsi_day_list, FUN=function(items) {mean( unlist(items))})
    new_wsi_day_idParcel <- c(list_idParcel,rep(0,length(c_obs11$idParcel)-length(list_idParcel)))
    new_wsi_day_df <- data.frame(wsi = new_wsi_day, idParcel = list_idParcel)
    all_parcels_wsi <- data.frame( idParcel = c_obs11$idParcel, wsi = 0)
    for (j in 1:length(all_parcels_wsi$idParcel)){
      id_plot <- all_parcels_wsi[j,1]
      id <- which(new_wsi_day_df[,2] == id_plot)
      wsi <- new_wsi_day_df[id,1]
      if (is.numeric0(wsi)==F){all_parcels_wsi[j,2] <- wsi}
    }
    
  ####### 6.2 Set the new state of crops in cormas #######
     setAttributesOfEntities("wsi", "Ecrop", all_parcels_wsi$idParcel, all_parcels_wsi$wsi)
     # setAttributesOfEntities("cropMaturitySignal", "Ecrop", c_obs1$id, new_lai_day)
}
cat('Simulation of day:', day, "/", dim(input_meteo)[1],"....",input_meteo$date[day],"....P[mm/day] =",P[day], "....P_cumTenDays[mm/day] =",p_cumTenDays,"\n")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 6. Calculate simulation time [OPTIONAL] #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
end_time <- Sys.time(); simu_time = end_time - start_time; cat ("................................................................",
                                                                "\n","Simulation time is ", round(simu_time,2), "minutes", "\n")

lai_coupled <- lai_list
wsi_coupled <- wsi_list 
etr_coupled <- etr_list
etm_coupled <- etm_list
tt_coupled <- tt_list
tt_p_coupled <- tt_p_list 
lai_p_coupled <- lai_p_list
tdm_coupled <- tdm_list
tdm_p_coupled <- tdm_p_list

lai_coupled_allplots <-sapply(lai_coupled, FUN=function(top){ apply( as.data.frame(top), 1, mean)})
write.csv(lai_coupled_allplots,"save/simulations_iEMSs2020/lai_coupled_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_lai_coupled <- rowMeans(lai_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_lai_coupled,"save/simulations_iEMSs2020/row_mean_lai_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_lai_coupled <- rowMins(lai_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_lai_coupled,"save/simulations_iEMSs2020/row_min_lai_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_lai_coupled <- rowMaxs(lai_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_lai_coupled,"save/simulations_iEMSs2020/row_max_lai_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

wsi_coupled_allplots <-sapply(wsi_coupled, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(wsi_coupled_allplots,"save/simulations_iEMSs2020/wsi_coupled_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_wsi_coupled <- rowMeans(wsi_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_wsi_coupled,"save/simulations_iEMSs2020/row_mean_wsi_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_wsi_coupled <- rowMins(wsi_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_wsi_coupled,"save/simulations_iEMSs2020/row_min_wsi_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_wsi_coupled <- rowMaxs(wsi_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_wsi_coupled,"save/simulations_iEMSs2020/row_max_wsi_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

etr_coupled_allplots <-sapply(etr_coupled, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(etr_coupled_allplots,"save/simulations_iEMSs2020/etr_coupled_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_etr_coupled <- rowMeans(etr_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_etr_coupled,"save/simulations_iEMSs2020/row_mean_etr_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_etr_coupled <- rowMins(etr_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_etr_coupled,"save/simulations_iEMSs2020/row_min_etr_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_etr_coupled <- rowMaxs(etr_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_etr_coupled,"save/simulations_iEMSs2020/row_max_etr_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

etm_coupled_allplots <-sapply(etm_coupled, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(etm_coupled_allplots,"save/simulations_iEMSs2020/etm_coupled_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_etm_coupled <- rowMeans(etm_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_etm_coupled,"save/simulations_iEMSs2020/row_mean_etm_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_etm_coupled <- rowMins(etm_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_etm_coupled,"save/simulations_iEMSs2020/row_mins_etm_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_etm_coupled <- rowMaxs(etm_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_etm_coupled,"save/simulations_iEMSs2020/row_max_etm_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tt_coupled_allplots <-sapply(tt_coupled, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tt_coupled_allplots,"save/simulations_iEMSs2020/tt_coupled_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tt_coupled <- rowMeans(tt_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tt_coupled,"save/simulations_iEMSs2020/row_mean_tt_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tt_coupled <- rowMins(tt_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_tt_coupled,"save/simulations_iEMSs2020/row_min_tt_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tt_coupled <- rowMaxs(tt_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tt_coupled,"save/simulations_iEMSs2020/row_max_tt_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tt_p_coupled_allplots <-sapply(tt_p_coupled, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tt_p_coupled_allplots,"save/simulations_iEMSs2020/tt_p_coupled_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tt_p_coupled <- rowMeans(tt_p_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tt_p_coupled,"save/simulations_iEMSs2020/row_mean_tt_p_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tt_p_coupled <- rowMins(tt_p_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_tt_p_coupled,"save/simulations_iEMSs2020/row_min_tt_p_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tt_p_coupled <- rowMaxs(tt_p_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tt_p_coupled,"save/simulations_iEMSs2020/row_max_tt_p_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

lai_p_coupled_allplots <-sapply(lai_p_coupled, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(lai_p_coupled_allplots,"save/simulations_iEMSs2020/lai_p_coupled_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_lai_p_coupled <- rowMeans(lai_p_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_lai_p_coupled,"save/simulations_iEMSs2020/row_mean_lai_p_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_lai_p_coupled <- rowMins(lai_p_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_lai_p_coupled,"save/simulations_iEMSs2020/row_min_lai_p_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_lai_p_coupled <- rowMaxs(lai_p_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_lai_p_coupled,"save/simulations_iEMSs2020/row_max_lai_p_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tdm_coupled_allplots <-sapply(tdm_coupled, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tdm_coupled_allplots,"save/simulations_iEMSs2020/tdm_coupled_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tdm_coupled <- rowMeans(tdm_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tdm_coupled,"save/simulations_iEMSs2020/row_mean_tdm_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tdm_coupled <- rowMins(tdm_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tdm_coupled,"save/simulations_iEMSs2020/row_mean_tdm_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tdm_coupled <- rowMaxs(tdm_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tdm_coupled,"save/simulations_iEMSs2020/row_max_tdm_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

tdm_p_coupled_allplots <-sapply(tdm_p_coupled, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
write.csv(tdm_p_coupled_allplots,"save/simulations_iEMSs2020/tdm_p_coupled_allplots.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_mean_tdm_p_coupled <- rowMeans(tdm_p_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_mean_tdm_p_coupled,"save/simulations_iEMSs2020/row_mean_tdm_p_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_min_tdm_p_coupled <- rowMins(tdm_p_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_min_tdm_p_coupled,"save/simulations_iEMSs2020/row_min_tdm_p_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
row_max_tdm_p_coupled <- rowMaxs(tdm_p_coupled_allplots) #Moyenne de l'ensemble des parcelles par jour
write.csv(row_max_tdm_p_coupled,"save/simulations_iEMSs2020/row_max_tdm_p_coupled.csv", row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")

X11(); plot(as.Date(input_meteo$Date),row_mean_lai_coupled, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Leaf Area Index");
lines(as.Date(input_meteo$Date),row_min_lai_coupled, col="black", lty = "dotted", lwd=1); lines(as.Date(input_meteo$Date),row_max_lai_coupled, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_lai_coupled,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(3,1), col=c("red", "blue","black"),
       legend = c("Max", "Mean", "Min"))
savePlot(filename = "save/simulations_iEMSs2020/lai_coupled.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_coupled, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");
lines(as.Date(input_meteo$Date),row_min_wsi_coupled, col="black", lty = "dotted", lwd=1); lines(as.Date(input_meteo$Date),row_max_wsi_coupled, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_wsi_coupled,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("bottomleft", lty=c(3,1), col=c("red", "blue","black"),
       legend = c("Max", "Mean", "Min"))
savePlot(filename = "save/simulations_iEMSs2020/wsi_coupled.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_etr_coupled, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Evapotranspiration");
lines(as.Date(input_meteo$Date),row_mean_etm_coupled, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_etr_coupled,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Max (ETM)", "Real (ETR"))
savePlot(filename = "save/simulations_iEMSs2020/et_coupled.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_tt_coupled, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Temperature sum (degree-day)");
lines(as.Date(input_meteo$Date),row_mean_tt_p_coupled, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_tt_coupled,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Potential", "Real"))
savePlot(filename = "save/simulations_iEMSs2020/tt_coupled.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_lai_coupled, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Leaf Area Index");
lines(as.Date(input_meteo$Date),row_mean_lai_p_coupled, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_lai_coupled,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Potential", "Real"))
savePlot(filename = "save/simulations_iEMSs2020/lai_comp_coupled.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_tdm_coupled, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Dry matter content");
lines(as.Date(input_meteo$Date),row_mean_tdm_p_coupled, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_tdm_coupled,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("Potential", "Real"))
savePlot(filename = "save/simulations_iEMSs2020/tdm_coupled.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_lai_coupled, type = "l", lwd=1, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Leaf Area Index");
lines(as.Date(input_meteo$Date),row_mean_lai_with_irrig, col="red", lty = "dotted", lwd=1)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_lai_coupled,type="l",lwd=1.5,col="blue"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("WatASit*Optirrig", "Optirrig"))
savePlot(filename = "save/simulations_iEMSs2020/lai_coupled_vs_not.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_coupled, type = "l", lwd=1.5, , lty = "dotted", axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");
lines(as.Date(input_meteo$Date),row_mean_wsi_with_irrig, col="blue", lwd=1.5)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_wsi_coupled,type="l",lwd=1.5,col="red", lty = "dotted"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("bottomleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("WatASit*Optirrig", "Optirrig"))
savePlot(filename = "save/simulations_iEMSs2020/wsi_coupled_vs_not.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_tdm_coupled, type = "l", lwd=1.5, , lty = "dotted", axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Dry matter content");
lines(as.Date(input_meteo$Date),row_mean_tdm_with_irrig, col="blue", lwd=1.5)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_tdm_coupled,type="l",lwd=1.5,col="red", lty = "dotted"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("bottomleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("WatASit*Optirrig", "Optirrig"))
savePlot(filename = "save/simulations_iEMSs2020/tdm_coupled_vs_not.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_etr_coupled, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "ETR");
lines(as.Date(input_meteo$Date),row_mean_etr_with_irrig, col="blue", lwd=1.5)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_etr_coupled,type="l",lwd=1.5,col="red"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("WatASit*Optirrig", "Optirrig"))
savePlot(filename = "save/simulations_iEMSs2020/etr_coupled_vs_not.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_tt_coupled, type = "l", lwd=1.5, axes=FALSE, xaxt= "n", xlab = "Month", ylab= "ETR");
lines(as.Date(input_meteo$Date),row_mean_tt_with_irrig, col="blue", lwd=1.5)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_tt_coupled,type="l",lwd=1.5,col="red"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("topleft", lty=c(2,1), col=c("red", "blue"),
       legend = c("WatASit*Optirrig", "Optirrig"))
savePlot(filename = "save/simulations_iEMSs2020/tt_coupled_vs_not.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_coupled, type = "l", lwd=1.5, , lty = "dotted", axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");
lines(as.Date(input_meteo$Date),row_mean_wsi_with_irrig, col="blue", lwd=1.5)
lines(as.Date(input_meteo$Date),row_mean_wsi_without_irrig, col="green", lwd=1.5)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
points(as.Date(input_meteo$Date),row_mean_wsi_coupled,type="l",lwd=1.5,col="red", lty = "dotted"); axis(2); axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("bottomleft", lty=c(3,1), col=c("red", "blue","green"),
       legend = c("Collective irrigation", "Individual irrigation", "Pluvial"))
savePlot(filename = "save/simulations_iEMSs2020/wsi_coll_vs_ind_vs_notirrig.png", device = dev.cur())

X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_coupled, type = "l", lwd=1.5, col="black", axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");
X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_with_irrig, type = "l", lwd=1.5, col="blue",axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");
X11(); plot(as.Date(input_meteo$Date),row_mean_wsi_without_irrig, type = "l", lwd=1.5,col="green", axes=FALSE, xaxt= "n", xlab = "Month", ylab= "Water Stress Index");












# mean_lai_without_irrig <- sapply( lai_without_irrig, FUN=function(items) {mean( unlist(items))})    
mean_lai_with_watasit <-sapply(lai_with_watasit, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
row_mean_lai_with_watasit <- rowMeans(mean_lai_with_watasit) #Moyenne de l'ensemble des parcelles par jour
mean_wsi_with_watasit <-sapply(wsi_with_watasit, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
row_mean_wsi_with_watasit <- rowMeans(mean_wsi_with_watasit) #Moyenne de l'ensemble des parcelles par jour
X11()
plot(row_mean_lai_with_watasit, type = "l", col="green")
lines(row_mean_lai_without_irrig, col="red")
X11()
plot(row_mean_lai_with_watasit, type = "l", col="green")
lines(row_mean_lai_with_irrig, col="blue")
X11()
plot(row_mean_wsi_with_watasit, type = "l", col="green")
lines(row_mean_wsi_without_irrig, col="red")
X11()
plot(row_mean_wsi_with_watasit, type = "l", col="green")
lines(row_mean_wsi_with_irrig, col="blue")

x11()
plot(as.Date(input_meteo$Date),row_mean_wsi_with_watasit,adj=0.02,type="l",lty=2,lwd=1.5,axes=FALSE,xaxt="n",ylim=c(0.78,1),col="red", xlab = "", ylab= "", main = "Water Stress Index (moyenne de 44 parcelles, 2016-2017)")
points(as.Date(input_meteo$Date),row_mean_wsi_with_irrig,type="l",lwd=1.5,col="blue")
axis(2)
axis.Date(1,at=seq(min(as.Date(input_meteo$Date)),max(as.Date(input_meteo$Date))+1,by="1 mon"),format="%m")
legend("bottomleft", lty=c(2,1), col=c("red", "blue"),
       #pt.bg  = c(NA, "red", NA, NA, NA),
       #pch    = c(NA, pmax, pobs, pmin, NA),
       legend = c("WatASit * Optirrig", "Optirrig"))
savePlot(filename = paste0("save/comp_plot_",scenario,".png"), device = dev.cur())


stop()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 7. Plot simulation results [OPTIONAL] #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4.1.3 Savings #######
res_with_irrig <- vect2_list
lai_with_irrig <- lai_list
wsi_with_irrig <- wsi_list
# mean_lai_without_irrig <- sapply( lai_without_irrig, FUN=function(items) {mean( unlist(items))})    
mean_lai_with_irrig <-sapply(lai_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
row_mean_lai_with_irrig <- rowMeans(mean_lai_with_irrig) #Moyenne de l'ensemble des parcelles par jour
mean_wsi_with_irrig <-sapply(wsi_with_irrig, FUN=function(top){ apply( as.data.frame(top), 1, mean)}) 
row_mean_wsi_with_irrig <- rowMeans(mean_wsi_with_irrig) #Moyenne de l'ensemble des parcelles par jour
X11()
plot(row_mean_lai_without_irrig, type = "l", col="red")
lines(row_mean_lai_with_irrig, col="blue")
X11()
plot(row_mean_wsi_without_irrig, type = "l", col="red")
lines(row_mean_wsi_with_irrig, col="blue")


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
# farmers_aff <- farmers_aff %>% select(-id.1,-id.2,-id.3,-id.4,-id.5,-id.6,-id.7,-id.8,-id.9,-id.10)
# farmers_act <- farmers_act %>% select(-id.1,-id.2,-id.3,-id.4,-id.5,-id.6,-id.7,-id.8,-id.9,-id.10)
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
# farmers_aff[which(farmers_aff$idExpl == 1),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 1),(2:11)] / parcels_nb_farm1
# farmers_aff[which(farmers_aff$idExpl == 2),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 2),(2:11)] / parcels_nb_farm2
# farmers_aff[which(farmers_aff$idExpl == 3),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 3),(2:11)] / parcels_nb_farm3
# farmers_aff[which(farmers_aff$idExpl == 4),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 4),(2:11)] / parcels_nb_farm4
# farmers_aff[which(farmers_aff$idExpl == 5),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 5),(2:11)] / parcels_nb_farm5
# farmers_aff[which(farmers_aff$idExpl == 6),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 6),(2:11)] / parcels_nb_farm6
# farmers_aff[which(farmers_aff$idExpl == 7),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 7),(2:11)] / parcels_nb_farm7
# farmers_aff[which(farmers_aff$idExpl == 8),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 8),(2:11)] / parcels_nb_farm8
# farmers_aff[which(farmers_aff$idExpl == 9),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 9),(2:11)] / parcels_nb_farm9
# farmers_aff[which(farmers_aff$idExpl == 10),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 10),(2:11)] / parcels_nb_farm10
# farmers_aff[which(farmers_aff$idExpl == 11),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 11),(2:11)] / parcels_nb_farm11
# farmers_aff[which(farmers_aff$idExpl == 12),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 12),(2:11)] / parcels_nb_farm12
# farmers_aff[which(farmers_aff$idExpl == 13),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 13),(2:11)] / parcels_nb_farm13
# farmers_aff[which(farmers_aff$idExpl == 14),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 14),(2:11)] / parcels_nb_farm14
# farmers_aff[which(farmers_aff$idExpl == 15),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 15),(2:11)] / parcels_nb_farm15
# farmers_aff[which(farmers_aff$idExpl == 16),(2:11)] <- farmers_aff[which(farmers_aff$idExpl == 16),(2:11)] / parcels_nb_farm16
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

farmplots_aff_df <- as.data.frame(farmplots_aff %>%
                                    group_by(idExpl, day) %>%
                                    summarize(sum_askAffCounter = sum(askAffCounter, na.rm = TRUE), sum_doSEAffCounter = sum(doSEAffCounter, na.rm = TRUE), sum_floodAffCounter = sum(floodAffCounter, na.rm = TRUE), sum_floodDRAffCounter = sum(floodDRAffCounter, na.rm = TRUE), sum_mowingAffCounter = sum(mowingAffCounter, na.rm = TRUE), sum_pickingAffCounter = sum(pickingAffCounter, na.rm = TRUE), sum_pressingAffCounter = sum(pressingAffCounter, na.rm = TRUE), sum_reapingAffCounter = sum(reapingAffCounter, na.rm = TRUE), sum_swathingAffCounter = sum(swathingAffCounter, na.rm = TRUE), sum_teddingAffCounter = sum(teddingAffCounter, na.rm = TRUE)))
farmplots_aff_df <- farmplots_aff_df %>% select(-sum_doSEAffCounter, -sum_mowingAffCounter, -sum_pickingAffCounter, -sum_pressingAffCounter, -sum_reapingAffCounter, -sum_swathingAffCounter, -sum_teddingAffCounter)
farmplots_aff_df[which(farmplots_aff_df$idExpl == 1),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 1),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm1) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 2),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 2),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm3) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 4),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 4),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm4) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 5),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 5),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm5) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 6),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 6),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm6) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 7),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 7),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm7) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 8),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 8),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm8) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 9),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 9),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm9) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 10),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 10),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm10) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 11),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 11),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm11) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 12),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 12),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm12) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 13),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 13),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm13) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 14),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 14),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm14) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 15),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 15),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm15) * 100
farmplots_aff_df[which(farmplots_aff_df$idExpl == 16),(3:dim(farmplots_aff_df)[2])] <- (farmplots_aff_df[which(farmplots_aff_df$idExpl == 16),(3:dim(farmplots_aff_df)[2])] / parcels_nb_farm16) * 100
farmplots_aff_df <- subset.data.frame(farmplots_aff_df, idExpl == 2 | idExpl == 3 | idExpl == 5 | idExpl == 6 | idExpl == 7 | idExpl == 10 |idExpl == 11  | idExpl == 12 | idExpl == 14 | idExpl == 16)

farmplots_act_df <- as.data.frame(farmplots_act %>%
                                    group_by(idExpl, day) %>%
                                    summarize(sum_askActCounter = sum(askActCounter, na.rm = TRUE), sum_doSEActCounter = sum(doSEActCounter, na.rm = TRUE), sum_floodActCounter = sum(floodActCounter, na.rm = TRUE), sum_floodDRActCounter = sum(floodDRActCounter, na.rm = TRUE), sum_mowingActCounter = sum(mowingActCounter, na.rm = TRUE), sum_pickingActCounter = sum(pickingActCounter, na.rm = TRUE), sum_pressingActCounter = sum(pressingActCounter, na.rm = TRUE), sum_reapingActCounter = sum(reapingActCounter, na.rm = TRUE), sum_swathingActCounter = sum(swathingActCounter, na.rm = TRUE), sum_teddingActCounter = sum(teddingActCounter, na.rm = TRUE)))
farmplots_act_df <- farmplots_act_df %>% select(-sum_doSEActCounter, -sum_mowingActCounter, -sum_pickingActCounter, -sum_pressingActCounter, -sum_reapingActCounter, -sum_swathingActCounter, -sum_teddingActCounter)
farmplots_act_df[which(farmplots_act_df$idExpl == 1),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 1),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm1) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 2),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 2),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm3) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 4),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 4),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm4) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 5),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 5),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm5) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 6),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 6),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm6) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 7),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 7),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm7) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 8),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 8),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm8) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 9),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 9),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm9) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 10),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 10),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm10) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 11),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 11),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm11) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 12),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 12),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm12) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 13),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 13),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm13) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 14),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 14),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm14) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 15),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 15),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm15) * 100
farmplots_act_df[which(farmplots_act_df$idExpl == 16),(3:dim(farmplots_act_df)[2])] <- (farmplots_act_df[which(farmplots_act_df$idExpl == 16),(3:dim(farmplots_act_df)[2])] / parcels_nb_farm16) * 100
farmplots_act_df <- subset.data.frame(farmplots_act_df, idExpl == 2 | idExpl == 3 | idExpl == 5 | idExpl == 6 | idExpl == 7 | idExpl == 10 |idExpl == 11  | idExpl == 12 | idExpl == 14 | idExpl == 16)

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
#Au 1er juillet (DOY 61 en partant du 1er mai)
# crops_results_61 <- subset.data.frame(crops_results, day == 61)
# crops_results_61[which(crops_results_61$idExpl==2),24] <- 1
# crops_results_61[which(crops_results_61$idExpl==3),24] <- 2
# crops_results_61[which(crops_results_61$idExpl==5),24] <- 3
# crops_results_61[which(crops_results_61$idExpl==6),24] <- 4
# crops_results_61[which(crops_results_61$idExpl==7),24] <- 5
# crops_results_61[which(crops_results_61$idExpl==10),24] <- 6
# crops_results_61[which(crops_results_61$idExpl==11),24] <- 7
# crops_results_61[which(crops_results_61$idExpl==12),24] <- 8
# crops_results_61[which(crops_results_61$idExpl==14),24] <- 9
# crops_results_61[which(crops_results_61$idExpl==16),24] <- 10
# failed_irrigations_61 <- crops_results_61 %>% select(id, abandonedState, idExpl)

#Au 1er septembre (DOY 123)
# crops_results_123 <- subset.data.frame(crops_results, day == 123)
# crops_results_123[which(crops_results_123$idExpl==2),24] <- 1
# crops_results_123[which(crops_results_123$idExpl==3),24] <- 2
# crops_results_123[which(crops_results_123$idExpl==5),24] <- 3
# crops_results_123[which(crops_results_123$idExpl==6),24] <- 4
# crops_results_123[which(crops_results_123$idExpl==7),24] <- 5
# crops_results_123[which(crops_results_123$idExpl==10),24] <- 6
# crops_results_123[which(crops_results_123$idExpl==11),24] <- 7
# crops_results_123[which(crops_results_123$idExpl==12),24] <- 8
# crops_results_123[which(crops_results_123$idExpl==14),24] <- 9
# crops_results_123[which(crops_results_123$idExpl==16),24] <- 10
# failed_irrigations_123 <- crops_results_123 %>% select(id, abandonedState, idExpl)

#Au 15 octobre
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
# failed_irrigations_ten_end <- crops_results_end %>% select(id, failed_irrigations_ten, idExpl)
# failed_irrigations_fifteen_end <- crops_results_end %>% select(id, failed_irrigations_fifteen, idExpl)
# failed_irrigations_twenty_end <- crops_results_end %>% select(id, failed_irrigations_twenty, idExpl)
# failed_irrigations_thirty_end <- crops_results_end %>% select(id, failed_irrigations_thirty, idExpl)
# failed_irrigations_fourty_end <- crops_results_end %>% select(id, failed_irrigations_fourty, idExpl)
# failed_irrigations_fifty_end <- crops_results_end %>% select(id, failed_irrigations_fifty, idExpl)
# failed_irrigations_sixty_end <- crops_results_end %>% select(id, failed_irrigations_sixty, idExpl)
failed_irrigations_classA_end <- crops_results_end %>% select(id, failed_irrigations_classA, idExpl)
failed_irrigations_classB_end <- crops_results_end %>% select(id, failed_irrigations_classB, idExpl)
failed_irrigations_classC_end <- crops_results_end %>% select(id, failed_irrigations_classC, idExpl)
failed_irrigations_classD_end <- crops_results_end %>% select(id, failed_irrigations_classD, idExpl)

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
aff <- farmplots_aff_df %>%
  gather(Affordance,value, sum_askAffCounter,sum_floodAffCounter, sum_floodDRAffCounter#, sum_mowingAffCounter, sum_pickingAffCounter, sum_pressingAffCounter, sum_reapingAffCounter, sum_swathingAffCounter, sum_teddingAffCounter
  ) %>%
  ggplot(aes(x=day, y=value, colour=Affordance)) +
  facet_wrap(~ factor(idExpl, levels=c('2','3','5','6', '7', '10', '11', '12', '14', '16')), scales = "fixed", ncol = 4, labeller = as_labeller(id_names)) +
  geom_line() + xlim(c(0, max(farmplots_aff_df$day))) + #ylim(c(0, max(farmplots_aff_df$sum_askAffCounter))) +
  theme_bw() +
  scale_color_manual(name="Affordance", 
                     labels = c("Ask more water at plot", 
                                "Flood plot", 
                                "Flood plot during restriction"#, 
                                # "Mowing",
                                # "Picking & storing",
                                # "Pressing",
                                # "Reaping & storing",
                                # "Swathing",
                                # "Tedding"
                     ), 
                     values = c("sum_askAffCounter"="#4daf4a", 
                                "sum_floodAffCounter"="#377eb8", 
                                "sum_floodDRAffCounter"="#e41a1c"#, 
                                # "sum_mowingAffCounter"="#00441b",
                                # "sum_pickingAffCounter"="#006d2c",
                                # "sum_pressingAffCounter"="#238b45",
                                # "sum_reapingAffCounter"="#41ab5d",
                                # "sum_swathingAffCounter"="#74c476",
                                # "sum_teddingAffCounter"="#a1d99b"
                     )) +
  theme(legend.box = "vertical",  legend.position = c(0.5,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste(" ",sep = ""), y = paste("Farm plots with affordance (%)", sep = "")) + ggtitle(paste0(scenario, " scenario"))
# legend <- get_legend(aff)

####### 7.4.2 Plot the farmers'actions [OPTIONAL] #######
act <- farmplots_act_df %>%
  gather(Action,value,sum_askActCounter,#doSEActCounter,
         sum_floodActCounter, sum_floodDRActCounter#, mowingActCounter, pickingActCounter, pressingActCounter, reapingActCounter, swathingActCounter, teddingActCounter
  ) %>%
  ggplot(aes(x=day, y=value, colour=Action)) +
  facet_wrap(~ factor(idExpl, levels=c('2','3','5','6', '7', '10', '11', '12', '14', '16')), scales = "fixed", ncol = 4, labeller = as_labeller(id_names)) +
  geom_line() + xlim(c(0, max(farmplots_act_df$day))) + ylim(c(0, max(farmplots_act_df$sum_askActCounter))) +
  scale_color_manual(name="Action", 
                     labels = c("Ask more water at plot", 
                                #"Do something else", 
                                "Flood plot", 
                                "Flood plot during restriction"#, 
                                # "Mowing",
                                # "Picking & storing",
                                # "Pressing",
                                # "Reaping & storing",
                                # "Swathing",
                                # "Tedding"
                     ), 
                     values = c("sum_askActCounter"="#4daf4a", 
                                #"doSEActCounter"="#000000", 
                                "sum_floodActCounter"="#377eb8", 
                                "sum_floodDRActCounter"="#e41a1c"#, 
                                # "mowingActCounter"="#00441b",
                                # "pickingActCounter"="#006d2c",
                                # "pressingActCounter"="#238b45",
                                # "reapingActCounter"="#41ab5d",
                                # "swathingActCounter"="#74c476",
                                # "teddingActCounter"="#a1d99b"
                     )) +
  theme_bw() +
  theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
        panel.grid = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = paste("DOY",sep = ""), y = paste("Farm plots with action (%)", sep = ""))

x11()
aff <- aff + theme(legend.position="none")
act <- act + theme(legend.position="none")
grid.arrange(aff,act,ncol=1, nrow = 2) # Ajouter la légende à la main
savePlot(filename = paste0("save/affordances_plot_",scenario,".png"), device = dev.cur())

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
  theme(legend.box = "vertical",  legend.position = c(0.5,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
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
# grid.arrange(f,m,wc,sc, ncol=1, nrow=4, widths=c(2.5)) #Rest à ajouter une légende à la main
grid.arrange(f,m,wc, ncol=1, nrow=3, widths=c(2.5)) #Rest à ajouter une légende à la main
savePlot(filename = paste0("save/wsi_plot_",scenario,".png"), device = dev.cur())

####### 7.4.4 Plot the indicator results [OPTIONAL] #######
# q <- ggplot(ind_results, aes(x=day, y=q_cumec)) +
#   geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$q_cumec))) +
#   theme_bw()  +
#   theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
#         panel.grid = element_line(),
#         panel.background = element_blank(),
#         legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
#         plot.title = element_text(hjust = 0.5)) +
#   labs(x = paste(" ",sep = ""), y = paste("Q [cumec]", sep = ""))
# 
# inquiries <- ggplot(ind_results, aes(x=day, y=inquiries)) +
#   geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$inquiries))) +
#   theme_bw()  +
#   theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
#         panel.grid = element_line(),
#         panel.background = element_blank(),
#         legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
#         plot.title = element_text(hjust = 0.5)) +
#   labs(x = paste(" ",sep = ""), y = paste("Farmer inquiries", sep = ""))
# 
# unrespect <- ggplot(ind_results, aes(x=day, y=unrespect)) +
#   geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$unrespect))) +
#   theme_bw()  +
#   theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
#         panel.grid = element_line(),
#         panel.background = element_blank(),
#         legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
#         plot.title = element_text(hjust = 0.5)) +
#   labs(x = paste(" ",sep = ""), y = paste("Irrigation under restriction", sep = ""))
# 
# abandoned <- ggplot(ind_results, aes(x=day, y=abandoned)) +
#   geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$abandoned))) +
#   theme_bw()  +
#   theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
#         panel.grid = element_line(),
#         panel.background = element_blank(),
#         legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
#         plot.title = element_text(hjust = 0.5)) +
#   labs(x = paste(" ",sep = ""), y = paste("Abandoned plot", sep = ""))
# 
# release <- ggplot(ind_results, aes(x=day, y=release_cumec)) +
#   geom_line() + xlim(c(0, max(ind_results$day))) + ylim(c(0, max(ind_results$release_cumec))) +
#   theme_bw()  +
#   theme(legend.box = "vertical",  legend.position = c(1.1,0.5), plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
#         panel.grid = element_line(),
#         panel.background = element_blank(),
#         legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"),
#         plot.title = element_text(hjust = 0.5)) +
#   labs(x = paste("DOY",sep = ""), y = paste("Release [cumec]", sep = ""))
# 
# q <- q + theme(legend.position="none")
# inquiries <- inquiries + theme(legend.position="none")
# unrespect <- unrespect + theme(legend.position="none")
# abandoned <- abandoned + theme(legend.position="none")
# release <- release + theme(legend.position="none")
# x11()
# grid.arrange(q,inquiries,unrespect,abandoned,release, ncol=1, nrow=5, widths=c(2.5)) #Reste à ajouter une légende à la main
# savePlot(filename = paste0("save/indicators_plot.png"), device = dev.cur())

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

# #At DOY 61 (Jully 1st)
# for (i in 1:length(rpg_df$ID_PARCEL)){
#   id_plot <- rpg_df[i,1] 
#   id <- which(failed_irrigations_61[,1] == id_plot)
#   state <- failed_irrigations_61[id,2]
#   state <- max(state); #if(state>0){state ==1}
#   if (is.numeric0(state)==F)
#   rpg_df[i,24] <- state
# }
# rpg_df[which(rpg_df$abandoned<=0),24] <- 0
# #rpg_df[which(rpg_df$abandoned==2),24] <- 1
# rpg = SpatialPolygonsDataFrame(as(rpg, "SpatialPolygons"), data=rpg_df, match.ID=F)
# # x11(); plot(rpg)
# # text(coordinates(rpg), labels=rpg$abandoned,font=2,cex=0.5)
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 0] <- "#ffffff"
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 1] <- "#abdda4"
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 2] <- "#ffffbf"
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 3] <- "#fdae61"
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 4] <- "#d7191c"
# x11(); plot(rpg, col=rpg@data$COLOUR, axes = T)
# title(main = "Failed irrigations on Jully, 1st 2017", sub = "(Fingers are farmer ID, parcels with no ID are not part \n of farms using the collective irrigation network)")
# text(coordinates(rpg), labels=rpg$ID_EXPL,font=2,cex=0.5)
# legend("topright",   # location of legend
#        legend = levels(factor(rpg@data$abandoned, levels=c('Failed irrigation','Not failed'))), # categories or elements to render in
#        # the legend
#        fill = c("#ffffff","#ef8a62")) # color palette to use to fill objects in legend.
# savePlot(filename = paste0("save/failed_irrigations_61.png"), device = dev.cur())
# 
# #At DOY 123 (September 1st)
# for (i in 1:length(rpg_df$ID_PARCEL)){
#   id_plot <- rpg_df[i,1] 
#   id <- which(failed_irrigations_123[,1] == id_plot)
#   state <- failed_irrigations_123[id,2]
#   state <- sum(state); if(state>0){state ==1}
#   if (is.numeric0(state)==F)
#     rpg_df[i,24] <- state
# }
# rpg_df[which(rpg_df$abandoned==2),24] <- 1
# rpg = SpatialPolygonsDataFrame(as(rpg, "SpatialPolygons"), data=rpg_df, match.ID=F)
# # x11(); plot(rpg)
# # text(coordinates(rpg), labels=rpg$abandoned,font=2,cex=0.5)
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 0] <- "#ffffff"
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 1] <- "#ef8a62"
# x11(); plot(rpg, col=rpg@data$COLOUR, axes = T)
# title(main = "Failed irrigations on September, 1st 2017", sub = "(Fingers are farmer ID, parcels with no ID are not part \n of farms using the collective irrigation network)")
# text(coordinates(rpg), labels=rpg$ID_EXPL,font=2,cex=0.5)
# legend("topright",   # location of legend
#        legend = levels(factor(rpg@data$abandoned, levels=c('Failed irrigation','Not failed'))), # categories or elements to render in
#        # the legend
#        fill = c("#ffffff","#ef8a62")) # color palette to use to fill objects in legend.
# savePlot(filename = paste0("save/failed_irrigations_123.png"), device = dev.cur())

#At the end of the irrigation campaign (October 15)
#classA
# for (i in 1:length(rpg_df$ID_PARCEL)){
#   id_plot <- rpg_df[i,1] 
#   id <- which(failed_irrigations_classA_end[,1] == id_plot)
#   state <- failed_irrigations_classA_end[id,2]
#   state <- max(state);
#   if (is.numeric0(state)==F)
#     rpg_df[i,24] <- state
# }
# rpg_df[which(rpg_df$abandoned <= 0),24] <- 0
# rpg = SpatialPolygonsDataFrame(as(rpg, "SpatialPolygons"), data=rpg_df, match.ID=F)
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 0] <- "#ffffff"
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 1] <- "#ef8a62"
# x11(); plot(rpg, col=rpg@data$COLOUR, axes = T)
# title(main = "0 to 10-days failed irrigations", sub = "(Fingers are farmer ID, parcels with no ID are not part \n of farms using the collective irrigation network)")
# text(coordinates(rpg), labels=rpg$ID_EXPL,font=2,cex=0.5)
# legend("topright",   
#        legend = levels(factor(rpg@data$abandoned, levels=c('Not failed','Failed'))), 
#        fill = c("#ffffff","#ef8a62")) 
# savePlot(filename = paste0("save/failed_irrigations_classeA_end_",scenario,".png"), device = dev.cur())

#classB
# for (i in 1:length(rpg_df$ID_PARCEL)){
#   id_plot <- rpg_df[i,1] 
#   id <- which(failed_irrigations_classB_end[,1] == id_plot)
#   state <- failed_irrigations_classB_end[id,2]
#   state <- max(state);
#   if (is.numeric0(state)==F)
#     rpg_df[i,24] <- state
# }
# rpg_df[which(rpg_df$abandoned <= 0),24] <- 0
# rpg = SpatialPolygonsDataFrame(as(rpg, "SpatialPolygons"), data=rpg_df, match.ID=F)
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 0] <- "#ffffff"
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 1] <- "#ef8a62"
# x11(); plot(rpg, col=rpg@data$COLOUR, axes = T)
# title(main = "10 to 20-days failed irrigations", sub = "(Fingers are farmer ID, parcels with no ID are not part \n of farms using the collective irrigation network)")
# text(coordinates(rpg), labels=rpg$ID_EXPL,font=2,cex=0.5)
# legend("topright",
#        legend = levels(factor(rpg@data$abandoned, levels=c('Not failed','Failed'))), 
#        fill = c("#ffffff","#ef8a62"))
# savePlot(filename = paste0("save/failed_irrigations_classB_end_",scenario,".png"), device = dev.cur())

#classC
# for (i in 1:length(rpg_df$ID_PARCEL)){
#   id_plot <- rpg_df[i,1] 
#   id <- which(failed_irrigations_classC_end[,1] == id_plot)
#   state <- failed_irrigations_classC_end[id,2]
#   state <- max(state);
#   if (is.numeric0(state)==F)
#     rpg_df[i,24] <- state
# }
# rpg_df[which(rpg_df$abandoned <= 0),24] <- 0
# rpg = SpatialPolygonsDataFrame(as(rpg, "SpatialPolygons"), data=rpg_df, match.ID=F)
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 0] <- "#ffffff"
# rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 1] <- "#ef8a62"
# x11(); plot(rpg, col=rpg@data$COLOUR, axes = T)
# title(main = "20 to 30-days failed irrigations", sub = "(Fingers are farmer ID, parcels with no ID are not part \n of farms using the collective irrigation network)")
# text(coordinates(rpg), labels=rpg$ID_EXPL,font=2,cex=0.5)
# legend("topright",  
#        legend = levels(factor(rpg@data$abandoned, levels=c('Not failed','Failed'))), 
#        fill = c("#ffffff","#ef8a62"))
# savePlot(filename = paste0("save/failed_irrigations_classC_end_",scenario,".png"), device = dev.cur())

#classD
for (i in 1:length(rpg_df$ID_PARCEL)){
  id_plot <- rpg_df[i,1] 
  id <- which(failed_irrigations_classD_end[,1] == id_plot)
  state <- failed_irrigations_classD_end[id,2]
  state <- max(state);
  if (is.numeric0(state)==F)
    rpg_df[i,24] <- state
}
rpg_df[which(rpg_df$abandoned <= 0),24] <- 0
rpg = SpatialPolygonsDataFrame(as(rpg, "SpatialPolygons"), data=rpg_df, match.ID=F)
rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 0] <- "#ffffff"
rpg@data$COLOUR[(as.numeric(as.character(rpg@data$abandoned))) == 1] <- "#ef8a62"
x11(); plot(rpg, col=rpg@data$COLOUR, axes = T)
title(main = paste0(scenario," scenario"), sub = "(Fingers are farmer ID, parcels with no ID are not part \n of farms using the collective irrigation network)")
text(coordinates(rpg), labels=rpg$ID_EXPL,font=2,cex=0.5)
legend("topright",
       legend = levels(factor(rpg@data$abandoned, levels=c('Not abandoned','Abandoned*'))),
       fill = c("#ffffff","#ef8a62"))
savePlot(filename = paste0("save/abandoned_end_",scenario,".png"), device = dev.cur())