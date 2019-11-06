###############################################################
#################      R coupler      #########################
###############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Settings 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### Set directory for R-coupler #######
rm(list=ls()); 
#wd <- "/home/bastien/Documents/2017-2020_These_GEAU/Work_Optirrig/Optirrig/WatASit/WatASit_Rcoupler/"
wd <- "~/cormas2017_package/Models/COWAT/WatASit_Rcoupler/"
setwd(wd)

####### Load functions #######
wd_Functions <- paste0(wd,"Rfunctions/")
for(FileName in list.files(wd_Functions, pattern="\\.[Rr]$")){ source(file.path(wd_Functions,FileName)); }

####### Load libraries #######
load <- c(require (multiplex), require(tidyr),require(ggplot2),require(dplyr),require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

####### Core parallelism #######
cores <- parallel:::detectCores(); registerDoParallel(cores-2);

####### Case study #######
case_study_name <- "Aspres"

####### Year of simulation #######
yearSim <- 1992

####### Set simulation duration #######
simDayNb <- 4

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load meteo variables 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

meteo      = read.csv(paste0(wd, 'climatefile/climate_test.csv'), header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
sim_period = which(meteo$year == yearSim) 
meteo      = meteo[sim_period,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate Optirrig param files for WatASit plots  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

list_idParcel <- optiParams(paste0(wd,'paramfiles/'), case_study_name, 'watasit.csv', 'paramDB.csv','climate_buech_2017.csv', yearSim, 1, 365, 'irrig_file_watasit.dat')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Init WatASit model 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### Open model #######
# r <- openModel("COWAT", parcelFile="WatASit[v8].pcl")
r <- openModel("COWAT", parcelFile="WatASit[EMSpaper].pcl")

####### Activate probes about crops (Facultatif: to get data from cormas) #######
r <- activateProbe("abandonedCropEvent","COWAT")
r <- activateProbe("ASAinquiries","COWAT")
r <- activateProbe("exceedMaxWithdrawalEvent","COWAT")
r <- activateProbe("qIntake","COWAT")
r <- activateProbe("unrespectRestrictionEvent","COWAT")
r <- activateProbe("f1IrrigatedPlotNb","COWAT")
r <- activateProbe("f2irrigatedPlotNb","COWAT")
# r <- activateProbe("f3IrrigatedPlotNb","COWAT")
# r <- activateProbe("f4IrrigatedPlotNb","COWAT")
# r <- activateProbe("f5IrrigatedPlotNb","COWAT")
# r <- activateProbe("f6IrrigatedPlotNb","COWAT")
# r <- activateProbe("f7IrrigatedPlotNb","COWAT")
# r <- activateProbe("f8IrrigatedPlotNb","COWAT")
# r <- activateProbe("f9IrrigatedPlotNb","COWAT")
# r <- activateProbe("f10IrrigatedPlotNb","COWAT")
# r <- activateProbe("f11IrrigatedPlotNb","COWAT")
# r <- activateProbe("f12IrrigatedPlotNb","COWAT")
# r <- activateProbe("f13IrrigatedPlotNb","COWAT")
# r <- activateProbe("f14IrrigatedPlotNb","COWAT")
# r <- activateProbe("f15IrrigatedPlotNb","COWAT")
# r <- activateProbe("f16IrrigatedPlotNb","COWAT")

# r <- activateProbe("idParcelProbe","Ecrop")
# r <- activateProbe("harvestSignalProbe","Ecrop")
# r <- activateProbe("wsiProbe","Ecrop")
# r <- activateProbe("laiProbe","Ecrop")
# r <- activateProbe("hiProbe","Ecrop")
# r <- activateProbe("irriDailyDoseProbe","Ecrop")
# r <- activateProbe("cropMaturitySignal","Ecrop")

####### Choose initial state and time step function (scenarios) #######
r <- setInit("INIT_2017_54x44")
r <- setStep("R_goBaselineStep:")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Init Optirrig model 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

paramFrame <- data.frame()
irr <- data.frame()
for (i in 1:length(list_idParcel)){
  
  ####### Load params of each plot #######
  param = read.csv(paste0(wd,'paramfiles/paramfiles_',case_study_name,'/',list_idParcel[i],'/parF', list_idParcel[i],'.csv'), header = TRUE,sep=",",dec = ".",stringsAsFactor=FALSE)
  
  ####### Create frame with all parameters #######
  paramFrame <- rbind(paramFrame, param)
  
  ####### Create frame with all irrigation vectors  #######
  I1   = as.vector(meteo$day) ; I1[] = 0; I2 = I1 #I1 = irrigation de surface et I2 = irrigation enterrée qui reste nulle
  # Irr = data.frame(doy=meteo$doy,I=0)
  #I1 = data.frame(I1, row.names = meteo$doy)
  irr = rbind(irr,I1)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialise simulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

r <- initSimu()

####### Create results dataFrame #######
cropResults <- data.frame(idParcel = NULL, wsi = NULL, lai = NULL, hi = NULL, cropMaturitySignal = NULL)
farmersResults<- data.frame(id = NULL, day = NULL, nbFloodPlotAffToday = NULL, dosCounter = NULL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run Optirrig simulation from 1 DOY to DOY 120 (1er mai)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#for (day in 1:120){
for (day in 1:10){ #For testing
  
  ####### Simulate the new state of crops with Optirrig #######
  ####### Init optirrig on day 1 #######
  if (day == 1) {
    cstesList <- list()
    invalList <- list()
    vectList <- list()
    for (i in 1:length(list_idParcel)){
      init <- init_optirr(paramFrame[i,], meteo)
      
      ####### Constantes #######
      cstes = init$cstes
      cstesList <- rbind(cstesList, cstes)
      
      ####### Valeurs de calcul dont on n'a pas besoin de l'historique (liste de valeurs) #######
      inval = init$inval
      invalList <-  rbind(invalList, inval)
      
      ####### Vecteur de variables d'états stokés sous forme de séries temporelles dans des vecteurs (liste de vecteurs) #######
      vect  = init$vect
      vectList <-  rbind(vectList, vect)
    }
  }
  
  ####### Simulate on other days #######
  if (day != 1) {
    inval2List <- list()
    vect2List <- list()
    
    for (i in 1:length(list_idParcel)){
      cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
      
      ####### Irrigation is nul #######
      I1 = I2
      
      ####### SImulate with Optirrig #######
      param<-paramFrame[i,]
      cstes<-cstesList[i,]
      inval<-invalList[i,]
      vect<-vectList[i,]
      optirday = daily_optirr(param,
                              meteo,
                              cstes,
                              inval,
                              vect,
                              I1, # Irrigation de surface 
                              I2, # Irrigation de profondeur (goutte à goutte enterré)
                              day) # Pas de temps.
      
      
      
      ####### cstes2 = optirday$cstes # A priori ne change pas donc pas besoin de recalculer (à vérifier) #######
      inval2 = optirday$inval
      inval2List <- rbind(inval2List, inval2)
      vect2  = optirday$vect
      vect2List <- rbind(vect2List, vect2)
      
      invalList[i,] <- inval2
      vectList[i,] <- vect2
    }
  } 
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run coupled simulation from DOY 121 (1er mai)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#for (day in 121:(121+simDayNb)){
for (day in 11:(10+simDayNb)){
  ####### Update Cormas Meteo #######
  P<-meteo$P
  setAttributesOfEntities("p", "Meteo", 1, P[day]) #meteo du jour
  #setAttributesOfEntities("p", "Meteo", 1, 12)
  p_forecast = sum(c(P[day],P[day+1],P[day+2]), na.rm = TRUE); if (p_forecast > 0) {p_forecast = 1}
  setAttributesOfEntities("p_forecast", "Meteo", 1, p_forecast) # Precipitation forecast for the next 3 days
  # setAttributesOfEntities("p_forecast", "Meteo", 1, 1)
  # if (day == 1) {p_cumTenDays = 0}
  # if (day == 2) {p_cumTenDays = P[day-1]}
  # if (day == 3) {p_cumTenDays = sum(c(P[day-1],P[day-2]), na.rm = TRUE)}
  # if (day == 4) {p_cumTenDays = sum(c(P[day-1],P[day-2],P[day-3]), na.rm = TRUE)}
  # if (day == 5) {p_cumTenDays = sum(c(P[day-1],P[day-2],P[day-3], P[day-4]), na.rm = TRUE)}
  # if (day == 6) {p_cumTenDays = sum(c(P[day-1],P[day-2],P[day-3], P[day-4], P[day-5]), na.rm = TRUE)}
  # if (day == 7) {p_cumTenDays = sum(c(P[day-1],P[day-2],P[day-3], P[day-4], P[day-5], P[day-6]), na.rm = TRUE)}
  # if (day == 8) {p_cumTenDays = sum(c(P[day-1],P[day-2],P[day-3], P[day-4], P[day-5], P[day-6], P[day-7]), na.rm = TRUE)}
  # if (day == 9) {p_cumTenDays = sum(c(P[day-1],P[day-2],P[day-3], P[day-4], P[day-5], P[day-6], P[day-7], P[day-8]), na.rm = TRUE)}
  # if (day == 10) {p_cumTenDays = sum(c(P[day-1],P[day-2],P[day-3], P[day-4], P[day-5], P[day-6], P[day-7], P[day-8], P[day-9]), na.rm = TRUE)}
  if (day >= 11) {p_cumTenDays = sum(c(P[day-1],P[day-2],P[day-3], P[day-4], P[day-5], P[day-6], P[day-7], P[day-8], P[day-9], P[day-10]), na.rm = TRUE)}
  setAttributesOfEntities("p_cumTenDays", "Meteo", 1, p_cumTenDays) # Cumul des précipitations des 10 derniers jours
  # setAttributesOfEntities("p_cumTenDays", "Meteo", 1, 10)
  # if (day <= 11) {p_cumFifteenDays = p_cumTenDays}
  # if (day == 12) {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-11]), na.rm = TRUE)}
  # if (day == 13) {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-11], P[day-12]), na.rm = TRUE)}
  # if (day == 14) {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-11], P[day-12], P[day-13]), na.rm = TRUE)}
  # if (day == 15) {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-11], P[day-12], P[day-13], P[day-14]), na.rm = TRUE)}
  if (day >= 16) {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-11], P[day-12], P[day-13], P[day-14], P[day-15]), na.rm = TRUE)} else
  {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-1], P[day-2], P[day-3], P[day-4], P[day-5]), na.rm = TRUE)}
  setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, p_cumFifteenDays) # Cumul des précipitations des 15 derniers jours
  # setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, 50)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Run coupled simulation of 24 hours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  r <- runSimu(duration = 24)
  response <- gettext(r[[2]])
  if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {stop("RUN STOPPED",call.=FALSE)} #Pour vérifier que le runSimu Cormas est bien fini
  obs1 <- getAttributesOfEntities("nbFloodPlotAffToday","Efarmer")
  obs2 <- getAttributesOfEntities("dosCounter","Efarmer")
  obs <- left_join(obs1,obs2, by = "id")
  obs$day = day
  farmersResults <- farmersResults %>% 
    bind_rows(obs)
  ####### Get the state of crops from Cormas #######
  idParcel      <- getAttributesOfEntities("idParcel", "Ecrop");  list_idParcel <- idParcel$idParcel
  harvestSignal <- getAttributesOfEntities("harvestSignal", "Ecrop")
  irriDailyDose <- getAttributesOfEntities("irriDailyDose", "Ecrop")
  
  ####### Simulate the new state of crops with Optirrig #######
  if (day != 1) {
    inval2List <- list()
    vect2List <- list()
    
    for (i in 1:length(list_idParcel)){
      cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
      
      ####### Update irrigation from Cormas #######
      irr[i,day]  <- irriDailyDose$irriDailyDose[i]
      # irr[i,day] <- 18
      irr<-as.matrix(irr)
      I1 =irr[i,]
      
      ####### SImulate with Optirrig #######
      param<-paramFrame[i,]
      cstes<-cstesList[i,]
      inval<-invalList[i,]
      vect<-vectList[i,]
      optirday = daily_optirr(param,
                              meteo,
                              cstes,
                              inval,
                              vect,
                              I1, # Irrigation de surface 
                              I2, # Irrigation de profondeur (goutte à goutte enterré)
                              day) # Pas de temps.
      
      
      
      ####### cstes2 = optirday$cstes # A priori ne change pas donc pas besoin de recalculer (à vérifier) #######
      inval2 = optirday$inval
      inval2List <- rbind(inval2List, inval2)
      vect2  = optirday$vect
      vect2List <- rbind(vect2List, vect2)
      
      invalList[i,] <- inval2
      vectList[i,] <- vect2
    }
  } 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get new crop state from Optirrig simulations 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ####### Get new crop states #######
  # newLAI<-vectList
  # newCropState <- data.frame(list_idParcel, vect2Frame$wsi, vect2Frame$lai, vect2Frame$hi, vect2Frame$cropMaturitySignal) # Tableau final avec les  sorties Optirrig pour chaque idParcel
  
  
  ####### Set the new state of crops in cormas #######
  #setAttributesOfEntities("wsi", "Ecrop", idParcel$id, newCropState$wsi)    # Vérifier qu'on prend bien les bonnes parcelles!!!!
  # setAttributesOfEntities("lai", "Ecrop", idParcel$id, newCropState$lai)
  #setAttributesOfEntities("hi", "Ecrop", idParcel$id, newCropState$hi)
  # setAttributesOfEntities("cropMaturiySignal", "Ecrop", idParcel$id, newCropState$cropMaturitySignal)
  
  ####### Save results in R (even though they are also in cormas) #######
  # newCropState$irrigation <- irriDailyDose$values
  # newCropState$day <- day
  # cropResults <- rbind(cropResults, newCropState )
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Observe the evolution of coupled dynamics 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
abandonedCropEvent <- getNumericProbe("abandonedCropEvent","COWAT")
ASAinquiries <- getNumericProbe("ASAinquiries","COWAT")
exceedMaxWithdrawalEvent <- getNumericProbe("exceedMaxWithdrawalEvent","COWAT")
qIntake <- getNumericProbe("qIntake","COWAT")
unrespectRestrictionEvent <- getNumericProbe("unrespectRestrictionEvent","COWAT")
f1IrrigatedPlotNb <- getNumericProbe("f1IrrigatedPlotNb","COWAT")
f2IrrigatedPlotNb <- getNumericProbe("f2irrigatedPlotNb","COWAT")
# f3IrrigatedPlotNb <- getNumericProbe("f3IrrigatedPlotNb","COWAT")
# f4IrrigatedPlotNb <- getNumericProbe("f4IrrigatedPlotNb","COWAT")
# f5IrrigatedPlotNb <- getNumericProbe("f5IrrigatedPlotNb","COWAT")
# f6IrrigatedPlotNb <- getNumericProbe("f6IrrigatedPlotNb","COWAT")
# f7IrrigatedPlotNb <- getNumericProbe("f7IrrigatedPlotNb","COWAT")
# f8IrrigatedPlotNb <- getNumericProbe("f8IrrigatedPlotNb","COWAT")
# f9IrrigatedPlotNb <- getNumericProbe("f9IrrigatedPlotNb","COWAT")
# f10IrrigatedPlotNb <- getNumericProbe("f10IrrigatedPlotNb","COWAT")
# f11IrrigatedPlotNb <- getNumericProbe("f11IrrigatedPlotNb","COWAT")
# f12IrrigatedPlotNb <- getNumericProbe("f12IrrigatedPlotNb","COWAT")
# f13IrrigatedPlotNb <- getNumericProbe("f13IrrigatedPlotNb","COWAT")
# f14IrrigatedPlotNb <- getNumericProbe("f14IrrigatedPlotNb","COWAT")
# f15IrrigatedPlotNb <- getNumericProbe("f15IrrigatedPlotNb","COWAT")
# f16IrrigatedPlotNb <- getNumericProbe("f16IrrigatedPlotNb","COWAT")

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
