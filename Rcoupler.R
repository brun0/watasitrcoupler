#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#################      R coupler      ################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script makes the WatASit model from Cormas plateform to communicate
# with the Optirrig model implemented in R Software to generate
# simulations for EMS 2020 paper
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2019, October, by
# B. Bonté -> make RCormas function to get/set Cormas attributes/probes
# M. Delmas -> make adapted daily Optirrig function, adapt it for
# meadows
# B. Richard -> make work together, make optiParams funcion and
# generate Optirrig climate file with specific R script
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


rm(list=ls());
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1. R Settings #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 1.1 Set directory for coupling #######
# Not necessary if you open watasit_rcoupler.Rproject
wd <- getwd()

####### 1.2 Load functions #######
wd_Functions <- file.path(wd,"Rfunctions/")
for(FileName in list.files(wd_Functions, pattern="\\.[Rr]$")){ source(file.path(wd_Functions,FileName)); }

####### 1.3 Load libraries #######
load <- c(require(zoo), require (multiplex), require(tidyr),require(ggplot2),require(dplyr),require(doParallel)); if(any(!load)){ cat("Error: a package is not installed \n"); stop("RUN STOPPED",call.=FALSE); };

####### 1.4 Core parallelism #######
cores <- parallel:::detectCores(); registerDoParallel(cores-2);


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 2. Simulation Settings and inputs #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### 2.0 specify wether Otirrig is used or not and simu starts #######
with_optirrig <- F
cormas_doy_start <- 121 # day of the year of first step in cormas
J2k_doy_start <- 1 # day of the year of first step in J2K
optirrig_doy_start <- NA # day of the year of first step in Optirrig

####### 2.1 Specification of case study, year and duration #######
case_study_name <- "Aspres"
year_sim <- 2017
cormas_sim_day_nb <- 4

####### 2.2 Importation of meteo data input  #######
input_meteo = read.csv(file.path(wd, 'climatefile/climate_buech_2017.csv'), header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)
meteo = input_meteo[which(input_meteo$year == year_sim),]
str(meteo)

####### 2.3 Generation of an Optirrig paramfile for each WatASit plots  #######
list_idParcel <- NULL
if (with_optirrig) {
  list_idParcel <- optiParams(
    paste0(wd,'paramfiles/'),
    case_study_name,
    'watasit.csv',
    'paramDB.csv',
    'climate_buech_2017.csv',
    year_sim,
    1,
    365,
    'irrig_file_watasit.dat'
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3. WatASit initialization and J2K initialisation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 3.1 Connexion and opening of WatASit model #######
# Open Cormas: dans le répertoire de cormas taper: "wine cormas.exe"
# Ça ouvre une image de cormas avec le modèle chargé mais ne pas regarder
# Dans l'interface principale, aller dans le menu: "simulation/Analysis/cormas<-->R/Sart webservie for R".
# Un petit logo R avec un point vert doit apparaitre.. Le tour est joué.
r <- openModel("COWAT", parcelFile="WatASit[EMSpaper].pcl")

####### 3.2 Activation of probes about crops (Facultatif: to get data from cormas) #######
# probe_names <- c("abandonedCropEvent", "ASAinquiries", "exceedMaxWithdrawalEvent", "qIntake", "unrespectRestrictionEvent", "sumQOfEwaterReleases", "f1IrrigatedPlotNb", "f2irrigatedPlotNb", "f3irrigatedPlotNb", "f5irrigatedPlotNb", "f6irrigatedPlotNb", "f7irrigatedPlotNb", "f10irrigatedPlotNb", "f11irrigatedPlotNb", "f12irrigatedPlotNb","f14irrigatedPlotNb", "f16irrigatedPlotNb")
# for (i in 1:length(probe_names)) { r <- activateProbe(probe_names[i],"COWAT") }

####### 3.3 Choose of WatASit initial state and time step function (scenarios) #######
# Note that the model is not initialized, we just set the init method..
r <- setInit("INIT_2017_54x44") # Initialization choice
r <- setStep("R_goBaselineStep:") # Scenario choice

####### 3.4 Initialize Cormas model #######
r <- initSimu()

####### 3.5 Initialize J2K model #######
# Lancer J2K de la manière suivante.
# En étant dans le dossier "superjams" (qui vient de l'archive superjams.zip) :
# java -jar jams-starter.jar -m data/J2K_cowat/j2k_cowat_buech_ju_couplage.jam -n
# et hop ça lance juste le modèle, pas d'interface graphique, pas  d'éditeur de modèle. Pour l'arrêter : CTRL+C .
# S'il s'arrête tout seul  au bout de 2 minutes d'inactivité : CTRL+C et on peut le relancer avec la même commande.
#  "Rfunctions/Rj2k.R".

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 4. Initialization of Optirrig model #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (with_optirrig) {
  #param_frame <- data.frame(); irr <- data.frame()
  #for (i in 1:length(list_idParcel)){
  #  ####### 4.1 Load params of each plot #######
  #  param = read.csv(paste0(wd,'paramfiles/paramfiles_',case_study_name,'/',list_idParcel[i],'/parF', list_idParcel[i],'.csv'), header = TRUE,sep=",",dec = ".",stringsAsFactor=FALSE)
  #
  #  ####### 4.2 Create frame with all parameters #######
  #  param_frame <- rbind(param_frame, param)
  #
  #  ####### 4.3 Create frame with all irrigation vectors  #######
  # I1   = as.vector(meteo$day) ; I1[] = 0; I2 = I1 # I1 is surface irrigation and I2  I2 is deep buried irrigation (I2 is null)
  # irr = rbind(irr,I1)
  #}
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 5. Run simulation #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### 5.1 Create results dataFrame #######
crop_results <- data.frame(idParcel = NULL, wsi = NULL, lai = NULL, hi = NULL, cropMaturitySignal = NULL)
farmers_results <- data.frame(id = NULL, day = NULL, nbFloodPlotAffToday = NULL, dosCounter = NULL)


####### 5.2 Run J2K from 1 DOY to DOY 120 (1er mai) to simulate the new state of the watershed before the irrigation campaign #######  #######
j2kMakeStep(cormas_doy_start - 1)
reachQTable = j2kGet("reach")

####### 5.3 Run Optirrig simulation without WataSit from 1 DOY to DOY 120 (1er mai) to simulate the new state of crops out of irrigation campaign #######
if (with_optirrig) {
## for (day in optirrig_doy_start:(cormas_doy_start - 1)){
#
#  ####### 5.3.1 Initialize optirrig on day 1 #######
#  if (day == 1) {
#    cstes_list <- list()
#    inval_list <- list()
#    vect_list <- list()
#    for (i in 1:length(list_idParcel)){
#      init <- init_optirr(param_frame[i,], meteo)
#      cstes = init$cstes; cstes_list <- rbind(cstes_list, cstes) # Constants
#      inval = init$inval; inval_list <-  rbind(inval_list, inval) # Calculation values for which the history is not required (list of values)
#      vect  = init$vect ; vect_list <-  rbind(vect_list, vect) # Vector of stored state variables as time series in vectors (list of vectors)
#    }
#  }
#
#  ####### 5.3.2 Simulate Optirrig on other days #######
#  if (day != 1) {
#    inval2_list <- list()
#    vect2_list <- list()
#    for (i in 1:length(list_idParcel)){
#      cat("Simulation of day",day, "and parcel number",i,"(idParcel =",list_idParcel[i],")","\n")
#      I1 = I2 # I2 is deep irrigation (buried drip), I2 is null
#      param<-param_frame[i,]; cstes<-cstes_list[i,];  inval<-inval_list[i,]; vect<-vect_list[i,]
#      optirday = daily_optirr(param,
#                              meteo,
#                              cstes,
#                              inval,
#                              vect,
#                              I1, # Surface irrigation
#                              I2, # Deep irrigation (buried drip)
#                              day) # Time step
#      inval2 = optirday$inval ; inval2_list <- rbind(inval2_list, inval2) ; inval_list[i,] <- inval2 # New constants
#      vect2  = optirday$vect ; vect2_list <- rbind(vect2_list, vect2) ; vect_list[i,] <- vect2 # New vectors
#    }
#  }
#}
}

####### 5.4 Run WatASit-Optirrig coupled simulation from DOY 121 (1er mai) during the irrigation campaign #######
for (day in cormas_doy_start:(cormas_doy_start + cormas_sim_day_nb)) {
  ####### 5.4.1 Update Cormas Meteo #######
  P <- meteo$P
  setAttributesOfEntities("p", "Meteo", 1, P[day]) # Precipitation conditions of the day
  p_forecast = sum(c(P[day], P[day+1], P[day+2]), na.rm = TRUE)
  if (p_forecast > 0) {
    p_forecast = 1
  }
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
  if (day >= 11) {
    p_cumTenDays = sum(c(P[day-1], P[day-2], P[day-3], P[day-4], P[day-5], P[day-6], P[day-7], P[day-8], P[day-9], P[day-10]), na.rm = TRUE)
  }
  setAttributesOfEntities("p_cumTenDays", "Meteo", 1, p_cumTenDays) # Calculate cumulative precipitation for the last 10 days
  # setAttributesOfEntities("p_cumTenDays", "Meteo", 1, 10)
  # if (day <= 11) {p_cumFifteenDays = p_cumTenDays}
  # if (day == 12) {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-11]), na.rm = TRUE)}
  # if (day == 13) {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-11], P[day-12]), na.rm = TRUE)}
  # if (day == 14) {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-11], P[day-12], P[day-13]), na.rm = TRUE)}
  # if (day == 15) {p_cumFifteenDays = sum(c(p_cumTenDays,P[day-11], P[day-12], P[day-13], P[day-14]), na.rm = TRUE)}
  if (day >= 16) {
    p_cumFifteenDays = sum(c(p_cumTenDays, P[day-11], P[day-12], P[day-13], P[day-14], P[day-15]), na.rm = TRUE)
  }
  else {
    p_cumFifteenDays = sum(c(p_cumTenDays, P[day-1], P[day-2], P[day-3], P[day-4], P[day-5]), na.rm = TRUE)
  }
  setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, p_cumFifteenDays) # Calculate cumulative precipitation for the last 15 days
  # setAttributesOfEntities("p_cumFifteenDays", "Meteo", 1, 50)

  ####### 5.4.2 Run coupled simulation of 24 hours #######
  r <- runSimu(duration = 24)
  response <- gettext(r[[2]])
  # To check if runSimu is done
  if (response != "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:ns=\"urn:vwservices\"><SOAP-ENV:Body><ns:RunSimuResponse><ns:result>true</ns:result></ns:RunSimuResponse></SOAP-ENV:Body></SOAP-ENV:Envelope>") {
    stop("RUN STOPPED", call.=FALSE)
  }
  obs1 <- getAttributesOfEntities("floodAffCounter", "Efarmer")
  obs2 <- getAttributesOfEntities("floodActCounter", "Efarmer")
  obs <- left_join(obs1, obs2, by = "id")
  obs$day = day
  farmers_results <- farmers_results %>%
    bind_rows(obs)

  ####### 5.4.3 Get the state of crops from Cormas #######
  idParcel      <- getAttributesOfEntities("idParcel", "Ecrop")
  list_idParcel <- idParcel$idParcel
  harvestSignal <- getAttributesOfEntities("harvestSignal", "Ecrop")
  irriDailyDose <- getAttributesOfEntities("irriDailyDose", "Ecrop")

  ####### 5.4.4 Simulate the new state of crops with Optirrig #######
  if (with_optirrig) {
    if (day != 1) {
      inval2_list <- list()
      vect2_list <- list()
      for (i in 1:length(list_idParcel)) {
        cat("Simulation of day", day, "and parcel number", i, "(idParcel =", list_idParcel[i], ")", "\n")
        irr[i,day]  <- irriDailyDose$irriDailyDose[i]
        irr <- as.matrix(irr)
        I1 = irr[i,] # Update irrigation from Cormas
        param <- param_frame[i,]
        cstes <- cstes_list[i,]
        inval <- inval_list[i,]
        vect <- vect_list[i,]
        optirday = daily_optirr(
          param,
          meteo,
          cstes,
          inval,
          vect,
          I1, # Surface irrigation
          I2, # Deep irrigation (buried drip)
          day # Time step
        )
        inval2 = optirday$inval
        inval2_list <- rbind(inval2_list, inval2)
        inval_list[i,] <- inval2 # News constants
        vect2  = optirday$vect
        vect2_list <- rbind(vect2_list, vect2)
        vect_list[i,] <- vect2 # New vectors
      }
    }
  }
  ####### 5.4.5 Set the irrigation in J2K #######
  j2kSet("drip", c(1,2,3), c(100, 100, 100)) # Mais en utilisant en fait les irriDailyDose ou truc du genre
                                              # récupérés ci-dessus depuis cormas
  #TODO La ligne précédente renvoie une erreur chez moi, c'est pour ça que je l'ai commenté..:
  # C'est bon ça roule maintenant

  ####### 5.4.6 Simulate the new state of watershed with J2K #######
  j2kMakeStep(1)
  reachQTable = j2kGet("reach")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # ATTENTION JE NE COMPRENDS PAS POURQUOI CETTE SECTION N'EST PAS
      # EN  5.4.7 vu qu'elle fait partie du pas de temps journalier..
  ####### 6.Get new crop state from Optirrig simulations #######
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (with_optirrig) {
    ####### 6.1 Get new crop states #######
    # newLAI<-vectList
    # newCropState <- data.frame(list_idParcel, vect2Frame$wsi, vect2Frame$lai, vect2Frame$hi, vect2Frame$cropMaturitySignal) # Tableau final avec les  sorties Optirrig pour chaque idParcel

    ####### 6.2 Set the new state of crops in cormas #######
    #setAttributesOfEntities("wsi", "Ecrop", idParcel$id, newCropState$wsi)    # Vérifier qu'on prend bien les bonnes parcelles!!!!
    #setAttributesOfEntities("lai", "Ecrop", idParcel$id, newCropState$lai)
    #setAttributesOfEntities("hi", "Ecrop", idParcel$id, newCropState$hi)
    #setAttributesOfEntities("cropMaturiySignal", "Ecrop", idParcel$id, newCropState$cropMaturitySignal)

    ####### 6.3 Save results in R (even though they are also in cormas) #######
    # newCropState$irrigation <- irriDailyDose$values
    # newCropState$day <- day
    # cropResults <- rbind(cropResults, newCropState )
  }

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 7. Observe the evolution of coupled dynamics #######
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# abandonedCropEvent <- getNumericProbe("abandonedCropEvent","COWAT")
#  ASAinquiries <- getNumericProbe("ASAinquiries","COWAT")
#  exceedMaxWithdrawalEvent <- getNumericProbe("exceedMaxWithdrawalEvent","COWAT")
#  qIntake <- getNumericProbe("qIntake","COWAT")
#  unrespectRestrictionEvent <- getNumericProbe("unrespectRestrictionEvent","COWAT")
#  f1IrrigatedPlotNb <- getNumericProbe("f1IrrigatedPlotNb","COWAT")
#  f2IrrigatedPlotNb <- getNumericProbe("f2irrigatedPlotNb","COWAT")
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

