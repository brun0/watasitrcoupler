#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######      Function to generate Optirrig parameter files      #########
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code developed in 2019, Oct, by B. Richard
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

optiParams = function(dir, case_study_name, shapefile_name, paramDBfile_name, climatefile_name, jdsim, jfsim, itest, gge, dosap, th_w) {
  
  
#case_study_param_folder
if (dir.exists(paste0(dir, "paramfiles_",case_study_name))) {unlink(paste0(dir, "paramfiles_",case_study_name), recursive = TRUE)}
dir.create(paste0(dir,"paramfiles_",case_study_name))

#Load_farm_plots_features
plots <- read.table(paste0(dir,shapefile_name), header = T, sep = ";", dec = ",", na.strings = "-9999")

#Load_Optirrig_params_database
paramDB <- read.table(paste0(dir,paramDBfile_name), header = T, sep = ";", dec = ",", na.strings = "-9999")

#Fill_in_param_file_for_each_plot
for (i in 1:dim(plots)[1]) {
  classCult <- plots$CLASS_CULT[i]
  typeSoil <- toString(plots$TYPESOIL[i])
    #1/Context_params 
    climate<-climatefile_name                                                         #climatic file to be read
    annee<-NA                                                                      #year or fictional year to grow winter
    jdsim<-jdsim                                                                      #julian day of the beginning of the simulation (DOY)
    jfsim<-jfsim                                                                      #julian end of simulation day (DOY)
    #2/Soil_params
    ppf<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$ppf))       #wilting point
    profx<-plots$EPAIS[i] / 100                                                       #maximum depth between roots and soil profil in meters
    ru<-plots$RU[i]                                                                   #useful reserve (ru) en mm/m
    fc<-(ppf+ru) / (1000*profx)                                                     #field capacity
    rkru<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rkru))        #ratio between the easily usable reserve (rfu) and the useful reserve (ru)
    res<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$res))         #total profile reserve on the day of the start of the simulation (jdsim)
    rksol<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rksol))      #analogous kc for bare ground, "evapotranspiration resistance"
    #3/Temp_params
    base<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$base))        #base temperature
    tseuil0<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tseuil0))  #root system installation temperature
    xt0<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$xt0))          #emergence temperature
    tjdc<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tjdc))        #temperature of the first phenological stage
    tjfc<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tjfc))        #temperature of the second phenological stage
    tm<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tm))            #max LAI reaching temperature in the absence of stress
    tfmat<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tfmat))      #maturity temperature
    #4/Plant_params
    taux<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$taux))        #root growth rate
    rkcm<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rkcm))        #maximum value of the cultivation coefficient kc
    rlaimax<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rlaimax))  #maximum value of the LAI
    rmg<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rmg))          #efficiency of radiation use
    hi<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$hi))            #maximum harvest index
    coefas0<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$coefas0))  #harmfulness of water stress to LAI
    cwx<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$cwx))          #harmfulness of water stress for TDM
    alpha1<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$alpha1))    #first shape parameter of the LAI
    alpha2<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$alpha2))    #second shape parameter of the LAI (growth stage)
    gama<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$gama))        #third shape parameter of the LAI (senescence stage)
    dens<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$dens))        #planting density
    densopt<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$densopt))  #optimal planting density (for optimal harvest index)
    rlcrit<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rlcrit))    #average of the LAI over a critical period, below which HI is penalized
    xl1<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$xl1))          #coefficient of penalty of the harvest index
    pourc<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$pourc))      #percentage grain moisture for wet grain yield
    #4/Irri_params
    jsem <-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$jsem))       #sowind day (DOY)
    jrecol<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$jrecol))    #number of days after sowing for harvesting
    rscx <-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rscx))       #mulch effect
    gge<- gge          #irrigation technique: 0 = aspersion
    itest <- itest    #itest = 0 to read an irirgation schedule
    irrigfile<-'NA'                                                         #irrigation file not used
    #th_w<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$th_w))        #if itest=1
    th_w <- th_w
    dosap<- dosap #if itest=1 (mm)
    dosem<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$dosem))      #irrigation at sowing (mm)
    jdir<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$jdir))        #DOY of the first possible irrigation
    jfir<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$jfir))        #DOY of the last possible irrigation
    quota<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$quota))      #quota of irrigation (mm)
    selini<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$selini))    
    ssel<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$ssel))
    csel<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$csel))
    ciw<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$ciw))
    css<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$css))
    f1<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$f1))            #fixed management costs (keuros/ha)
    f2<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$f2))            #variable costs (euros/m3)
    f3<-as.numeric(as.character(subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$f3))            #crop selling price (keuros/ton)
    
#Generate
parF <- data.frame(climate,annee,jdsim,jfsim,fc,ppf,profx,rkru,res,rksol,base,tseuil0,xt0,tjdc,tjfc,tm,tfmat,taux,rkcm,rlaimax,rmg,hi,coefas0,cwx,alpha1,alpha2,gama,dens,densopt,rlcrit,xl1,pourc,jsem,jrecol,rscx,gge,itest,irrigfile,th_w,dosap,dosem,jdir,jfir,quota,selini,ssel,csel,ciw,css,f1,f2,f3)

#Save
output_file_name <- paste0('parF',plots$ID_PARCEL[i])
dir.create(paste0(dir,"paramfiles_",case_study_name,'/',plots$ID_PARCEL[i]))
write.csv(parF,paste0(dir,"paramfiles_",case_study_name,'/',plots$ID_PARCEL[i],'/',output_file_name,'.csv'), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
}
list_idParcel <- as.numeric(list.files(paste0(dir, "paramfiles_",case_study_name,'/'), full.names=FALSE))
return(list_idParcel)
}
