#############################################
#Function to generate Optirrig parameter files#
#############################################

optiParams = function(dir, case_study_name, shapefile_name, paramDBfile_name, climatefile_name, annee, jdsim, jfsim, irrigfile_name) {
  
  
#case_study_name<-"ASPRES"
param_folder_name<-paste0("paramfiles_",case_study_name)
dir.create(paste0(dir,param_folder_name))

#Load_farm_plots_features
plots <- read.table(paste0(dir,shapefile_name), header = T, sep = ";", dec = ",", na.strings = "-9999")

#Load_Optirrig_params_database
paramDB <- read.table(paste0(dir,paramDBfile_name), header = T, sep = ";", dec = ",", na.strings = "-9999")

#Fill_in_param_file_for_each_plot
for (i in 1:dim(plots)[1]) {
  classCult <- plots$CLASS_CULT[i]
  typeSoil <- toString(plots$TYPESOIL[i])
  if ((classCult != 1) & (classCult != 5) & (classCult != 7)){  #1: estives, 5: orchads, 7: various
    #1/Context_params 
    climate<-climatefile_name                                                         #climatic file to be read
    annee<-annee                                                                      #year or fictional year to grow winter
    jdsim<-jdsim                                                                      #julian day of the beginning of the simulation (DOY)
    jfsim<-jfsim                                                                      #julian end of simulation day (DOY)
    #2/Soil_params
    ppf<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$ppf          #wilting point
    profx<-plots$EPAIS[i]                                                             #maximum depth between roots and soil profil
    ru<-plots$RU[i]                                                                   #useful reserve (ru)
    fc<- (ppf + ru)/ (1000*profx)                                                     #field capacity
    rkru<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rkru        #ratio between the easily usable reserve (rfu) and the useful reserve (ru)
    res<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$res          #total profile reserve on the day of the start of the simulation (jdsim)
    rksol<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rksol      #analogous kc for bare ground, "evapotranspiration resistance"
    #3/Temp_params
    base<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$base        #base temperature
    tseuil0<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tseuil0  #root system installation temperature
    xt0<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$xt0          #emergence temperature
    tjdc<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tjdc        #temperature of the first phenological stage
    tjfc<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tjfc        #temperature of the second phenological stage
    tm<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tm            #max LAI reaching temperature in the absence of stress
    tfmat<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$tfmat      #maturity temperature
    #4/Plant_params
    taux<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$taux        #root growth rate
    rkcm<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rkcm        #maximum value of the cultivation coefficient kc
    rlaimax<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rlaimax  #maximum value of the LAI
    rmg<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rmg          #efficiency of radiation use
    hi<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$hi            #maximum harvest index
    coefas0<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$coefas0  #harmfulness of water stress to LAI
    cwx<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$cwx          #harmfulness of water stress for TDM
    alpha1<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$alpha1    #first shape parameter of the LAI
    alpha2<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$alpha2    #second shape parameter of the LAI (growth stage)
    gama<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$gama        #third shape parameter of the LAI (senescence stage)
    dens<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$dens        #planting density
    densopt<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$densopt  #optimal planting density (for optimal harvest index)
    rlcrit<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rlcrit    #average of the LAI over a critical period, below which HI is penalized
    xl1<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$xl1          #coefficient of penalty of the harvest index
    pourc<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$pourc      #percentage grain moisture for wet grain yield
    #4/Irri_params
    jsem <-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$jsem       #sowind day (DOY)
    jrecol<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$jrecol    #number of days after sowing for harvesting
    rscx <-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$rscx       #mulch effect
    gge<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$gge          #irrigation technique: 0 = aspersion
    itest<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$itest      #itest = 0 to read an irirgation schedule
    irrigfile<-irrigfile_name                                                         #irrigation file name
    th_w<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$th_w        #if itest=1
    dosap<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$dosap      #if itest=1 (mm)
    dosem<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$dosem      #irrigation at sowing (mm)
    jdir<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$jdir        #DOY of the first possible irrigation
    jfir<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$jfir        #DOY of the last possible irrigation
    quota<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$quota      #quota of irrigation (mm)
    selini<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$selini    
    ssel<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$ssel
    csel<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$csel
    ciw<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$ciw
    css<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$css
    f1<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$f1            #fixed management costs (keuros/ha)
    f2<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$f2            #variable costs (euros/m3)
    f3<-subset(paramDB, CLASS_CULT == classCult & TYPESOIL == typeSoil)$f3            #crop selling price (keuros/ton)
    
#Generate
parF <- data.frame(climate,annee,jdsim,jfsim,fc,ppf,profx,rkru,res,rksol,base,tseuil0,xt0,tjdc,tjfc,tm,tfmat,taux,rkcm,rlaimax,rmg,hi,coefas0,cwx,alpha1,alpha2,gama,dens,densopt,rlcrit,xl1,pourc,jsem,jrecol,rscx,gge,itest,irrigfile,th_w,dosap,dosem,jdir,jfir,quota,selini,ssel,csel,ciw,css,f1,f2,f3)

#Save
# output_file_name <- paste0('parF',plots$ID_PARCEL[i])
output_file_name <- paste0('parF',plots$ID_PARCEL[i])
dir.create(paste0(dir,param_folder_name,'/',plots$ID_PARCEL[i]))
write.csv(parF,paste0(dir,param_folder_name,'/',plots$ID_PARCEL[i],'/',output_file_name,'.csv'), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
#write.dat(parF,paste0(dir,param_folder_name,'/',output_file_name))
  }
}
list_idParcel <- as.numeric(list.files(paste0(dir,param_folder_name,'/'), full.names=FALSE)) #liste des parcelles modélisées (ne sont pas modélisées les vergers, jachères et estives)
return(list_idParcel)
}
