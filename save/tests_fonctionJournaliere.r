rm(list=ls())

#########################################
##### REPERTOIRE DE TRAVAIL A DEFINIR ###
#########################################
# dirfile = "~/cormas2017_package/Models/COWAT/R-connection/fonction_journaliereTest/"
dirfile = "/home/bastien/Documents/2017-2020_These_GEAU/Work_Optirrig/Optirrig/WatASit/OPTIRRIG_fonction_journaliereTest/"
setwd(dirfile)

################################################################################
################# FICHIERS ENTREE (param?tres, climat, irrigation) #############
################################################################################

########## FICHIER PARAMETRES #################
param_file = read.csv("cas_test/param_etm_2011.dat",header = TRUE,sep="",dec = ".",stringsAsFactor=FALSE) # NOM A INDIQUER
param = param_file[1,]

# Fichier meteo (lu dans les param)
met        = as.character(paste("cas_test/",as.character(param$climate),sep="")) # CHEMIN A INDIQUER
meteo      = read.csv(file=met,header=TRUE,sep=",",dec=".",stringsAsFactors=FALSE)
sim_period = which(meteo$year ==param$annee) # Selection periode de simulation
meteo      = meteo[sim_period,]

############################################
########## FICHIER IRRIGATION #################
I1   = as.vector(meteo$day)
I1[] = 0
I2 = I1

Irr = data.frame(doy=meteo$doy,I=0) #
if (param$itest==0) # si fichier avec date irrigations
{
  # Nom du fichier irrigation
  dir_irrig = param$irrigfile ############# A CONSERVER MAIS DANS CE TEST:
  # dir_irrig = "irrig_file.dat" 
  file_ir = read.csv(paste0("cas_test/",as.character(dir_irrig)), header=FALSE, sep=",", skip=1) # on saute la premiere ligne (commentaire) 
  file_ir = as.numeric(file_ir[1,]) #on ne collecte que la premi?re ligne pour le moment
  nbI = as.numeric(file_ir[1])
  
  for (i in 1:nbI)
  {
    N=which(Irr$doy == file_ir[i*2])
    Irr$I[N] = file_ir[i*2+1]  
  }
  if (param$gge == 0) ## aspersion donc irrigation de surface
  {
    I1 = Irr$I
  } else if (param$gge == 999) # gag enterr? donc irrigation dans Hz
  {
    I2 = Irr$I 
    ### ajout pour la premi?re irrigation, si apr?s semis, sera ? priori en aspersion dans pour I1 (dans les 10 premiers jours apr?s semis)
    Irr_1st = which(Irr$I>0)[1]  # la premiere irrigation
    if ((Irr_1st >= param$jsem) & (Irr_1st <= param$jsem+10))
    {
      I1[Irr_1st] = Irr$I[Irr_1st]
      I2[Irr_1st] = 0
    }
  }
}


############################################
############################################



source("/home/bastien/Documents/2017-2020_These_GEAU/Work_Optirrig/Optirrig/WatASit/OPTIRRIG_fonction_journaliereTest/fonction_journaliere/optirrig_init_fonctionJour.r")
source("/home/bastien/Documents/2017-2020_These_GEAU/Work_Optirrig/Optirrig/WatASit/OPTIRRIG_fonction_journaliereTest/fonction_journaliere/optirrig_fonctionJour.r")

i=1

init = init_optirr(param,meteo)
# Constantes 
cstes = init$cstes
# Valeurs de calcul dont on n'a pas besoin de l'historique (liste de valeurs)
inval = init$inval
# Vecteur de variables d'états stokés sous forme de séries temporelles dans des vecteurs (liste de vecteurs)
vect  = init$vect

for (i in 2:365) {
  optirday = daily_optirr(param,
                          meteo,
                          cstes,
                          inval,
                          vect,
                          I1, # Irrigation de surface 
                          I2, # Irrigation de profondeur (goutte à goutte enterré)
                          i) # Pas de temps.
  # cstes2 = optirday$cstes # A priori ne change pas donc pas besoin de recalculer (à vérifier)
  inval = optirday$inval
  vect2  = optirday$vect

}























