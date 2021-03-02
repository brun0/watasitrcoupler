# INPUT FONCTION param, meteo
init_optirr = function(param,meteo)
{

### fonction initialisation
## input vecteurs meteo

  P   = meteo$P
  Rg  = meteo$Rg
  T   = meteo$T
  ET0 = meteo$etp
  ET0[which(ET0==0)] = 0.01 # pour ne pas avoir ET0=0
  
  rscx = param$rscx
  if (param$gge == 999)   {rscx = 0}
  if (param$rscx == 1000) {rscx = 0.1} 
  
  EP = ET0 / (1+rscx) # avec effet mulch 
  
  
  
  ###### Liste de constantes ######################
  
  Hz1 = 0.1
  RU  = (param$fc - param$ppf) * 1000 # valeur de la RU en mm
  RU1max = RU * Hz1 # reserve max de la couche de surface (10 cm) en mm 
  R1max  = param$fc * 1000 * Hz1 
  tetaR  = 0.04 ## avec 0.04 pour tetaR teneur en eau résiduelle, 
  ## eau très liée ne pouvant pas être évaporée
  c1 = 1        # param utilisés dans Cp
  c2 = 0.7      # param utilisés dans Cp
  
  test1 = 1 - max(sign(param$jsem-250) , 0)
  tcrit = 23 + test1 * 9  # tcrit température critique, 32 degres, ou 23 si jsem>250
  
  jlc  = 3	 # utilisé pour définir durée sur laquelle moyenne glissante pour Sw_tdm
  jcum = 10	 # utilisé pour définir durée sur laquelle moyenne glissante pour Sw_lai
  tm = param$tm - param$xt0
  
  # param pour les calculs de LAI
  somc  = 0.65 * param$tm
  stab0 = 50
  if (param$rlaimax<5.5 & param$tseuil0<=250)
  {
    stab0 = 450
  }
  # parametres utillisés pour choix de alpha dans TL et donc dans LAI 
  tseuil = tm + 1
  tsen   = tm + 10  
  ######### dens et densopt, et param cd (utilisé dans les calculs de LAI) qui en découle
  test    = sign(param$rlaimax)  # si sol nu dens=densopt=1 sinon on garde les paramètres
  dens    = test * param$dens + (1-test)
  densopt = test * param$densopt + (1-test)
  
  if (dens > densopt) # si densité plus grande que densopt cd diminué
  {
    cd = 0.4
  } else if (dens<=densopt) { 
    cd = 0.6
  }                 
  ## param pour croissance racinaire
  racplus = 0.0002
  vrac    = 1.13
  seuil   = 0.2 # utilisé pour croissance racinaire
  taux    = min(0.5 , param$taux) # utilisé pour degred et pour rap (dans croissance racinaire)
  degred  = 0.001
  if ((param$jsem >= 250) & (rscx != 0)) { degred = degred * 0.7 }
  if (taux > 0.015) { degred = 1.5 * degred }
  if (taux > 0.025) { degred = 2.5 * degred }

  jdc   = 70
  
  ####################################
  # initialisation de "variables"
  kjh    = 0
  rlaims = param$rlaimax # ajouté le 08/10/2018
  
  kg    = 0
  istr  = 0
  iccc  = 0
  ill   = 0 
  ipas1 = 0

  idrp  = 0
    
  ifoi  = 0
  ifois = 0 # pour irrigation au semis

  ###  ajout pour LAIav
  percrit  = 0 # pour compter nb jour au fur et à mesure
  sumLAIav = 0 # pour sommer LAI au fur et à mesure dans cette période
  sumLAIpav = 0 # pour sommer LAI_p au fur et à mesure dans cette période
  
  rlmin=0 #### rajouté pour fonction journalière!!!!!!!
  rlav=0 #### rajouté pour fonction journalière!!!!!!!  
  
  ###################################################################
  ## définition des vecteurs (mêmes dimensions que meteo$day et nuls)
  R1   = as.vector(meteo$day)
  R1[] = 0
  
  TPM = TPM2 = ETM = ETR = TRP = R1  # série vecteurs nuls de même dimension que R1
  R2 = R3_1 = d3 = R3 = R1   # série vecteurs nuls de même dimension que R1
  teta3 = tetaRU3 = RU1 = RU2 = RU3 = R1
  Sw_lai = R1
  Sw_lai[] = NA 
  TS = TS_p = TT = TT_p = LAI_p = LAI = LAImax = LAI_av = LAIp_av = R1
  LAI_av[] = LAIp_av[] = NA
  TDM = TDM_p = imoins = fac = R1
  crac = crat = dHz2 = ks = Hz2 = R1
  crac[] = crat[] = Hz2[] = 0.2               # on a 3 horizons dés le départ, avec Hz2= 0.2 par défaut 
  taum = R1
  taum[] = taux
  # on met rap à 1 pour commencer, idem pour fac
  fac[] = 1
  kt = R1 # compteur de jours après levée


  ###### initialisation variables pour bilan  
  Hz3 = param$profx - Hz1 - Hz2[1] 
  Cp=TPM=TP1=TP2=0
  
  ##############################################################################
  ########## CONDITIONS INITIALES - on définit valeur pour i=1
  
  
  if (param$jdsim > 1) 
  {
    R1[1]  = R1max
    RU1[1] = RU * Hz1
    R2[1]  = param$fc * 1000 * Hz2[1]
    RU2[1] = RU * Hz2[1]
    R3[1]  = param$fc * 1000 * Hz3
    RU3[1] = RU * Hz3
  } else if (param$jdsim == 1) {
    R1[1]  = param$res * Hz1 / param$profx
    RU1[1] = (param$res - param$ppf * 1000 * param$profx) * Hz1 / param$profx
    R2[1]  = param$res *Hz2[1] / param$profx
    RU2[1] = (param$res - param$ppf * 1000 * param$profx) * Hz2[1] / param$profx
    R3[1]  = param$res * Hz3 / param$profx
    RU3[1] = (param$res - param$ppf * 1000 * param$profx) * Hz3 / param$profx
    ratio_a = min(100 , 100* (RU1[1] + RU2[1]) / ((param$fc-param$ppf)*1000*(Hz1+Hz2[1])))
  }
  
  teta1 = R1[1] / (Hz1*1000)
  teta2 = R2[1] / (Hz2[1]*1000) #
  teta3[1]   = R3[1] / (Hz3*1000) #
  tetaRU3[1] = RU3[1] / (Hz3*1000)
  
  d1  = max(0, R1[1]-R1max) #verif
  ES0 = EP[1]        # effet mulch
  ES1 = min(ES0 , RU1[1])
  R2max  = param$fc * 1000 * Hz2[1]
  RU2max = RU * Hz2[1]
  xsol  = min(1,RU2[1]/RU2max) # dans la suite du code (boucle) on a i-1
  rks   = param$rksol * exp(-(1-xsol)) 
  ES2   = min(RU2[1] , (ES0-ES1)*rks) # Dans le code (boucle) on a R2[i-1] 
  ETR[1] = TP1 + ES1 + TP2 + ES2
  ETM[1] = ES0 + TPM ## ajouté le 06/06/2019
  
  d2     = max(0, R2[1] - R2max) 
  RU3max = RU * Hz3 
  R3max  = param$fc * 1000 * Hz3
  d3[1]  = max(0, R3[1] - R3max) 
  
  
  # valeurs nulles pour TPM, ETM, TP1, TP2_1, TP2,
  # pour TT, TT_p, TL, TL_p, TS, TS_p, LAI, LAI_p, LAImax (vecteurs nuls)
  # pour Ir, TDM, Cp, Kc, imoins, fac, rap,
  # pour crac, dcrac, crat, dcrat, dHz2, ks
  # Hz2=0.2 , Hz3 = param$profx-Hz2-Hz1 , Sw=NA
  
  
  #####################################
  # on génère listes qui seront transmises pour la suite du code
  cstes = list (Hz1=Hz1,RU=RU,RU1max=RU1max,R1max=R1max,tetaR=tetaR,c1=c1,c2=c2,tcrit=tcrit,jlc=jlc,jcum=jcum,tm=tm,somc=somc,stab0=stab0,tseuil=tseuil,tsen=tsen,
			dens=dens,densopt=densopt,cd=cd,racplus=racplus,vrac=vrac,seuil=seuil,taux=taux,degred=degred,jdc=jdc)

  inval = list(kjh=kjh,rlaims=rlaims,kg=kg,istr=istr,iccc=iccc,ill=ill,ipas1=ipas1,idrp=idrp,ifoi=ifoi,ifois=ifois,percrit=percrit,sumLAIav=sumLAIav,sumLAIpav=sumLAIpav,
			rlmin=rlmin,rlav=rlav)
  
  vect  = list(ET0=ET0,EP=EP,R1=R1,R2=R2,R3_1=R3_1,d3=d3,R3=R3,teta3=teta3,tetaRU3=tetaRU3,RU1=RU1,RU2=RU2,RU3=RU3,Sw_lai=Sw_lai,Cp=Cp,
				TPM=TPM,TPM2=TPM2,ETM=ETM,ETR=ETR,TRP=TRP,TS=TS,TS_p=TS_p,TT=TT,TT_p=TT_p,LAI_p=LAI_p,LAI=LAI,LAImax=LAImax,LAI_av=LAI_av,LAIp_av=LAIp_av,
				TDM=TDM,TDM_p=TDM_p,imoins=imoins,fac=fac,crac=crac,crat=crat,dHz2=dHz2,ks=ks,Hz2=Hz2,taum=taum,kt=kt)
				

  list(cstes=cstes , inval=inval , vect=vect)
  
  
  }