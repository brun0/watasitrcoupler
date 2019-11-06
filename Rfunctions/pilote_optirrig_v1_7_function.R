
pilote_function = function(param, meteo, I1, I2)
{

  P   = meteo$P
  Rg  = meteo$Rg
  T   = meteo$T
  ET0 = meteo$etp
  ET0[which(ET0==0)] = 0.01 # pour ne pas avoir ET0=0
  
  rscx = param$rscx
  if (param$gge == 999)   {rscx = 0}
  if (param$rscx == 1000) {rscx = 0.1} 
  
  EP = ET0 / (1+rscx) # avec effet mulch 
  
  ####### 'paramètres' donnés et directement calculés
  
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
  kj1  = 0 # utilisé ensuite comme compteur de jour après TS_p au dessus tsen
  
  # tm 
  tm = param$tm - param$xt0
  
  # param pour les calculs de LAI
  somc  = 0.65 * param$tm
  stab0 = 50
  if (param$rlaimax<5.5 & param$tseuil0<=250)
  {
    stab0 = 450
  }
  
  kjh    = 0
  rlaims = param$rlaimax # ajouté le 08/10/2018
  
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
  
  kg    = 0
  istr  = 0
  iccc  = 0
  ill   = 0 
  ipas1 = 0
  jdc   = 70
  idrp  = 0
  ## param pour croissance racinaire
  racplus = 0.0002
  vrac    = 1.13
  seuil   = 0.2 # utilisé pour croissance racinaire
  taux    = min(0.5 , param$taux) # utilisé pour degred et pour rap (dans croissance racinaire)
  degred  = 0.001
  if ((param$jsem >= 250) & (rscx != 0)) { degred = degred * 0.7 }
  if (taux > 0.015) { degred = 1.5 * degred }
  if (taux > 0.025) { degred = 2.5 * degred }
  
  ifoi  = 0
  ifois = 0 # pour irrigation au semis

  ###  ajout pour LAIav
  percrit  = 0 # pour compter nb jour au fur et à mesure
  sumLAIav = 0 # pour sommer LAI au fur et à mesure dans cette période
  sumLAIpav = 0 # pour sommer LAI_p au fur et à mesure dans cette période
  
  ## définition des vecteurs (mêmes dimensions que meteo$day et nuls)
  R1   = as.vector(meteo$day)
  R1[] = 0
  
  d1 = ES0 = TPM = TPM2 = ETM = ES1 = ES2 = TP1 = TP2 = ETR = TRP = R1  # série vecteurs nuls de même dimension que R1
  d2 = R2 = R3_1 = d3 = R3 = R1   # série vecteurs nuls de même dimension que R1
  teta1 = teta2 = teta3 = tetaRU3 = RU1 = RU2 = RU3 = R1
  ratio_a = R1
  Sw_lai = Sw_tdm = R1
  Sw_lai[] = Sw_tdm[] = NA 
  TS = TS_p = TT = TT_p = TL = TL_p = LAI_p = LAI = LAImax = LAI_av = LAIp_av = R1
  LAI_av[] = LAIp_av[] = NA
  TDM = TDM_p = imoins = fac = Kc = Kc_p = Cp = R1
  crac = dcrac = crat = dcrat = dHz2 = ks = Hz2 = Hz3 = R1
  crac[] = crat[] = Hz2[] = 0.2               # on a 3 horizons dés le départ, avec Hz2= 0.2 par défaut 
  Hz3[] = param$profx - Hz1 - Hz2[] 
  taum = R1
  taum[] = taux
  # on met rap à 1 pour commencer, idem pour fac
  fac[] = 1
  kt = R1 # compteur de jours après levée
  
  # rap = 1 ##
  
  ##############################################################################
  ########## CONDITIONS INITIALES - on définit valeur pour i=1!!!!!!!!!!!!!!!!!!
  
  
  if (param$jdsim > 1) 
  {
    R1[1]  = R1max
    RU1[1] = RU * Hz1
    R2[1]  = param$fc * 1000 * Hz2[1]
    RU2[1] = RU * Hz2[1]
    R3[1]  = param$fc * 1000 * Hz3[1]
    RU3[1] = RU * Hz3[1]
  } else if (param$jdsim == 1) {
    R1[1]  = param$res * Hz1 / param$profx
    RU1[1] = (param$res - param$ppf * 1000 * param$profx) * Hz1 / param$profx
    R2[1]  = param$res *Hz2[1] / param$profx
    RU2[1] = (param$res - param$ppf * 1000 * param$profx) * Hz2[1] / param$profx
    R3[1]  = param$res *Hz3[1] / param$profx
    RU3[1] = (param$res - param$ppf * 1000 * param$profx) * Hz3[1] / param$profx
    ratio_a[1] = min(100 , 100* (RU1[1] + RU2[1]) / ((param$fc-param$ppf)*1000*(Hz1+Hz2[1])))
  }
  
  teta1[1]   = R1[1] / (Hz1*1000)
  teta2[1]   = R2[1] / (Hz2[1]*1000) #
  teta3[1]   = R3[1] / (Hz3[1]*1000) #
  tetaRU3[1] = RU3[1] / (Hz3[1]*1000)
  
  d1[1]    = max(0, R1[1]-R1max) #verif
  ES0[1]   = EP[1]        # effet mulch
  ES1[1]   = min(ES0[1] , RU1[1])
  R2max  = param$fc * 1000 * Hz2[1]
  RU2max = RU * Hz2[1]
  xsol  = min(1,RU2[1]/RU2max) # dans la suite du code (boucle) on a i-1
  rks   = param$rksol * exp(-(1-xsol)) 
  ES2[1]   = min(RU2[1] , (ES0[1]-ES1[1])*rks) # Dans le code (boucle) on a R2[i-1] 
  ETR[1] = TP1[1] + ES1[1] + TP2[1] + ES2[1]
  ETM[1] = ES0[1] + TPM[1] ## ajouté le 06/06/2019
  
  d2[1]    = max(0, R2[1] - R2max) 
  RU3max = RU * Hz3[1] 
  R3max  = param$fc * 1000 * Hz3[1]
  d3[1]    = max(0, R3[1] - R3max) 
  
  
  # valeurs nulles pour TPM, ETM, TP1, TP2_1, TP2,
  # pour TT, TT_p, TL, TL_p, TS, TS_p, LAI, LAI_p, LAImax (vecteurs nuls)
  # pour Ir, TDM, Cp, Kc, imoins, fac, rap,
  # pour crac, dcrac, crat, dcrat, dHz2, ks
  # Hz2=0.2 , Hz3 = param$profx-Hz2-Hz1 , Sw=NA
  
  
  #################################################################################
  ####################### boucle sur tout le cycle ################################
  #################################################################################
  
  for (i in 2:length(meteo$Date))
  # for (i in 2:365)
  {
    
    #######################################
    ## 2 - Developpement Plante
    #######################################
    
    #### après semis et avant récolte
    if ((i >= param$jsem) & (i <= param$jsem + param$jrecol))
    {
      ### ajouter un if(TS<tfmat)
      kt[i] = kt[i-1] + 1 #compteur jours après semis
      
      ### INDICES THERMIQUES ###
      if (kt[i] > 1) 
      {
        TT[i]   = TT[i-1] + fac[i-1] * max (T[i]-param$base , 0)
        TT_p[i] = TT_p[i-1] + max (T[i]-param$base , 0)
      }
      
      #### après levée
      if (TT[i] > param$xt0) 
      {
        
        ##########################################
        ###### Calcul du stress ##################
        
        # 1) pour le LAI _ Sw_lai (streswl[i-1])
        if (kt[i]<=jcum) 
        {
          Sw_lai[i] = 1
        } else {
          Sw_lai[i] = sum (ETR[(i-(jcum+1)):(i-1)])/sum (ETM[(i-(jcum+1)):(i-1)])
        }
        
        if((istr==0) & (Sw_lai[i]<1))
        {
          istr = 1
        }
        
        # 2) pour le TDM _ Sw_tdm (stresw[i])
        if(kt[i]<=jlc)
        {
          Sw_tdm[i] = 1
        } else {
          sumtrp = sum(TRP[(i-(jlc+1)):(i-1)])# modifié le 09/10/2018
          sumtpm = sum(TPM2[(i-(jlc+1)):(i-1)])
          if (sumtpm == 0) 
          {
            Sw_tdm[i] = 1 
          } else if (sumtpm > 0) {
            Sw_tdm[i] = min(1, sumtrp/sumtpm)
          }
        }
        
        #########################################
        
        # Indice thermique
        TS_p[i] = TT_p[i] - param$xt0	# après levée. Correspond à ts1 dans fortran
        TS[i]   = TT[i] - param$xt0	  #après levée. Correspond à ts dans fortran
        
        ### alpha - paramètre de croissance / sénescence dans la formule du LAI
        if (param$alpha1 == param$alpha2)  { alpha2 = 0.98 * param$alpha1
        } else if (param$alpha1 != param$alpha2) { alpha2 = param$alpha2 }
        alpha = param$alpha1
        if (TS_p[i] > tseuil)  { alpha = param$alpha2 }

        # TL temperature/logistique
        TL[i]   = (TS[i] / tm)^param$gama * exp((param$gama/alpha) * (1-(TS[i]/tm)^alpha))	    #correspond à rav dans fortran
        TL_p[i] = (TS_p[i] / tm)^param$gama * exp((param$gama/alpha) * (1-(TS_p[i]/tm)^alpha)) # rav0 mais voir les différents calculs
        
        ############## Calculs des LAI #####################################    
        ####################################################################

        # LAI potentiel/théorique
        LAI_p[i] = param$rlaimax * TL_p[i] #* (dens/densopt)^cd ####
        
        # avant tm
        if (TS[i] <= tm)
        {
          # LAI_1
          LAI[i] = rlaims * (TL[i] + Sw_lai[i]^param$coefas0 - 1) * (dens/densopt)^cd # modifié le 08/10/2018 (rlaims et non laimax)
          
          rlav   = LAI[i-1] ## rajouté le 07/09/2018
          
          # calcul rlmx[i] -  que l'on appelle LAImax[i]
          LAImax[i] = max(LAI[i],LAImax[i-1])
          
          # IMOINS ET FAC
          imoins[i] = imoins[i-1] 
          test1     = 1 - max(sign(LAI[i]- min(LAI[i-1], 0.75*LAImax[i])) , 0)
          imoins1   = min(imoins[i-1] + test1 , 1)
          if (imoins[i]==0 & test1==1)       ###modifié le 05/09/2018 
          { 
            kg = 0
            ifoi  = 0
            rlmin = 0.9 * LAImax[i] 
          }
          test2 = sign (Sw_lai[i]-0.8) 
          if ((imoins1==1) & (test2 == 1)) 
          {
            ifoi = 1
            LAImax[i] = 0
          }
          
          imoins[i] = imoins1 * max (-test2 , 0) 
          fac[i]    = 1 - imoins[i]*0.35 
          
          # LAI_2
          if ((imoins[i-1]==0) & (LAI[i]< LAI[i-1]) & (LAI[i]<=0.2))
          {
            LAI[i] = 0.2
          }
          
          # LAI_3
          if (imoins[i] == 1 & ifoi==0)
          {
            kg = kg + 1
            LAI[i] = rlmin * exp(-kg*(alpha2/10))
          }
          
          # LAI_4
          if (((TS_p[i]-TS_p[i-1]) != 0) & ((TS[i]-TS[i-1]) != 0))
          {
            dLAI_p = (LAI_p[i] - LAI_p[i-1]) / ( (TS_p[i] - TS_p[i-1]) / tm)  #pentx dans fortran
            dLAI   = (LAI[i] - LAI[i-1]) / ( (TS[i] - TS[i-1]) / tm)          #pents dans fortran
            if (abs(dLAI) > abs(dLAI_p))
            {
              LAI[i] = LAI[i-1] + (0.75 * dLAI_p + 0.25 * dLAI) * ((TS[i]-TS[i-1])/tm)
            }
          }

          # LAI_7
          LAI[i] = min(LAI[i] , 0.999999*LAI_p[i])
          
          # LAI_8
          if ((kjh==0) & (TT_p[i-1]>=somc-10) & (TT_p[i-1]<=somc+10))
          {
            rlx0 = 0.70 * param$rlaimax
            if (LAI[i]<=0.5*LAI_p[i])
            {
              kjh = 1
              rlaims = rlx0
            }
          }

          if ((iccc!=1) & (stab0>150) & (LAI[i]>LAI[i-1]) & (TS_p[i]>=tsen))
          {
            iccc = 1
            LAI[i] = LAI[i-1] - (LAI[i-2] - LAI[i-1])
          }
          
          # LAI_9
          if((TS_p[i]>=tm-50) & (LAI[i]<=LAI[i-1]) & (idrp != 1))
          {
            idrp  = 1
            rlmin = LAI[i-1]
          }
          
          if (idrp == 1)
          {
            LAI[i] = min (rlmin , LAI[i])
          }
          
        } # fin if avant tm

        # LAI_10 - après tm
        if (TS[i] > tm) 
        {
          LAI[i] = rlav - ((LAI_p[i-1] - LAI_p[i]) * (1/max(Sw_lai[i],0.2) )) 
        }
        
        rlav = LAI[i] ### rajouté 07/09/2018
        
        #########################
        ###### calcul TDM
        
        test1   = max(0 , -sign(TS_p[i] - (param$tfmat-param$xt0)))        
        test2_1 = min(-sign(TS_p[i] - (param$tjdc-param$xt0))+1 , 1)
        test2_2 = min(-sign(T[i] - tcrit)+1 , 1)
        test2   = test2_1 + test2_2
        cortemp = min(test1,test2)
        if (LAI[i] > 0)
        {
          extin  = min(1,(1.43/sqrt(LAI[i])))
          Ir     = 1-exp(-extin*LAI[i])
          TDM[i] = TDM[i-1] + cortemp * param$rmg * 0.0001 * Rg[i] * Ir * Sw_tdm[i]^param$cwx
        } else if (LAI[i] <= 0) {
          TDM[i] = TDM[i-1]
        }
        
        ## TDM_p (i.e. TDM potentiel)
        if(LAI_p[i] > 0)
        {
          extin_p  = min(1,(1.43/sqrt(LAI_p[i])))
          Ir_p     = 1-exp(-extin_p*LAI_p[i])
          TDM_p[i] = TDM_p[i-1] + cortemp * param$rmg * 0.0001 * Rg[i] * Ir_p
        } else if (LAI_p[i] <= 0) {
          TDM_p[i] = TDM_p[i-1]
        }
        
        ################## TDM fin ####
        
        # LAI_11
        if ((param$tseuil0<=200) & (ill==1) & (TS_p[i-1] < (param$tfmat-param$xt0)))
        {
          LAI[i] = rl0 * exp((l0-kt[i])*0.05) # rl0 étant la valeur du LAI et l0 la valeur de kt quand ill = 1 
        }
        
        if (TS_p[i]>=(param$tjdc-param$xt0) & (ipas1==0))
        {
          jdc   = i-param$jsem
          ipas1 = 1
        }
        # ill	
        if ((param$tseuil0<=200) & (LAI[i]<=0.2) & (kt[i] > (jdc+10)) & (ill==0))
        {
          ill = 1
          rl0 = LAI[i]
          l0  = kt[i]
        }
        
        # LAI_12
        if ((istr==0) & (TS[i] < (param$tjfc - param$xt0)) & (TS_p[i-1] < (param$tfmat-param$xt0)))
        {
          LAI[i] = LAI_p[i] * (dens/densopt)^cd
        }
        
        # LAI_13
        if ((LAI[i]<=0) & (TS_p[i-1] < (param$tfmat-param$xt0))) { LAI[i] = 0 }
        
        # LAI_14
        if ((TS_p[i-1] >= (param$tfmat-param$xt0)) & (param$tfmat-param$xt0 > tm+100) & (LAI[i]>LAI[i-1])) # 1er critère pour imat=1 (avec kt>=jrecol ds fortran mais me semble pas cohérent), je ne mets que le critère température
        {
          LAI[i] = 0.85 * LAI[i-1]
        }
        
        # LAI_15
        if (LAI[i]<=0) { LAI[i] = 0 }
        
        ### calcul LAI_av et LAIp_av
        if ((TT[i]>=param$tjdc) & (TT[i] <= param$tjfc))
        {
          percrit  = percrit + 1
          sumLAIav = sumLAIav + LAI[i]
          sumLAIpav = sumLAIpav + LAI_p[i]
          LAI_av[i] = sumLAIav / percrit
          LAIp_av[i] = sumLAIpav / percrit
        }
        
        #################### fin termes LAI ################################    
        ####################################################################
        
        # Cp Canope coeff
        Cp[i] = c1 - exp(-c2 * LAI[i])
        
        ### croissance racinaire dcrac[i] différentes étapes
        if (TT[i] >= param$tseuil0)
        {
          # 2 rap = etr/etm
          rap = ETR[i-1] / ETM[i-1]
          
          # 3 test sur rap
          test4 = 1 - max(sign(rap-0.95), 0)
          taum1 = taum[i-1] + test4 * racplus 
          
          # 4 test taum
          taurap = taux*vrac  ###### déplacer vers param
          
          if((rap <= 0.95) & (taum1 >= taurap)) 
          {
            taum[i] = taurap
          } else { 
            taum[i] = taum1
          }
          
          # 5 calculs crac[i], crat[i], dcrac[i],dcrat[i]
          crac[i]  = ks[i-1] * taum[i] + seuil
          dcrac[i] = crac[i] - crac[i-1]
          crat[i]  = degred * (TT_p[i] - param$tseuil0)+seuil
          dcrat[i] = crat[i] - crat[i-1]
          
          # 6 calcul dHz2[i]: croissance racinaire qui agrandit le R2
          test6   = min(sign(taum[i]-0.25), 0) + 1
          dHz2_1  = test6 * dcrac[i] - (test6 - 1) * min(dcrac[i] , dcrat[i])
          dHz2[i] = min (dHz2_1 , (param$profx-Hz1)-Hz2[i-1])
          
          # 8 pour ks[i] (paramètre qui contrôle la croissance racinaire)
          rk0    = (param$fc-param$ppf)*(1-param$rkru) # si rlaimx=0 (sol nu), rkru=0.55
          test7a = max(sign(TT_p[i-1] - param$tseuil0), 0)
          test7b = min(sign(TS_p[i]-(param$tfmat-param$xt0))+1 , 1) 
          test7  = min((test7a+test7b) , 1)
          
          test8 = min(sign(tetaRU3[i-1] - rk0), 0) +1
          ks[i] = test7 * (ks[i-1] + test8)
          
        } # fin boucle après tseuil0 pour crssce racinaire
        
        
      } # fin boucle après levée
      
      
    } # fin boucle dev plante à partir de jsem
    
    
    #######################################
    ## 1 - Bilan Hydrique
    #######################################
    
    # taille des réservoirs Hz2 et Hz3 qui découlent de la croissance racinaire
    Hz2[i] = min((param$profx-Hz1) , (Hz2[i-1] + dHz2[i])) #!!!! on met un max??? sinon dépasse prof
    
    Hz3[i] = param$profx - Hz1 - Hz2[i]
    R2max  = param$fc * Hz2[i] * 1000
    RU2max = RU * Hz2[i]
    R3max  = param$fc * Hz3[i] * 1000
    RU3max = RU * Hz3[i]
    
    ### xsol ###
    if (i < param$jsem)
    {
      xsol = min(1 , (RU2[i-1]+RU3[i-1])/(RU2max+RU3max))
    } else if (i >= param$jsem) {
      xsol = min(1 , (RU2[i-1]/ RU2max))
    }
    ### rks et Kc - propriétés coeff cultural et propriété sol à laisser passer ES2 ###
    rks = param$rksol * exp(-(1 - xsol))
    if (TT_p[i] <= param$xt0) 
    {
      Kc[i] = rks 
    } else if (TT_p[i] > param$xt0) {  
      Kc[i] = max( param$rkcm*(1 - exp(-LAI[i])) , rks )
    }
    
    ## Kc_p (correspond à rkct dans fortran) - ajouté le 12/10/2018
    if (kt[i]==0)
    {
      Kc_p[i] = param$rksol
    } else if ((kt[i] > 0) & (kt[i] < 60)) {
      Kc_p[i] = param$rkcm*(1 - exp(-LAI_p[i]))
    } else if ((kt[i] >= 60) & (TS_p[i] <= (param$tfmat-param$xt0))) {  
      Kc_p[i] = max ( param$rkcm*(1 - exp(-LAI_p[i])) , param$rksol )
    } else if (TS_p[i] > (param$tfmat-param$xt0)) {
      Kc_p[i] = param$rkcm*(1 - exp(-LAI_p[i]))
    }
    
    ratio_a[i] = min (100 , 100*(RU1[i-1]+RU2[i-1])/(RU1max+RU2max))  
    #####################
    ####### irrigation ##
    ## si itest==1 (pas de flichier irrigation)
    if ((param$itest == 1) & (i > (param$jsem+param$jdir)) & (i< (param$jsem+param$jfir)))
    {
      if ((i<=param$jsem+10) & (ifois==0) & (P[i] < param$dosem))
      {
        if ((RU1[i-1]<=0) & (RU2[i-1] <= (0.75 * RU2max)))
        {
          if (param$gge == 0)   { I1[i] = param$dosem }
          if (param$gge == 999) { I2[i] = param$dosem }
          ifois = 1
        }
      }
      
      if ((ratio_a[i] < param$th_w) & ((RU1[i-1]-Kc[i]*ET0[i])<=0))
      {
        # if (param$gge == 0)   { I1[i] = param$dosap }
        # if (param$gge == 999) { I2[i] = param$dosap }
        if (param$gge == 0)   { I1[i] =  min(param$dosap,param$quota-sum(I1)) }
        if (param$gge == 999) { I2[i] =  min(param$dosap,param$quota-sum(I1)) }
      }
    }
   
    ######## Calculs pour R1 #########
    ##################################
    R1_a  = R1[i-1] + P[i] + I1[i]
    d1[i] = max(0, R1_a - R1max)
    R1_1  = min(R1_a , R1max) 
    RU1_1 = max(0 , round(R1_1 - (param$ppf * 1000 * Hz1), 10)) ### reprendre les "round"
    
    ####### Demandes ES0, TPM et ETM ## 
    ES0[i] = (1 - Cp[i]) * EP[i] # EP contient effet mulch 
    if ((LAI[i]==0) & (RU1_1[i]<=0)) ES0[i] = EP[i]*rks[i]  # ajout d'un effet "résistance" à l'évaporation (le 06/06/2019)
    
    TPM[i] = Cp[i] * Kc[i] * ET0[i] 
    ETM[i] = TPM[i]+ES0[i]

    #######
    ES1[i] = min(ES0[i] , R1_1 - (tetaR*1000*Hz1)) # peut prendre eau liée justque tetaR dans R1
    
    TP1[i] = min(TPM[i], max(RU1_1-ES1[i] , 0))
    
    if (RU1_1 - ES0[i] - TPM[i] >= 0)
    {
      ETM[i] = min (ET0[i], ETM[i])
      TP1[i] = max(ETM[i]-ES1[i] , 0)
    }# pas de modif d'ETM TP1+ES1=ETR=ETM > ET0 si LAI>1.8 et Kcmax=1.2
    # répartition à faire entre TP1 et TP2? eu prorata des tailles Hz1 et Hz2
    
    R1[i]  = R1_1 - ES1[i] -TP1[i]
    RU1[i] = max(0 , round(R1[i] - param$ppf * 1000 * Hz1, 10))
    teta1[i] = R1[i] / (Hz1*1000)
    
    ######## Calculs pour R2 #########
    ##################################
    
    R2_a  = R2[i-1] + d1[i] + I2[i] + (dHz2[i] * teta3[i-1] * 1000)
    d2[i] = max(0, round(R2_a - R2max , 10))
    R2_1  = min(R2_a , R2max) 
    RU2_1 = max(0 , R2_1 - param$ppf * 1000 * Hz2[i])
    
    ## si sol nu (LAI nul) avant xt0
    # testa = min(-sign(LAI[i]) + 1 , 1)
    # testb = max(0 , -sign(TT_p[i]-param$xt0)) 
    # ES2[i] = min(1,(testa+testb)) * min(RU2[i-1] , (ES0[i]-ES1[i])* rks[i]) 
    if (LAI[i] == 0) {
      ES2[i] = min(RU2[i-1] , (ES0[i]-ES1[i])* rks[i]) # rks apporte résistance de Hz1 pour évap dans R2
      if (RU1_1[i]<=0) ES2[i] = min(RU2_1[i] , ES0[i] - ES1[i]) # cas dans lequel sol surface très sec
    }
    
    ## calculs TP2
    TP2_1 = min(RU2_1 , TPM[i] - TP1[i]) 
    
    ## cas particuliers pour TP2 sans eau dans RU1 et avec plante : toute etm dirigée pour transpiration
    if (RU1_1 - ES0[i] - TPM[i] >= 0) ###??? pas utile???
    {
      TP2_1 = 0
    }
    
    if ((RU1_1 <= 0) & (TT_p[i]> 0) & (LAI[i]>0))
    {
      TP2_1 = ET0[i] * Kc[i]
    }
    
    if (RU1_1 > 0 & (RU1_1-ES0[i])<0)
    {
      TP2_1 = max(0,(ET0[i]-ES1[i]) * Kc[i])
      if (TT_p[i]>=param$xt0 & LAI[i]==0)
      {
        ES2[i] = max(0, (EP[i]-ES1[i])*rks)
      }
    }
    
    rs  = (1 - param$rkru) * RU2max		# réserve difficile à utiliser pour plante - reserve de survie
    
    TP2[i] = TP2_1
    if ((TT_p[i] >= param$xt0) & (RU2_1 < rs)) 
    {    
      TP2[i] = TP2_1 * RU2_1 / rs	# réduction de la transpiration dans R2 car réserve en dessous de rs
    } #else if ((TT_p[i] < param$xt0) & (TT_p[i] >= param$xt0/3)) 
    # {
    #   rs = rs *TT_p[i] / param$xt0
    #   if (RU2_1 >= rs)
    #   {
    #     TP2[i] = TP2_1
    #   }
    # }
    
    R2[i]  = R2_1 - ES2[i] - TP2[i]
    RU2[i] = max(0 , R2[i] - param$ppf * 1000 * Hz2[i])
    teta2[i] = R2[i] / (Hz2[i]*1000)
    
    ### ETR ###
    ETR[i] = TP1[i] + ES1[i] + TP2[i] + ES2[i]
    if (ETR[i] > ETM[i])
    {
      ETR[i] = ETM[i]
    }
    if (Cp[i] == 0) 
    {
      ETR[i] = ES1[i] + ES2[i]  
      if (ETR[i] >= ET0[i]) { ETR[i] = ET0[i] }
      if (ETM[i] >= ET0[i]) { ETM[i] = ET0[i] }
    }
    
    ETM[i] = max (Kc[i]*ET0[i] , ETR[i])
    
    ### TRP et TMP2 - utilse pour Sw_tdm
    TRP[i] = TP1[i] + TP2[i]
    if (TRP[i] > TPM[i]) 
    {
      TRP[i] = TPM[i]
    }
    etmx = max(Kc[i],Kc_p[i]) * ET0[i]
    if (TPM[i] == 0) 
    { 
      TPM2[i] = etmx 
    } else if (TPM[i] > 0)
    {
      TPM2[i] = TPM[i]
    }
    
    # Calculs pour R3 #
    if (Hz3[i] > 0) # pour éviter d'avoir division par zero
    {
      R3_1[i]  = R3[i-1] + d2[i] - (dHz2[i] * teta3[i-1] * 1000) 
      d3[i]    = max(0, round(R3_1[i] - R3max,10))
      R3[i]    = R3_1[i] - d3[i]
      RU3[i]   = R3[i] - param$ppf * 1000 * Hz3[i] 
      teta3[i] = R3[i] / (Hz3[i]*1000)    
      tetaRU3[i] = RU3[i] / (Hz3[i]*1000) ## ajouté
    } else if (Hz3[i] ==0) {
      d3[i] = d2[i]
    }
    
    # mise à jour de R1, R2, R3 si jdsim>1 et qu'on est à jdsim
    if ((param$jdsim>1) & (meteo$doy[i] == param$jdsim))
    {
      if (R1[i] < 0.5) #
      {
        R2[i]  = param$res / (Hz2[i]+Hz3[i]) * Hz2[i]
        RU2[i] = (param$res-param$ppf*1000*param$profx) / (Hz2[i]+Hz3[i]) * Hz2[i]
        R3[i]  = param$res / (Hz2[i]+Hz3[i]) * Hz3[i]
        RU3[i] = (param$res-param$ppf*1000*param$profx) / (Hz2[i]+Hz3[i]) * Hz3[i]
      } 
      else if (R1[i] >= 0.5)#
      {
        rsu_1 = (param$res-param$ppf*1000*param$profx) / param$profx * Hz1
        if (rsu_1 >= RU1[i]) # cas "impossible" car on est parti avec réserve pleine, on conserve valeur R1 estimée
        {
          res    = param$res-R1[i] # ce qui reste à redistribuer
          R2[i]  = max(res*Hz2[i]/(Hz2[i]+Hz3[i]) , R2max)
          RU2[i] = R2[i] - param$ppf * 1000 * Hz2[i]
          R3[i]  = max(res*Hz3[i]/(Hz2[i]+Hz3[i]) , R3max)
          RU3[i] = R3[i] - param$ppf * 1000 * Hz3[i]
        }
        else if (rsu_1 < RU1[i])
        {
          R1[i]  = param$res / param$profx * Hz1
          RU1[i] = (param$res-param$ppf*1000*param$profx) / param$profx * Hz1
          R2[i]  = param$res / param$profx * Hz2[i]
          RU2[i] = (param$res-param$ppf*1000*param$profx) / param$profx * Hz2[i]
          R3[i]  = param$res / param$profx * Hz3[i]
          RU3[i] = (param$res-param$ppf*1000*param$profx) / param$profx * Hz3[i]
        }		
      }
    }
    
    teta2[i] = R2[i] / (Hz2[i]*1000) 
    if (Hz3[i] > 0)
    {                        
      teta3[i] = R3[i] / (Hz3[i]*1000) 
    } else if (Hz2[i]==0) {
      teta3[i] = 0
    }
    
    
    
    
  }  ### fin boucle i=2:fin
  
  
  # #######################################
  # ## 3 - Termes de Rendement
  # #######################################
  # 
  # indice de récolte HI

  LAIav_crit = LAI_av[which(is.na(LAI_av[])==F)] #pour récupérer la dernière valeur
  LAIav  = LAIav_crit[length(LAIav_crit)]
  LAIav_crit_p = LAIp_av[which(is.na(LAIp_av[])==F)] #pour récupérer la dernière valeur
  LAIav_p  = LAIav_crit_p[length(LAIav_crit_p)]
  
  hi_p    = param$hi * (densopt/dens)^0.20 
  HI_1    = hi_p - param$xl1 * (param$rlcrit - LAIav) 
  HI_1_p  = hi_p - param$xl1 * (param$rlcrit - LAIav_p) 
  HI      = min(hi_p , HI_1)
  HI_p    = min(hi_p , HI_1_p)
  
  # rendement grain Y[i] (rendement sec, rend0) et Yhu[i] (rendement humide, rend)
  Y   = HI * max(TDM[])               # correspond à rend0
  Yhu = Y * (100 + param$pourc) / 100 # correspond à rend
  Y_p   = HI_p * max(TDM_p[])
  Yhu_p = Y_p * (100 + param$pourc) / 100
  
  # attentes financières
  sumI      = sum(I1[],I2[])   #somme irrigations
  IrrCum = cumsum(sumI)
  
  Financial = Yhu * param$f3 - 0.01 * sumI * param$f2 - 1 * param$f1
  
  
  ###################################################
  ################## fichiers de sortie #############
  # 1- fichier meteo
  # write.csv(meteo,paste("./meteo_file_",as.character(j),".csv",sep=""),quote=FALSE,row.names=FALSE)
  # # 2- fichier paramètres
  # write.csv(param,paste("./param_file_",as.character(j),".csv",sep=""),quote=FALSE,row.names=FALSE)
  # 
  # # 3- ficher recap
  # i=c(1:i) # valeurs de i
  # IrrCum = cumsum(I1)
  # outvar = cbind(i,meteo$doy,meteo$Date,I1,IrrCum,ratio_a,RU1,RU2,ETM,TPM,ETR,d1,d2,d3,TT_p,LAI_p,LAI,TDM)
  # outvar=as.data.frame(outvar)
  # colnames(outvar)=c("i","doy","Date","Irr","IrrCum","ratio_a","RU1","RU2","ETM","TPM","ETR","d1","d2","d3","TT_p","LAI_p","LAI","TDM")
  # write.csv(outvar,paste("./outvar_file_",as.character(j),".csv",sep=""),quote=FALSE,row.names=FALSE)

  ##### on retourne fichier outvar
  # i=c(1:i) # valeurs de i
  
  # outvar = list(Irr=sumI,IrrCum=IrrCum,ratio_a=ratio_a,
  #               RU1=RU1,RU2=RU2,ETM=ETM,TPM=TPM,ETR=ETR,d1=d1,d2=d2,d3=d3,TT_p=TT_p,
  #               LAI_p=LAI_p,LAI=LAI,TDM=TDM,TDM_p=TDM_p,Y=Y,Yhu=Yhu,Y_p=Y_p,Yhu_p=Yhu_p,
  #               sumI=sumI,Financial=Financial)
  
  ############################################################################
  
  # outvar = list(Irr=I1+I2,IrrCum=IrrCum,ratio_a=ratio_a,TT=TT,TT_p=TT_p,
  #               LAI_p=LAI_p,LAI=LAI,LAI_av=LAI_av,LAIp_av=LAIp_av,
  #               TDM=TDM,TDM_p=TDM_p,
  #               sumI=sumI,Yhu=Yhu,Yhu_p=Yhu_p,Financial=Financial)
  
  ## pour test: round sorties pour alléger:
  outvar = list(Irr=round(I1+I2,3),IrrCum=round(IrrCum,3),ratio_a=round(ratio_a,3),TT=TT,TT_p=TT_p,
                LAI_p=round(LAI_p,3),LAI=round(LAI,3),LAI_av=round(LAI_av,3),LAIp_av=round(LAIp_av,3),
                TDM=round(TDM,3),TDM_p=round(TDM_p,3),
                RU1=round(RU1,3),RU2=round(RU2,3),ETM=round(ETM,3),
                sumI=sumI,Yhu=Yhu,Y=Y,Financial=Financial,
                Hz1=Hz1,Hz2=Hz2,Hz3=Hz3)#,Yhu_p=Yhu_p)
  
  return(outvar)



#############################################################################################
#############################################################################################
#############################################################################################

# pdfpath <- paste("./",case,"_v1_6.pdf",sep="")
# pdf(file = pdfpath, width=21/2.54, height=29.7/2.54)
# 
# 
# Splits the first page in 1 column and 4 lines with additional controls
# par(mfrow=c(3,1),bty="l",cex.axis=1.2,cex.lab=1.5,cex.main=1.8,lab=c(10,6,5),mar=c(3,3.5,3,4))
# 
# ## Radiation
# par(col.main="black")
# main_rad <- expression(paste("Radiation: ",R[g]," (J ",cm^-2,")"))
# plot(as.Date(meteo$Date),meteo$Rg,adj=0.02,main=main_rad,type="l",xlab="",ylab="",col="indianred",axes=FALSE)
# axis(2)
# axis.Date(1,at=seq(min(as.Date(meteo$Date)),max(as.Date(meteo$Date))+1,by="1 mon"),format="%m")
# par(new=FALSE)
# 
# ## Temperature and degrees.days
# par(col.main="black")
# main_T<-expression(paste("Temperature: T (°C)"))
# plot(as.Date(meteo$Date),meteo$T,adj=0.02,main=main_T,type="l",axes=FALSE,xaxt="n",xlab="",ylab="",ylim=c(0,max(meteo$T)), col="orange")
# axis(2)
# axis.Date(1,at=seq(min(as.Date(meteo$Date)),max(as.Date(meteo$Date))+1,by="1 mon"),format="%m")
# par(new=TRUE)
# # T sum
# par(col.main="black")
# main_SigmaT<-expression(paste("Degrees.days: ",Sigma,"T (°C)"))
# sigmaT=TT_p
# sigmaT[which(sigmaT==0)]=NA
# plot(as.Date(meteo$Date),sigmaT,adj=0.98,main=main_SigmaT,type="l",lwd=1.5,axes=FALSE,xlab="",ylab="",xlim=c(min(as.Date(meteo$Date)),max(as.Date(meteo$Date))),col="red")
# axis(4,col="red")
# par(new=FALSE)
# 
# ## Rain - ETP
# par(col.main="black")
# main_RmETP<-expression(paste("Effective rain: R-ETP (mm)"))
# plot(as.Date(meteo$Date),meteo$P-meteo$etp,adj=0.5, main=main_RmETP,type="l",axes=FALSE,xaxt="n",xlab="",ylab="",ylim=c(-10,max(meteo$P-meteo$etp)),col="blue")
# abline(h=0,lty=3,col="red")
# axis(2)
# axis.Date(1,at=seq(min(as.Date(meteo$Date)),max(as.Date(meteo$Date))+1,by="1 mon"),format="%m")
# par(new=FALSE)
# 
# ## Irrigations
# par(col.main="black")
# main_Ibar<-expression(paste("Daily irrigation amounts: ",bar(I)," (mm)"))
# plot(as.Date(meteo$Date),I1,adj=0.02, main=main_Ibar,type="h",axes=FALSE,xaxt="n",xlab="",ylab="",col="blue")
# axis(2)
# main_SigmaIbar<-expression(paste(Sigma,bar(I)," (mm)"))
# axis.Date(1,at=seq(min(as.Date(meteo$Date)),max(as.Date(meteo$Date))+1,by="1 mon"),format="%m")
# par(new=TRUE)
# par(col.main="black")
# plot(as.Date(meteo$Date),cumsum(I1),adj=0.98,main=main_SigmaIbar,axes=FALSE,type="s",lwd=2,xlab="",ylab="",xlim=c(min(as.Date(meteo$Date)),max(as.Date(meteo$Date))+1),col="magenta")
# axis(4,col="magenta")
# par(new=FALSE)
# 
# ### res. racinaire - ratio_a
# par(col.main="black")
# main_ratio<-expression(paste("Root zone reserve (%)"))
# plot(as.Date(meteo$Date),ratio_a,type="l",axes=FALSE,xaxt="n",xlab="",ylab="",ylim=c(0,100),col="dark blue")
# abline(h=(100-100*param$rkru),lty=3,col="purple")
# par(new=TRUE)      # RFU is not a vector
# # observation data for RATIO?
# title(main_ratio,adj=0.02)
# axis(2)
# axis.Date(1,at=seq(min(as.Date(meteo$Date)),max(as.Date(meteo$Date))+1,by="1 mon"),format="%m")
# text(min(as.Date(meteo$Date)),(100-100*param$rkru),"RAW",cex=0.8,col="purple",font=2)
# par(new=TRUE)
# main_Irr2<-expression(paste("Irrigations I: (mm)"))
# plot(as.Date(meteo$Date),I1,type="h",axes=FALSE,xlab="",ylab="",ylim=c(0,200),col="magenta")
# par(new=TRUE)
# title(main_Irr2,adj=0.98)
# axis(4,col="magenta")
# par(new=FALSE)
# 
# ## LAI
# par(col.main="black")
# main_lai<-expression(paste("LAI (-)"))
# plot(as.Date(meteo$Date),LAI_p,main=main_lai,adj=0.02,type="l",lty=2,lwd=1.5,axes=FALSE,xaxt="n",xlab="",ylab="",ylim=c(0,max(LAI_p)),col="red")
# points(as.Date(meteo$Date),LAI,type="l",lwd=1.5,col="blue")
# axis(2)
# axis.Date(1,at=seq(min(as.Date(meteo$Date)),max(as.Date(meteo$Date))+1,by="1 mon"),format="%m")
# legend("left", lty=c(2,1), col=c("red", "blue"),
#        #pt.bg  = c(NA, "red", NA, NA, NA),
#        #pch    = c(NA, pmax, pobs, pmin, NA),
#        legend = c("LAI_p", "LAI"))
# par(new=FALSE)
# 
# 
# dev.off()

  
  
  
}
