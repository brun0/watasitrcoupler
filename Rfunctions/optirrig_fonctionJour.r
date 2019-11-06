 #################################################################################
 ## fonction journalière 

 # INPUT FONCTION param, meteo

daily_optirr = function(param,meteo,const,inpval,vecteurs,I1,I2,i)
{
	attach(const)
	attach(inpval)
	attach(vecteurs)
    
	T  = meteo$T
	P  = meteo$P
	Rg = meteo$Rg
	
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
          Sw_tdm = 1
        } else {
          sumtrp = sum(TRP[(i-(jlc+1)):(i-1)])# modifié le 09/10/2018
          sumtpm = sum(TPM2[(i-(jlc+1)):(i-1)])
          if (sumtpm == 0) 
          {
            Sw_tdm = 1 
          } else if (sumtpm > 0) {
            Sw_tdm = min(1, sumtrp/sumtpm)
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
        TL   = (TS[i] / tm)^param$gama * exp((param$gama/alpha) * (1-(TS[i]/tm)^alpha))	    #correspond à rav dans fortran
        TL_p = (TS_p[i] / tm)^param$gama * exp((param$gama/alpha) * (1-(TS_p[i]/tm)^alpha)) # rav0 mais voir les différents calculs
        
        ############## Calculs des LAI #####################################    
        ####################################################################

        # LAI potentiel/théorique
        LAI_p[i] = param$rlaimax * TL_p #* (dens/densopt)^cd ####
        
        # avant tm
        if (TS[i] <= tm)
        {
          # LAI_1
          LAI[i] = rlaims * (TL + Sw_lai[i]^param$coefas0 - 1) * (dens/densopt)^cd # modifié le 08/10/2018 (rlaims et non laimax)
          
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
          TDM[i] = TDM[i-1] + cortemp * param$rmg * 0.0001 * Rg[i] * Ir * Sw_tdm^param$cwx
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
        Cp = c1 - exp(-c2 * LAI[i])
        
        ### croissance racinaire dcrac différentes étapes
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
          
          # 5 calculs crac[i], crat[i], dcrac,dcrat
          crac[i]  = ks[i-1] * taum[i] + seuil
          dcrac = crac[i] - crac[i-1]
          crat[i]  = degred * (TT_p[i] - param$tseuil0)+seuil
          dcrat = crat[i] - crat[i-1]
          
          # 6 calcul dHz2[i]: croissance racinaire qui agrandit le R2
          test6   = min(sign(taum[i]-0.25), 0) + 1
          dHz2_1  = test6 * dcrac - (test6 - 1) * min(dcrac , dcrat)
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
    
    Hz3 = param$profx - Hz1 - Hz2[i]
    R2max  = param$fc * Hz2[i] * 1000
    RU2max = RU * Hz2[i]
    R3max  = param$fc * Hz3 * 1000
    RU3max = RU * Hz3
    
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
      Kc = rks 
    } else if (TT_p[i] > param$xt0) {  
      Kc = max( param$rkcm*(1 - exp(-LAI[i])) , rks )
    }
    
    ## Kc_p (correspond à rkct dans fortran) - ajouté le 12/10/2018
    if (kt[i]==0)
    {
      Kc_p = param$rksol
    } else if ((kt[i] > 0) & (kt[i] < 60)) {
      Kc_p = param$rkcm*(1 - exp(-LAI_p[i]))
    } else if ((kt[i] >= 60) & (TS_p[i] <= (param$tfmat-param$xt0))) {  
      Kc_p = max ( param$rkcm*(1 - exp(-LAI_p[i])) , param$rksol )
    } else if (TS_p[i] > (param$tfmat-param$xt0)) {
      Kc_p = param$rkcm*(1 - exp(-LAI_p[i]))
    }
    
    ratio_a = min (100 , 100*(RU1[i-1]+RU2[i-1])/(RU1max+RU2max))  
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
      
      if ((ratio_a < param$th_w) & ((RU1[i-1]-Kc*ET0[i])<=0))
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
    d1 = max(0, R1_a - R1max)
    R1_1  = min(R1_a , R1max) 
    RU1_1 = max(0 , round(R1_1 - (param$ppf * 1000 * Hz1), 10)) ### reprendre les "round"
    cat(R1max, "\n")
    cat(R1[i-1], "\n")
    cat(P[i] , "\n")
    cat(I1[i], "\n")
    
    ####### Demandes ES0, TPM et ETM ## 
    ES0 = (1 - Cp) * EP[i] # EP contient effet mulch 
    if ((LAI[i]==0) & (RU1_1<=0)) ES0 = EP[i]*rks  # ajout d'un effet "résistance" à l'évaporation (le 06/06/2019)
    
    TPM = Cp * Kc * ET0[i] 
    ETM[i] = TPM + ES0

    #######
    ES1 = min(ES0 , R1_1 - (tetaR*1000*Hz1)) # peut prendre eau liée justque tetaR dans R1
    
    TP1 = min(TPM, max(RU1_1-ES1 , 0))
    
    if (RU1_1 - ES0 - TPM >= 0)
    {
      ETM[i] = min (ET0[i], ETM[i])
      TP1 = max(ETM[i]-ES1 , 0)
    }# pas de modif d'ETM TP1+ES1=ETR=ETM > ET0 si LAI>1.8 et Kcmax=1.2
    # répartition à faire entre TP1 et TP2? eu prorata des tailles Hz1 et Hz2
    
    R1[i]  = R1_1 - ES1 -TP1
    RU1[i] = max(0 , round(R1[i] - param$ppf * 1000 * Hz1, 10))
    teta1 = R1[i] / (Hz1*1000)
    
    ######## Calculs pour R2 #########
    ##################################
    
    R2_a  = R2[i-1] + d1 + I2[i] + (dHz2[i] * teta3[i-1] * 1000)
    d2 = max(0, round(R2_a - R2max , 10))
    R2_1  = min(R2_a , R2max) 
    RU2_1 = max(0 , R2_1 - param$ppf * 1000 * Hz2[i])
    
    ## si sol nu (LAI nul) avant xt0
    # testa = min(-sign(LAI[i]) + 1 , 1)
    # testb = max(0 , -sign(TT_p[i]-param$xt0)) 
    # ES2[i] = min(1,(testa+testb)) * min(RU2[i-1] , (ES0[i]-ES1[i])* rks[i]) 
	ES2=0 ### rajouté pour version journalière
    if (LAI[i] == 0) {
      ES2 = min(RU2[i-1] , (ES0-ES1)* rks) # rks apporte résistance de Hz1 pour évap dans R2
      if (RU1_1<=0) ES2 = min(RU2_1 , ES0 - ES1) # cas dans lequel sol surface très sec
    }
    
    ## calculs TP2
    TP2_1 = min(RU2_1 , TPM - TP1) 
    
    ## cas particuliers pour TP2 sans eau dans RU1 et avec plante : toute etm dirigée pour transpiration
    if (RU1_1 - ES0 - TPM >= 0) ###??? pas utile???
    {
      TP2_1 = 0
    }
    
    if ((RU1_1 <= 0) & (TT_p[i]> 0) & (LAI[i]>0))
    {
      TP2_1 = ET0[i] * Kc
    }
    
    if (RU1_1 > 0 & (RU1_1-ES0)<0)
    {
      TP2_1 = max(0,(ET0[i]-ES1) * Kc)
      if (TT_p[i]>=param$xt0 & LAI[i]==0)
      {
        ES2 = max(0, (EP[i]-ES1)*rks)
      }
    }
    
    rs  = (1 - param$rkru) * RU2max		# réserve difficile à utiliser pour plante - reserve de survie
    
    TP2 = TP2_1
    if ((TT_p[i] >= param$xt0) & (RU2_1 < rs)) 
    {    
      TP2 = TP2_1 * RU2_1 / rs	# réduction de la transpiration dans R2 car réserve en dessous de rs
    } #else if ((TT_p[i] < param$xt0) & (TT_p[i] >= param$xt0/3)) 
    # {
    #   rs = rs *TT_p[i] / param$xt0
    #   if (RU2_1 >= rs)
    #   {
    #     TP2[i] = TP2_1
    #   }
    # }
    
    R2[i]  = R2_1 - ES2 - TP2
    RU2[i] = max(0 , R2[i] - param$ppf * 1000 * Hz2[i])
    teta2 = R2[i] / (Hz2[i]*1000)
    
    ### ETR ###
    ETR[i] = TP1 + ES1 + TP2 + ES2
    if (ETR[i] > ETM[i])
    {
      ETR[i] = ETM[i]
    }
    if (Cp == 0) 
    {
      ETR[i] = ES1 + ES2  
      if (ETR[i] >= ET0[i]) { ETR[i] = ET0[i] }
      if (ETM[i] >= ET0[i]) { ETM[i] = ET0[i] }
    }
    
    ETM[i] = max (Kc*ET0[i] , ETR[i])
    
    ### TRP et TMP2 - utilse pour Sw_tdm
    TRP[i] = TP1 + TP2
    if (TRP[i] > TPM) 
    {
      TRP[i] = TPM
    }
    etmx = max(Kc,Kc_p) * ET0[i]
    if (TPM == 0) 
    { 
      TPM2[i] = etmx 
    } else if (TPM > 0)
    {
      TPM2[i] = TPM
    }
    
    # Calculs pour R3 #
    if (Hz3 > 0) # pour éviter d'avoir division par zero
    {
      R3_1[i]  = R3[i-1] + d2 - (dHz2[i] * teta3[i-1] * 1000) 
      d3[i]    = max(0, round(R3_1[i] - R3max,10))
      R3[i]    = R3_1[i] - d3[i]
      RU3[i]   = R3[i] - param$ppf * 1000 * Hz3 
      teta3[i] = R3[i] / (Hz3*1000)    
      tetaRU3[i] = RU3[i] / (Hz3*1000) ## ajouté
    } else if (Hz3 ==0) {
      d3[i] = d2
    }
    
    # mise à jour de R1, R2, R3 si jdsim>1 et qu'on est à jdsim
    if ((param$jdsim>1) & (meteo$doy[i] == param$jdsim))
    {
      if (R1[i] < 0.5) #
      {
        R2[i]  = param$res / (Hz2[i]+Hz3) * Hz2[i]
        RU2[i] = (param$res-param$ppf*1000*param$profx) / (Hz2[i]+Hz3) * Hz2[i]
        R3[i]  = param$res / (Hz2[i]+Hz3) * Hz3
        RU3[i] = (param$res-param$ppf*1000*param$profx) / (Hz2[i]+Hz3) * Hz3
      } 
      else if (R1[i] >= 0.5)#
      {
        rsu_1 = (param$res-param$ppf*1000*param$profx) / param$profx * Hz1
        if (rsu_1 >= RU1[i]) # cas "impossible" car on est parti avec réserve pleine, on conserve valeur R1 estimée
        {
          res    = param$res-R1[i] # ce qui reste à redistribuer
          R2[i]  = max(res*Hz2[i]/(Hz2[i]+Hz3) , R2max)
          RU2[i] = R2[i] - param$ppf * 1000 * Hz2[i]
          R3[i]  = max(res*Hz3/(Hz2[i]+Hz3) , R3max)
          RU3[i] = R3[i] - param$ppf * 1000 * Hz3
        }
        else if (rsu_1 < RU1[i])
        {
          R1[i]  = param$res / param$profx * Hz1
          RU1[i] = (param$res-param$ppf*1000*param$profx) / param$profx * Hz1
          R2[i]  = param$res / param$profx * Hz2[i]
          RU2[i] = (param$res-param$ppf*1000*param$profx) / param$profx * Hz2[i]
          R3[i]  = param$res / param$profx * Hz3
          RU3[i] = (param$res-param$ppf*1000*param$profx) / param$profx * Hz3
        }		
      }
    }
    
    teta2 = R2[i] / (Hz2[i]*1000) 
    if (Hz3 > 0)
    {                        
      teta3[i] = R3[i] / (Hz3*1000) 
    } else if (Hz2[i]==0) {
      teta3[i] = 0
    }
    
 #######################################################################################################
  #####################################
  # on génère listes qui seront transmises pour la suite du code
  
  cstes = list (Hz1=Hz1,RU=RU,RU1max=RU1max,R1max=R1max,tetaR=tetaR,c1=c1,c2=c2,tcrit=tcrit,jlc=jlc,jcum=jcum,tm=tm,somc=somc,stab0=stab0,tseuil=tseuil,tsen=tsen,
				dens=dens,densopt=densopt,cd=cd,racplus=racplus,vrac=vrac,seuil=seuil,taux=taux,degred=degred,jdc=jdc)

  inval = list(kjh=kjh,rlaims=rlaims,kg=kg,istr=istr,iccc=iccc,ill=ill,ipas1=ipas1,idrp=idrp,ifoi=ifoi,ifois=ifois,percrit=percrit,sumLAIav=sumLAIav,sumLAIpav=sumLAIpav,
				rlmin=rlmin,rlav=rlav)
  
  vect  = list(ET0=ET0,EP=EP,R1=R1,R2=R2,R3_1=R3_1,d3=d3,R3=R3,teta3=teta3,tetaRU3=tetaRU3,RU1=RU1,RU2=RU2,RU3=RU3,Sw_lai=Sw_lai,Cp=Cp,
				TPM=TPM,TPM2=TPM2,ETM=ETM,ETR=ETR,TRP=TRP,TS=TS,TS_p=TS_p,TT=TT,TT_p=TT_p,LAI_p=LAI_p,LAI=LAI,LAImax=LAImax,LAI_av=LAI_av,LAIp_av=LAIp_av,
				TDM=TDM,TDM_p=TDM_p,imoins=imoins,fac=fac,crac=crac,crat=crat,dHz2=dHz2,ks=ks,Hz2=Hz2,taum=taum,kt=kt)



	detach(const)
  detach(inpval)
  detach(vecteurs)   
  # list(cstes=cstes , inval=inval , vect=vect)
  return(list(cstes=cstes ,inval=inval , vect=vect))

  
   
 
  
 

}