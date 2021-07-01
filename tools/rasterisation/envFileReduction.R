# A la mano modification of cormas input data from  Bruno
# To be added to genric env creation from Bastien
# Does two things: 1. remove unused cells 2. add cells altitudes.

library(dplyr)
library(tidyr)
#.........................................................................
library(rgdal)         # lien vers la librairie GDAL
#.........................................................................
library(raster)        # analyses sur données Raster
library(gdistance)     # chemins de moindre coût sur Raster
library(rasterVis)     # visualisation de données Raster
#.........................................................................
library(sp)            # analyses sur données vecteur
library(spdep)         # géostatistiques sur données vecteur
library(rgeos)         # opérateurs sur données vecteur
library(spatstat)      # analyses de données vecteur
library(igraph)        # analyses de réseaux
library(maptools)      # lecture/ecriture de données vecteur
library(fields)        # kriegage et manipulation de données vecteur

#getting only data table from .env
envTable <- read.table("tools/rasterisation/spatial_ent[2017]_div2grandbuechcanaux318x238_numvalues.env", 
                       skip =6,
                       sep= ",",
                       dec = ".") %>%
  tbl_df()

colnames(envTable)<-c("idParcel","surfParc","codeCultu","codeGroup","codeAsa","idExpl","idReach","wintakeId","wreleaseId","wderivId","filteringSoil","canalsId")
## Getting altitude
r<-raster('tools/rasterisation/MNT25m_93_buech_emprise.tif', sep="")

#r<-raster('tools/rasterisation/step2_streams_new_cas10.tif', sep="")
etendue=readShapePoly('tools/rasterisation/emprise_upper_buech.shp')
proj=CRS("+init=epsg:2154")    
projection(etendue)<-proj 
etendueBox<-extent(etendue) # calcul de l'étendue rectangulaire du fichier  buffer
r <- crop(r,etendueBox)        # découpage du MNT pour qu'il corresponde à l'étendue
fdiv=4 # !!! division resolution par ...
rdiv<-aggregate(r, fact=fdiv)#; str(rdiv) 

#rpg=readShapePoly('tools/rasterisation/upper_buech_hru-plots.shp');
#rpgRast_ID_PARCEL=rasterize(rpg, rdiv, field="ID_PARCEL", progress="text");


#Adding altitude to data table
#envTable$V14 <- rdiv %>% as.data.frame() %>% pull() #No altitude..

#Reomving unused cells and adding id column
txt <- envTable %>% 
  #mutate(isNull = (idExpl < 0) & (V7 <= 0) & (V8 < 0) & (V9 < 0) & (V10 < 0) & (V12 < 0)) %>% #keeping only real farmPlot (parcells)
  mutate(isNull = ((codeAsa < 0)| (codeAsa == 11) | (codeAsa == 8) | (codeAsa == 10) | (codeAsa == 9) | (codeAsa == 1)) &
           (idReach == 0) & 
           (canalsId < 0) & 
           (wintakeId < 0) & 
           (wreleaseId < 0) &
         (wderivId < 0)
         ) %>% #keeping only reaches, canals, and asa 7)
  mutate(id = row_number()) %>%
  filter(!isNull) %>% 
  dplyr::select(-isNull) 

#Write header (adding id and altitude)
fileConn<-file("tools/rasterisation/spatial_ent[2017]_div2grandbuechcanaux318x238_numvaluesLessCellsMiniAllAsas.env")
header <- writeLines(c(
  "dimensions\t318 238",
  "cloture\tclosed" ,
  "connexite\teight" ,
  "backgrounColor 0.666707 0.666707 0.666707",
  "full\tfalse",
  "attributs\tid(Number) idParcel(Number) surfParc(Number) codeCultu(Number) codeGroup(Number) codeAsa(Number) idExpl(Number) idReach(Number) wintakeId(Number) wreleaseId(Number) wderivId(Number) filteringSoil(Number) canalsId(Number)"
), con= fileConn)
close(fileConn)

write.table(txt[,c(13,1:12)], "tools/rasterisation/spatial_ent[2017]_div2grandbuechcanaux318x238_numvaluesLessCellsMiniAllAsas.env",
            sep=",",
            dec=".",
            row.names = F,
            append = T,
            col.names = F)

