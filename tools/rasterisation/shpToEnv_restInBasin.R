dir <-'/home/bastien/Documents/2017-2020_These_GEAU/SIG/work_Buech/cormas/rasterisation/'
input <- paste(dir,"INPUT/Test/", sep="")
output <-paste(dir,"OUTPUT/", sep="")
setwd(dir)

.........................................................................
library(rgdal)         # lien vers la librairie GDAL
.........................................................................
library(raster)        # analyses sur données Raster
library(gdistance)     # chemins de moindre coût sur Raster
library(rasterVis)     # visualisation de données Raster
.........................................................................
library(sp)            # analyses sur données vecteur
library(spdep)         # géostatistiques sur données vecteur
library(rgeos)         # opérateurs sur données vecteur
library(spatstat)      # analyses de données vecteur
library(igraph)        # analyses de réseaux
library(maptools)      # lecture/ecriture de données vecteur
library(fields)        # kriegage et manipulation de données vecteur
........................................................................
library(spgrass6)      # interfaçage avec Grass (données vecteur)
library(scales)        # interfaçage avec SAGA (données Raster)
........................................................................

##_Projection_et_étendue_du_projet
proj=CRS("+init=epsg:2154")                   # projection du projet en RGF93

##_Chargement_données_vecteurs
#rpg=readShapePoly(paste('/home/bastien/Documents/2017-2020_These_GEAU/SIG/work_Buech/cormas/rasterisation/rpg2014_fus_buech_N4_gestionnaires.shp', sep="")) 
setwd(input)

#rpg=readShapePoly('FINAL_JOINT_PCL_INT_TAMP100[RPG2015]_GEST_TAMP100_BUECH[HYDRA_DREALPACA]_EXPL_INTERSECT_PCL_INTERIEUR_TAMP100_BUECH[HYDRA_DREALPACA].shp', sep="")
# rpg=readShapePoly('prov_pcl_buech_ZOOM.shp'); class(rpg)
#rpg=readShapePoly('FINAL_ZOOM.shp'); class(rpg)
rpg=readShapePoly('rectInBasin.shp'); 
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
x11(); plot(rpg)

#wintake=readShapePoints('wintake.shp') # from BD HYDRA
wintake=readShapePoints('prise_eau_rectInBasin[HYDRA_DREALPACA].shp') # from BD HYDRA
wintake$ID<-as.numeric(as.character(wintake$ID))
projection(wintake)<-proj
head(wintake,5); dim(wintake); class(wintake)
x11(); plot(wintake)

#wrelease=readShapePoints('wrelease.shp') # from BD HYDRA
wrelease=readShapePoints('pt_rejet_rectInBasin[HYDRA_DREALPACA].shp') # from BD HYDRA
wrelease$ID<-as.numeric(as.character(wrelease$ID))
projection(wrelease)<-proj
head(wrelease,5); dim(wrelease); class(wrelease) 
x11(); plot(wrelease)

# wderiv=readShapePoints('wderiv_ZOOM.shp') 
# wderiv$id<-as.numeric(as.character(wderiv$id))
# projection(wderiv)<-proj
# head(wderiv,5); dim(wderiv); class(wderiv) 
# x11(); plot(wderiv)

# hrus=readShapePoly('hrusOK2_PCL_buech.shp') 
hrus=readShapePoly('hru_delete_duplicates[2154].shp')
hrus$cat<-as.numeric(as.character(hrus$cat)) #!!!!transformer les autres variables pour les utiliser!!!
projection(hrus)<-proj
head(hrus,5); dim(hrus); class(hrus) 
x11(); plot(hrus)

##_Chargement_données_raster_et_aggrégation
# r <- raster(ncol=44, nrow=54) #marche pas
r<-raster('step2_streams_new.tif', sep="")
str(r); dim(r); x11(); plot(r)

##_Découpage_raster_si_necessaire_selon_étendue
etendue=readShapePoly('buech_emprise_rectInBasin[2154].shp') # lecture de l'étendue de découpage
# etendue=readShapePoly('buech_emprise_zoomOK[2154].shp')
projection(etendue)<-proj 
etendueBox<-extent(etendue)                   # calcul de l'étendue rectangulaire du fichier  buffer
X11(); par(mfcol=c(1,2))
plot(r, main="Raster étendue d'origine")
plot(etendueBox, col="red", add=T, lwd=3)
r=crop(r,etendueBox)                  # découpage du MNT pour qu'il corresponde à l'étendue
plot(r, main="Raster nouvelle étendue")
plot(etendueBox, col="red", add=T, lwd=3)

##_Aggrégation_du_raster_si_nécessaire
fdiv=1 # !!! division resolution par ...
rdiv<-aggregate(r, fact=fdiv); str(rdiv) 
cat("Old dimensions are:", "\n", "nrows =", dim(r)[1], "\n", "ncols = ", dim(r)[2])
cat("New dimensions are:", "\n", "nrows =", dim(rdiv)[1], "\n", "ncols = ", dim(rdiv)[2])
X11(); par(mfcol=c(1,2)); plot(r, col=bpy.colors(), main="Resolution de base"); plot(rdiv, col=bpy.colors(), main=paste("Resolution de base divisée",fdiv,"fois", sep=" "))

##_Rasterisation_des_données_vecteurs_par_attribut
#rpgRast=rasterize(rpg, rdiv3, field="NUM_ILOT", progress="text"); dim(rpgRast)
rpgRast_ID_PARCEL=rasterize(rpg, rdiv, field="ID_PARCEL", progress="text"); dim(rpgRast_ID_PARCEL)
plot(rpgRast_ID_PARCEL, main="RPG_ID_PARCEL")

rpgRast_SURF_PARC=rasterize(rpg, rdiv, field="SURF_PARC", progress="text"); dim(rpgRast_SURF_PARC)
plot(rpgRast_SURF_PARC, main="RPG_SURF_PARC")

rpgRast_CODE_CULTU=rasterize(rpg, rdiv, field="CODE_CULTU", progress="text"); dim(rpgRast_CODE_CULTU)
plot(rpgRast_CODE_CULTU, main="RPG_CODE_CULTU")

rpgRast_CODE_GROUP=rasterize(rpg, rdiv, field="CODE_GROUP", progress="text"); dim(rpgRast_CODE_GROUP)
plot(rpgRast_CODE_GROUP, main="RPG_CODE_GROUP")

rpgRast_CODE_ASA=rasterize(rpg, rdiv, field="CODE_ASA", progress="text"); dim(rpgRast_CODE_ASA)
plot(rpgRast_CODE_ASA, main="RPG_CODE_ASA")

rpgRast_ID_EXPL=rasterize(rpg, rdiv, field="ID_EXPL", progress="text"); dim(rpgRast_ID_EXPL)
plot(rpgRast_ID_EXPL, main="RPG_ID_EXPL")

rpgRast_FILTSOIL=rasterize(rpg, rdiv, field="FILTSOIL", progress="text"); dim(rpgRast_FILTSOIL)
plot(rpgRast_FILTSOIL, main="RPG_FILTSOIL")

rpgRast_CAT_HRU=rasterize(hrus, rdiv, field="cat", progress="text"); dim(rpgRast_CAT_HRU)
plot(rpgRast_CAT_HRU, main="RPG_CAT_HRU")

wintake_ID=rasterize(wintake, rdiv, field="ID_ENT", progress="text"); dim(wintake_ID)
plot(wintake_ID, main="wintake_ID")

wrelease_ID=rasterize(wrelease, rdiv, field="ID_ENT", progress="text"); dim(wrelease_ID)
plot(wrelease_ID, main="wrelease_ID")

wderiv_ID=rasterize(wderiv, rdiv, field="id", progress="text"); dim(wderiv_ID)
plot(wderiv_ID, main="wderiv_ID")

##_Enregistrement_des_rasters_et_ascii
#writeRaster(rpgRast,filename = "rpg2014_Buech.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
#writeRaster(rpgRast,filename = "rpg2014_Buech.asc", format="ascii", overwrite=TRUE)
setwd(output)
newdir <- paste(dim(rdiv)[1],"x",dim(rdiv)[2],"_div",fdiv,"_rectInbasin_2017_NUMVALUES",sep="")
dir.create(newdir)
setwd(paste(output,newdir,"/",sep=""))

#writeRaster(rpgRast_ID_PARCEL,filename = "rpgRast_ID_PARCEL.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(rpgRast_ID_PARCEL,filename = "rpgRast_ID_PARCEL.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(rpgRast_SURF_PARC,filename = "rpgRast_SURF_PARC.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(rpgRast_SURF_PARC,filename = "rpgRast_SURF_PARC.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(rpgRast_CODE_CULTU,filename = "rpgRast_CODE_CULTU.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(rpgRast_CODE_CULTU,filename = "rpgRast_CODE_CULTU.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(rpgRast_CODE_GROUP,filename = "rpgRast_CODE_GROUP.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(rpgRast_CODE_GROUP,filename = "rpgRast_CODE_GROUP.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(rpgRast_CODE_ASA,filename = "rpgRast_CODE_ASA.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(rpgRast_CODE_ASA,filename = "rpgRast_CODE_ASA.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(rpgRast_ID_EXPL,filename = "rpgRast_ID_EXPL.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(rpgRast_ID_EXPL,filename = "rpgRast_ID_EXPL.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(rpgRast_FILTSOIL,filename = "rpgRast_FILTSOIL.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(rpgRast_FILTSOIL,filename = "rpgRast_FILTSOIL.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(rpgRast_CAT_HRU,filename = "rpgRast_CAT_HRU.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(rpgRast_CAT_HRU,filename = "rpgRast_CAT_HRU.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(rdiv,filename = paste("step2_streams_new_div",fdiv,".tif",sep=""), options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(rdiv,filename = paste("step2_streams_new_div",fdiv,".asc",sep=""), format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(wintake_ID,filename = "wintake_ID.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(wintake_ID,filename = "wintake_ID.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(wrelease_ID,filename = "wrelease_ID.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(wrelease_ID,filename = "wrelease_ID.asc", format="ascii", overwrite=TRUE, bylayer = F)

# writeRaster(wderiv_ID,filename = "wrderiv_ID.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
writeRaster(wderiv_ID,filename = "wderiv_ID.asc", format="ascii", overwrite=TRUE, bylayer = F)

##_Lecture_fichiers_.asc
f1<-read.table("rpgRast_ID_PARCEL.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f1); dim(f1)

f2<-read.table("rpgRast_SURF_PARC.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f2); dim(f2)

f3<-read.table("rpgRast_CODE_CULTU.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f3); dim(f3)

f4<-read.table("rpgRast_CODE_GROUP.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f4); dim(f4)

f5<-read.table("rpgRast_CODE_ASA.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f5); dim(f5)

f6<-read.table("rpgRast_ID_EXPL.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f6); dim(f6)

f7<-read.table(paste("step2_streams_new_div",fdiv,".asc",sep=""), header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f7); dim(f7)

f8<-read.table("wintake_ID.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f8); dim(f8)

f9<-read.table("wrelease_ID.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f9); dim(f9)

f10<-read.table("wderiv_ID.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f10); dim(f10)

f11<-read.table("rpgRast_CAT_HRU.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f11); dim(f11)

f12<-read.table("rpgRast_FILTSOIL.asc", header=F, sep=" ", dec=".") # !!! Enlever préalablement l'entête du fichier "à la main"
str(f12); dim(f12)

##_Creation_fichier_.env
fout <- file(paste("spatial_ent[2017]_div",fdiv,"rectInbasin_numvalues.env",sep=""))
open(fout, "w")
cat("dimensions", file=fout, sep="\t" ,labels=NULL)
cat("\t", file = fout)
cat(dim(f1)[1], dim(f1)[2], file=fout, sep=" " ,labels=NULL)
cat("\n", file = fout)
cat("cloture", "closed", file=fout, sep=" ", labels=NULL)
cat("\n", file = fout)
cat("connexite", "eight", file=fout, sep="\t", labels=NULL)
cat("\n", file = fout)
cat("backgrounColor", 0.666707, 0.666707, 0.666707, file=fout, sep=" ", labels=NULL)
cat("\n", file = fout)
cat("attributs", file=fout, sep="\t", labels=NULL)
cat("\t", file = fout)
cat("idParcel(Number)", "surfParc(Number)" , "codeGroup(Number)", 
    "codeAsa(Number)", "idExpl(Number)", "catHru(Number)", file=fout, sep=" ", labels=NULL)
cat("\n", file = fout)
for (i in 1:dim(f1)[1]) {
  for (j in 1:dim(f1)[2]) {
    cat(f1[i,j], f2[i,j], f4[i,j], f5[i,j], f6[i,j], f11[i,j], file=fout, sep = "," , labels=NULL)
    cat(file=fout, sep = "\n" , labels=NULL)
  }
}
close(fout)


