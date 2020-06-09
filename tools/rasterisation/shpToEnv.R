library(ConfigParser)
library(rlist)

dir <- getwd()
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  configFilePath = "config.cfg"
} else {
  configFilePath = args[1]
}
config = ConfigParser$new(NULL)
config$read(configFilePath)
#input <- paste(dir,"INPUT/", sep="")
#output <- paste(dir,"OUTPUT/", sep="")
input = config$get("inputDir", NA, "input")
output = config$get("outputDir", NA, "output")
dir.create(output, showWarnings = FALSE)

rpgPath = config$get("rpg", NA, "input")
wintakePath = config$get("wintake", NA, "input")
wreleasePath = config$get("wrelease", NA, "input")
wderivPath = config$get("wderiv", NA, "input")
hrusPath = config$get("hrus", NA, "input")
rasterAggregPath = config$get("rasterAggreg", NA, "input")
etenduePath = config$get("etendue", NA, "input")
fdiv = as.numeric(config$get("fdiv", NA, "params"))
#setwd(dir)

plotDirName = paste("plots_", etenduePath, sep="")
dir.create(file.path(output, plotDirName), showWarnings = FALSE)

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
#........................................................................
library(spgrass6)      # interfaçage avec Grass (données vecteur)
library(scales)        # interfaçage avec SAGA (données Raster)
#........................................................................

##_Projection_et_étendue_du_projet
proj=CRS("+init=epsg:2154")                   # projection du projet en RGF93

##_Chargement_données_vecteurs
#rpg=readShapePoly(paste('/home/bastien/Documents/2017-2020_These_GEAU/SIG/work_Buech/cormas/rasterisation/rpg2014_fus_buech_N4_gestionnaires.shp', sep="")) 
setwd(input)

#rpg=readShapePoly('FINAL_JOINT_PCL_INT_TAMP100[RPG2015]_GEST_TAMP100_BUECH[HYDRA_DREALPACA]_EXPL_INTERSECT_PCL_INTERIEUR_TAMP100_BUECH[HYDRA_DREALPACA].shp', sep="")
# rpg=readShapePoly('prov_pcl_buech_ZOOM.shp'); class(rpg)
#rpg=readShapePoly('FINAL_ZOOM.shp'); class(rpg)
cat("read", getwd(), rpgPath,"\n")
rpg = readShapePoly(rpgPath)
rpg$ID_PARCEL<-as.numeric(as.character(rpg$ID_PARCEL)) #transformation en "numeric", car c'est importé comme un "factor"
rpg$SURF_PARC<-as.numeric(as.character(rpg$SURF_PARC))
# TODO check why there is no such attribute in rectBasin.shp
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
if (interactive()) {
  x11()
} else {
  png(file.path("..", output, plotDirName, "rpg.png"))
}
plot(rpg)

if (! is.na(wintakePath)) {
  #wintake=readShapePoints('wintake.shp') # from BD HYDRA
  wintake=readShapePoints(wintakePath) # from BD HYDRA
  wintake$ID_ENT<-as.numeric(as.character(wintake$ID_ENT))
  projection(wintake)<-proj
  if (interactive()) {
    head(wintake,5); dim(wintake); class(wintake)
    x11()
  } else {
    png(file.path("..", output, plotDirName, "wintake.png"))
  }
  plot(wintake)
}

if (! is.na(wreleasePath)) {
  #wrelease=readShapePoints('wrelease.shp') # from BD HYDRA
  wrelease=readShapePoints(wreleasePath) # from BD HYDRA
  wrelease$ID_ENT<-as.numeric(as.character(wrelease$ID_ENT))
  projection(wintake)<-proj
  if (interactive()) {
    head(wrelease,5); dim(wrelease); class(wrelease)
    x11()
  } else {
    png(file.path("..", output, plotDirName, "wrelease.png"))
  }
  plot(wrelease)
}

if (! is.na(wderivPath)) {
  wderiv=readShapePoints(wderivPath)
  wderiv$id<-as.numeric(as.character(wderiv$id))
  projection(wderiv)<-proj
  if (interactive()) {
    head(wderiv,5); dim(wderiv); class(wderiv)
    x11()
  } else {
    png(file.path("..", output, plotDirName, "wderiv.png"))
  }
  plot(wderiv)
}

if (! is.na(hrusPath)) {
  # hrus=readShapePoly('hrusOK2_PCL_buech.shp')
  cat("read", hrusPath,"\n")
  hrus=readShapePoly(hrusPath)
  hrus$cat<-as.numeric(as.character(hrus$cat)) #!!!!transformer les autres variables pour les utiliser!!!
  projection(hrus)<-proj
  if (interactive()) {
    head(hrus,5); dim(hrus); class(hrus)
    x11()
  } else {
    png(file.path("..", output, plotDirName, "hrus.png"))
  }
  plot(hrus)
}

##_Découpage_raster_si_necessaire_selon_étendue
etendue=readShapePoly(etenduePath) # lecture de l'étendue de découpage
# etendue=readShapePoly('buech_emprise_zoomOK[2154].shp')
projection(etendue)<-proj 
etendueBox<-extent(etendue)                   # calcul de l'étendue rectangulaire du fichier  buffer
if (interactive()) {
  X11()
} else {
  png(file.path("..", output, plotDirName, "etendue.png"))
}

##_Chargement_données_raster_et_aggrégation
# r <- raster(ncol=44, nrow=54) #marche pas
r<-raster(rasterAggregPath, sep="")
if (interactive()) {
  str(r); dim(r); x11()
} else {
  png(file.path("..", output, plotDirName, "aggreg.png"))
}
plot(r)

par(mfcol=c(1,2))
plot(r, main="Raster étendue d'origine")
plot(etendueBox, col="red", add=T, lwd=3)

r=crop(r,etendueBox)                  # découpage du MNT pour qu'il corresponde à l'étendue
plot(r, main="Raster nouvelle étendue")
plot(etendueBox, col="red", add=T, lwd=3)
  
##_Aggrégation_du_raster_si_nécessaire
#fdiv=1 # !!! division resolution par ...
rdiv<-aggregate(r, fact=fdiv); str(rdiv) 
cat("Old dimensions are:", "\n", "nrows =", dim(r)[1], "\n", "ncols = ", dim(r)[2])
cat("New dimensions are:", "\n", "nrows =", dim(rdiv)[1], "\n", "ncols = ", dim(rdiv)[2])
if (interactive()) {
  X11()
} else {
  png(file.path("..", output, plotDirName, "aggreg_div.png"), width = 1500, height = 1500)
}
par(mfcol=c(4,4)); plot(r, col=bpy.colors(), main="Resolution de base"); plot(rdiv, col=bpy.colors(), main=paste("Resolution de base divisée",fdiv,"fois", sep=" "))

ras = list()

ras = list.append(ras, list(rpg, "ID_PARCEL", "rpgRast_ID_PARCEL.asc", "idParcel(Number)"))
ras = list.append(ras, list(rpg, "SURF_PARC", "rpgRast_SURF_PARC.asc", "surfParc(Number)"))
ras = list.append(ras, list(rpg, "CODE_CULTU", "rpgRast_CODE_CULTU.asc", "codeCultu(Number)"))
ras = list.append(ras, list(rpg, "CODE_GROUP", "rpgRast_CODE_GROUP.asc", "codeGroup(Number)"))
ras = list.append(ras, list(rpg, "CODE_ASA", "rpgRast_CODE_ASA.asc", "codeAsa(Number)"))
ras = list.append(ras, list(rpg, "ID_EXPL", "rpgRast_ID_EXPL.asc", "idExpl(Number)"))
ras = list.append(ras, list(rpg, "FILTSOIL", "rpgRast_FILTSOIL.asc", "filteringSoil(Number)"))
if (! is.na(hrusPath)) {
  ras = list.append(ras, list(hrus, "cat", "rpgRast_CAT_HRU.asc", "catHru(Number)"))
}
if (! is.na(wintakePath)) {
  ras = list.append(ras, list(wintake, "ID_ENT", "wintake_ID.asc", "wintakeId(Number)"))
}
if (! is.na(wreleasePath)) {
  ras = list.append(ras, list(wrelease, "ID_ENT", "wrelease_ID.asc", "wreleaseId(Number)"))
}
if (! is.na(wderivPath)) {
  ras = list.append(ras, list(wderiv, "id", "wderiv_ID.asc", "wderivId(Number)"))
}

# ##_Rasterisation_des_données_vecteurs_par_attribut
for (i in seq_along(ras)) {
  cat(ras[[i]][[2]], "\n")
  ras[[i]][[5]] = rasterize(ras[[i]][[1]], rdiv, field=ras[[i]][[2]], progress="text")
  plot(ras[[i]][[5]], main=ras[[i]][[4]])
}

ras = list.append(ras, list(NA, NA, paste("step2_streams_new_div", fdiv, ".asc", sep=""), "idReach(Number)", rdiv))

##_Enregistrement_des_rasters_et_ascii
#writeRaster(rpgRast,filename = "rpg2014_Buech.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
#writeRaster(rpgRast,filename = "rpg2014_Buech.asc", format="ascii", overwrite=TRUE)
cat("cwd ",getwd(),"\n")
cat("output ",output,"\n")
setwd(file.path("..", output))
newdir <- paste(dim(rdiv)[1], "x", dim(rdiv)[2], "_div", fdiv, "_ZOOM_2017_NUMVALUES", sep="")
dir.create(newdir, showWarnings = FALSE)
setwd(file.path("..", output, newdir, "/", sep=""))

for (e in ras) {
  #writeRaster(rpgRast_ID_PARCEL,filename = "rpgRast_ID_PARCEL.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
  writeRaster(e[[5]], filename = e[[3]], format="ascii", overwrite=TRUE, bylayer = F)
}

##_Lecture_fichiers_.asc (moins l'entête)
for (i in seq_along(ras)) {
  ras[[i]][[6]] = read.table(ras[[i]][[3]], header=F, sep=" ", dec=".", skip=6)
  str(ras[[i]][[6]]); dim(ras[[i]][[6]])
}

##_Creation_fichier_.env
f1 = ras[[1]][[6]]
fout = file(paste("spatial_ent[2017]_div", fdiv, "ZOOM_numvalues.env", sep=""))
open(fout, "w")
cat("dimensions", file=fout, sep="\t", labels=NULL)
cat("\t", file = fout)
cat(dim(f1)[1], dim(f1)[2], file=fout, sep=" " ,labels=NULL)
cat("\n", file = fout)
cat("cloture", "\t", "closed", "\n", file=fout, sep="", labels=NULL)
cat("connexite", "eight", file=fout, sep="\t", labels=NULL)
cat("\n", file = fout)
cat("backgroundColor", file=fout, labels=NULL)
cat("\t", file = fout)
cat(0.666707, 0.666707, 0.666707, file=fout, sep=" ", labels=NULL)
cat("\n", file = fout)
cat("attributs", file=fout, sep="\t", labels=NULL)
cat("\t", file = fout)
# print all header values
headers = c()
for (i in seq_along(ras)) {
  headers = c(headers, ras[[i]][[4]])
}
cat(headers, file=fout, sep=" ", labels=NULL)
cat("\n", file = fout)

for (i in 1:dim(f1)[1]) {
   for (j in 1:dim(f1)[2]) {
     # concat all values for one line
     line = c()
     for (e in ras) {
       line = c(line, e[[6]][i,j])
     }
     cat(line, file=fout, sep = "," , labels=NULL)
     cat(file=fout, sep = "\n" , labels=NULL)
   }
}
close(fout)


