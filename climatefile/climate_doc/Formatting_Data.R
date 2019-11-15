#####################################################################################
# Script to prepare input data for using scripts that computes hydrological signatures
# (use of scripts developed by Ivan Horner 2019)
# The objective is to get input rainfall, temperature, ET0 data for the chosen catchments
# and discharge. The time step for the analysis is daily time step
# Outputs:
# For the different catchments several .RData objects are saved.
# One contains time, rainfall (liquid + solid) (mm), ET0 (mm), T (°C)
# The other one contains discharge (mm/day)
# Written by Isabelle Braud, July 2019
####################################################################################
workdir <- "C:/braud/Repertoires_Travail_R/R_OZCAR_Signatures/"

# Required packages 
library(ncdf4)
library(raster)
library(chron)
library(zoo)
# Required sources
source("C:/braud/HydroTools/trunk/src/R/Isabelle_Braud/Scripts_Data_Paper_Borga_et_al_ESSD/readBDOH_lacune.fn")
source("C:/braud/HydroTools/trunk/src/R/Isabelle_Braud/Scripts_Analyse_Donnees_OZCAR/Get_SAFRAN_Data.R")
# Required sources codes and executable for time aggregation (codes of B. Renard)
# load Aggregator R User Interface functions set
generalfunkfolder <- "C:/braud/HydroTools/trunk/src/R/Isabelle_Braud/Scripts_Analyse_Donnees_OZCAR/"
source(paste0(generalfunkfolder, "aggregator-rui.R"))
# locate Aggregator.Exe
agg_fp <- "C:/braud/HydroTools/trunk/src/R/Isabelle_Braud/Scripts_Analyse_Donnees_OZCAR/"
aggExeFilePath(agg_fp)

#########################################################################
#
# Formatting rainfall and temperature time series from SAFRAN for France
#
##########################################################################

# For France, we want to get the SAFRAN grid points that correspond to the OZCAR observatories
# SAFRAN new version data of Rainfall, snowfall, ET0 and temperature are located in
# \\LY-DATA\LY-Unites\Riverly\Hhly\Entrepothh_depot\SAFRAN\NEW

# Done once to get the .shp with the SAFRAN grid
# To get the metadata and location of the SAFRAN isba grid points.
#dir <- "//LY-DATA/LY-Unites/Riverly/Hhly/Entrepothh_depot/SAFRAN/"
#load(paste0(dir,"isbagrid.Rdata")) # grid points coordinates
#load(paste0(dir,"polisba.Rdata")) # polygons of the grid points
#load(paste0(dir,"pol.Rdata")) # polygons of the grid points
# Output of a shapefile with the ISBA grid over France
#shapefile(pol,filename="SAFRAN_Grid")
#shapefile(polfrance,filename="SAFRAN_Grid")
# Note: We have to make the correspondence between the shapefile Ids and the netcdf files ids that
# are different as the netcdf files contain also the grids outside France. 
# It is possible using the in.France variable in the netcdf file (see GetCellsFromExtent function)

# Read SAFRAN data netcdf files
#dirsaf <- "//LY-DATA/LY-Unites/Riverly/Hhly/Entrepothh_depot/SAFRAN/"
#namencdf <- paste0(dirsaf,"NEW/safran_new_Tair.nc")
#ncdf = nc_open(namencdf)
#print(ncdf) # Allows to understand the structure of the file and variables names to write the following scripts

# Loops over the catchments/observatories to be analysed
# OTHU/Yzeron observatory
catchment <- c("YzeTaffignon","YzeCraponne","YzeCharbonnnieres","YzeRatier","YzeMercier","YzePontBarge")
#*** Caution: the names of the .shp, percent files, etc.. must be in the same order as the catchment names*****
# Directory with the .shp of the catchment in Lambert II étendu.
dirsig <- "C:/braud/AVuPUR_Yzeron/Couches_SIG/Sous_bassins_stations/"
shpname <- c("ss_bassins_stations_Nom__Yzeron@Taffignon.shp","ss_bassins_stations_Nom__Yzeron@Craponne_Urbain.shp",
			"ss_bassins_stations_Nom__Charbonnieres@Casino_Urbain.shp","ss_bassins_stations_Nom__Ratier@StGenis.shp",
			"contour_mercier6.shp","NewPdlB.shp")
# Directory with SAFRAN data	dir <- "//LY-DATA/LY-Unites/Riverly/Hhly/Entrepothh_depot/SAFRAN/NEW/"
dirsaf <- "//LY-DATA/LY-Unites/Riverly/Hhly/Entrepothh_depot/SAFRAN/NEW/"
# Time range for the various catchments
tbeg <- c("1958-09-01","1958-09-01","1958-09-01","1958-09-01","1958-09-01","1958-09-01")
tend <- c("2018-08-31","2018-08-31","2018-08-31","2018-08-31","2018-08-31","2018-08-31")
# Directory and names of the files with the percents SAFRAN grid points
# These files have two columns: the SAFRAN France grid numbers and the area of the intersection 
# of the grid points with the catchment boundary + one header line
dirper <- "C:/braud/Signatures_Obs_OZCAR/Data/Yzeron/"
pername <- c("safran_grid_percent_Taffignon.csv","safran_grid_percent_Craponne.csv",
			"safran_grid_percent_Charbonnieres.csv","safran_grid_percent_Ratier.csv",
			"safran_grid_percent_Mercier.csv","safran_grid_percent_PontBarge.csv")

# AgrHys observatory
catchment <- c("Kerbernez","Naizin")
#*** Caution: the names of the .shp, percent files, etc.. must be in the same order as the catchment names*****
# Directory with the .shp of the catchment in Lambert II étendu.
dirsig <- "C:/braud/Signatures_Obs_OZCAR/Data/AgrHys/"
shpname <- c("agrhys_kerbernez.shp","agrhys_naizin.shp")
# Directory with SAFRAN data	dir <- "//LY-DATA/LY-Unites/Riverly/Hhly/Entrepothh_depot/SAFRAN/NEW/"
dirsaf <- "//LY-DATA/LY-Unites/Riverly/Hhly/Entrepothh_depot/SAFRAN/NEW/"
# Time range for the various catchments
tbeg <- c("1958-09-01","1958-09-01")
tend <- c("2018-08-31","2018-08-31")
# Directory and names of the files with the percents SAFRAN grid points
# These files have two columns: the SAFRAN France grid numbers and the area of the intersection 
# of the grid points with the catchment boundary + one header line
dirper <- "C:/braud/Signatures_Obs_OZCAR/Data/AgrHys/"
pername <- c("safran_grid_percent_Kerbernez.csv","safran_grid_percent_Naizin.csv")

# OHMCV observatory
catchment <- c("Meyras","Claduegne","Gazel")
#*** Caution: the names of the .shp, percent files, etc.. must be in the same order as the catchment names*****
# Directory with the .shp of the catchment in Lambert II étendu.
dirsig <- "C:/braud/Signatures_Obs_OZCAR/Data/OHMCV/"
shpname <- c("boundary_meyras_LambIIE.shp","boundary_claduegne_5m_LambIIE.shp","boundary_gazel_5m_LambIIE.shp")
# Directory with SAFRAN data	dir <- "//LY-DATA/LY-Unites/Riverly/Hhly/Entrepothh_depot/SAFRAN/NEW/"
dirsaf <- "//LY-DATA/LY-Unites/Riverly/Hhly/Entrepothh_depot/SAFRAN/NEW/"
# Time range for the various catchments
tbeg <- c("1958-09-01","1958-09-01","1958-09-01")
tend <- c("2018-08-31","2018-08-31","2018-08-31")
# Directory and names of the files with the percents SAFRAN grid points
# These files have two columns: the SAFRAN France grid numbers and the area of the intersection 
# of the grid points with the catchment boundary + one header line
dirper <- "C:/braud/Signatures_Obs_OZCAR/Data/OHMCV/"
pername <- c("safran_grid_percent_Meyras.csv","safran_grid_percent_Claduegne.csv","safran_grid_percent_Gazel.csv")

########### Start the loop ################

areacat <- vector(length=length(catchment))
for (k in 1:length(catchment)) {
	print(catchment[k])
	shp <- shapefile(paste0(dirsig,shpname[k]))
	extent <- extent(shp)
	# We reformat extent to be compatible with the R function of I. Horner to retrieve the SAFRAN grid points 
	# that are within a given extent (a matrix with xmin, xmax in the first column and ymin, ymax in the second)
	# As the search is done using the grid centroid, we add a security of half the grid resolution to be sure
	# to get all the grid points (4000m)
	# **Caution: the extent is the bounding box so some cells of the rectangle may not intersect the catchment
	# boundary but intersect the bounding box.
	extent <- matrix(c(extent[1]-4000.,extent[2]+4000.,extent[3]-4000.,extent[4]+4000.),nrow=2,ncol=2)
	print(extent)
	# Get the data for precipitation (rainfall + snowfall, temperature and ET0)
	time_range <- c(tbeg[k],tend[k])
	# In the script of Ivan, time_range must be a POSIXct format.
	time_range <- as.POSIXct(strptime(time_range, format=c("%Y-%m-%d"), tz="UTC"))
	# data is a list with cell_infos, the cells number and coordinates (France grid points), the P, E,T lists (time, data)
	DATA <- FromSafranToData(dirsaf,time_range,extent)
	# We get the cells numbers in the grid with only French grid points.
	cells <- DATA$cell_infos
	print(cells)
	# For each catchment, we have defined the list of SAFRAN meshes numsaf
	# and the¨% intersection with the catchment boundary persaf for the french grid points using the .shp layer
	# For Yzeron/Charbonnières catchment, the SAFRAN meshes are
	toto <- read.table(paste0(dirper,pername[k]),header=TRUE,sep=";")
	numsaf <- toto[,1]
	if(length(numsaf)!=length(cells$ID)) print("matching cells and numsaf files are not matching")
	if(length(which(numsaf %in% cells$ID))!=length(numsaf)) print("error in cells matching")
	# caution: the numsaf and cells vector must be ordered in increasing order so that the matching is OK
	print (numsaf)
	persaf <- toto[,2]/sum(toto[,2])
	areacat[k] <- sum(toto[,2])
	print(areacat[k]) # To verify the catchment area.
	# We define the weighted average of P,E,T over the catchment
	P <- DATA$P$data%*%persaf # This is a matrix product
	E <- DATA$E$data%*%persaf 
	T <- DATA$T$data%*%persaf 
	# We save all the variables and time vectors in a data.frame as .RData
	DATA <- data.frame("Time" = DATA$P$time, "P"=P, "E"=E, "T"=T)
	save(DATA,file=paste0(workdir,"SAFRAN_",catchment[k],".RData"))
    }
	print(areacat)

############	 End of the loop #########
	
# Some trials to aggregate/visualize the data in this format
# To get the annual values (caution: here you get the calendar year not the hydrological year!!!!)
	toto <- aggregate(DATA$P,list(as.POSIXlt(DATA$Time)$year+1900),FUN="sum")
	mean(toto[,2])
	toto <- aggregate(DATA$E,list(as.POSIXlt(DATA$Time)$year+1900),FUN="sum")
	mean(toto[,2])
	toto <- aggregate(DATA$T,list(as.POSIXlt(DATA$Time)$year+1900),FUN="mean")
	mean(toto[,2])

#########################################################################
#
# Formatting discharge time series. It will be necessary to interpolate
# and/or aggregate the data to the daily time step.
#
##########################################################################

# Yzeron catchments
dirQ <- "C:/braud/Signatures_Obs_OZCAR/Data/Yzeron/"
aggExeWorkingDir(paste0(dirQ, "aggregator/"))
catchment <- c("YzeTaffignon","YzeCraponne","YzeCharbonnnieres","YzeRatier","YzeMercier","YzePontBarge")
# *** Caution: the information in the following vector must be in the same order as in the catchment vector****
Qname <- c()
# Time range for the various catchments
tbeg <- c("1988-09-01","1969-09-01","2009-09-01","2010-09-01","1997-09-01","1997-09-01")
tend <- c("2018-09-30","2018-09-30","2018-09-30","2018-09-30","2018-09-30","2018-09-30")

# Loop over the catchments
for(k in 1:length(catchment)) {
	# Loading data in the Hydro2 daily format (Taffignon and Craponne). Data are in l/s
	if(k<=2) {
		DATA <- read.table(paste0(data_fp, "V3015301_DEB.txt"), skip = 2, header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = "-9999")
		DATA[, 1] <- as.POSIXct(strptime(DATA[, 1], format = "%d/%m/%Y %H:%M:%S"), tz = "UTC")
		summary(DATA)
	}
	if(k>2) {
	# loading data in the BDOH format. Data are in l/s
		DATA <- read.table(paste0(data_fp, "V3015301_DEB.txt"), skip = 2, header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = "-9999")
		DATA[, 1] <- as.POSIXct(strptime(DATA[, 1], format = "%d/%m/%Y %H:%M:%S"), tz = "UTC")
		summary(DATA)
	}
	# Get the data at at daily time step from variable time step
	# Note: missing values are set to -9999.
	aggExeEasy(tv = DATA[, 1], mat = DATA[, 2, drop = FALSE],
			   period = as.Date(c("2009-09-01", "2018-08-31"), format = "%Y-%m-%d"),
			   timeinterval = "D", scale = 1, printstdout = TRUE, contagiousNA = TRUE)
	aggExeEasy(tv = DATA[, 1], mat = DATA[, 2, drop = FALSE],
			   period = as.Date(c("2009-09-01", "2018-08-31"), format = "%Y-%m-%d"),
			   timeinterval = "m", scale = 30, printstdout = TRUE, contagiousNA = FALSE)
	# reading results:
	# two files are created in the working directory (folder path stored in 'aggExe.wdfp'):
	# "OUT_time_xxx.txt"  --> which contains the aggregated time sequence
	# "OUT_value_xxx.txt" --> which contains the resulting aggregated values (as many column as in the input matrix)
	# Note: 'xxx' in file names stand for the code used for a give type of aggregation 
	#        e.g. a 3-day aggregation would result in a time file named: OUT_time_3D.txt
	time_seq <- read.table(file = paste(aggExe.wdfp, "OUT_time_1D.txt", sep = ""), stringsAsFactors = FALSE)
	value_mat <- read.table(file = paste(aggExe.wdfp, "OUT_value_1D.txt", sep = ""), header = FALSE, skip = 1, stringsAsFactors = FALSE, na.strings = .aggExe.navalue)
	head(time_seq)
	summary(value_mat)
	# IMPORTANT NOTE: the resulting time sequence in a sequence of time in the middle of the target time step (see example above)
	aggregated_DATA <- data.frame(Date = as.Date(time_seq[, 1], format = "%d/%m/%Y"), Value = value_mat[, 1])
	tail(aggregated_DATA)
	summary(aggregated_DATA)
	save(aggregated_DATA,file=paste0(workdir,"YzeCharbonnieres.RData"))
	}

