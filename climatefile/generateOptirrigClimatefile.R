############################################################
# Script to generate Optirrig climatefile
############################################################
#The scipt extract T, ETP and P (total precipitation) from J2K output variables at the water intake localisation (called "Les Vierges, ID_ALL = 41 in J2K)
# of the Aspres case study, and fill in RG from Meteo France mean monthly data (1990-2011)
# Oct 2019 - BR
############################################################
require(zoo)

wd <- "/home/bastien/Documents/2017-2020_These_GEAU/Work_Optirrig/Optirrig/WatASit/WatASit_Rcoupler/"

# Extract meteo from J2K variables
folder="/home/bastien/Documents/JAMS-3.9_02-bin/JAMS/data/J2K_BuechRef/output/"
filename <-"41_buechRef_2016-2017.sdat" #SAFRAN meteo time series from J2K variables extracted at the case study (ID 41 in J2K: "Les Vierges" water intake)
dateStart <- as.Date("2016-01-01"); dateEnd <- as.Date("2017-12-31")

# Lecture du .dat issue de JADE
temp <- read.table(paste(folder,filename,sep=""), as.is = TRUE, skip = 3, header =F, sep = "", comment.char = "#", na.strings = "-9999.0")
colnames(temp) <- c("day","hour","precip","tmean","etpot")
precip <- cbind(temp$precip); tmean <- cbind(temp$tmean); etpot <- cbind(temp$etpot)

# Make time series and extract on time window
dates     <- as.Date(temp$day,"%Y-%m-%d"); 
ZooPrecip <- zoo(precip, dates);  P <- window(ZooPrecip, start = dateStart, end = dateEnd)
ZooTmean  <- zoo(tmean, dates);   T <- window(ZooTmean, start = dateStart, end = dateEnd)
ZooEtp    <- zoo(etpot, dates); etp <- window(ZooEtp, start = dateStart, end = dateEnd)
date<-index(P); year <- P; year <- as.numeric(format(date,"%Y"))
month <- as.numeric(format(date,"%m")); day <- as.numeric(format(date,"%d")); doy <- seq(from=1,to=length(P),by=1)

# Add Rg monthly values (in Joules/cm-2)
# -> Data are from the "Fiche climatologique" available at https://donneespubliques.meteofrance.fr/?fond=rubrique&id_rubrique=29
Rg <- month ; Rg[which(Rg == 1)]<-16498/31 ; Rg[which(Rg == 2)]<- 25652/28 ; Rg[which(Rg == 3)]<- 42013/31 ; Rg[which(Rg == 4)]<- 48861/30 ; Rg[which(Rg == 5)]<- 60160/31 ; Rg[which(Rg == 6)]<- 69145/30 ; Rg[which(Rg == 7)]<- 72085/31 ; Rg[which(Rg == 8)]<- 61407/31 ; Rg[which(Rg == 9)]<- 44700/30 ; Rg[which(Rg == 10)]<- 30067/31 ; Rg[which(Rg == 11)]<- 17972/30 ; Rg[which(Rg == 12)]<- 12053/31 ; 

# Format and write climatefile
meteo <- data.frame(date,year,month,day,doy,P,etp,Rg,T)
write.csv(meteo,paste0(wd,'climatefile/climate_buech_',as.numeric(format(dateStart,"%Y")),"-",as.numeric(format(dateEnd,"%Y")),'.csv'), row.names = FALSE, quote = FALSE, na = "NA", eol = "\n")
