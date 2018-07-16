#Get data from DWD
#Download minutes values
#
#wind
#hourly/10min
path_dwd=("~/R/dwd/")
setwd(path_dwd)
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(Metrics)

 #url_wind="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/10_minutes/extreme_wind/recent/zehn_min_fx_Beschreibung_Stationen.txt"
 #download.file(url_wind,dest=(paste(path_dwd,"/stationen_10min.txt",sep="")))
 stationen_stunden=read.table("~/R/data/dwd/stationen_10min.txt",header=T)
 stationen_stunden=stationen_stunden[-1,]
 station_ids=stationen_stunden$Stations_id
 stationen_stunden$Stations_id=as.numeric(levels(stationen_stunden$Stations_id))
 stationen_stunden$geoBreite=as.numeric(levels(stationen_stunden$geoBreite))[stationen_stunden$geoBreite]
 stationen_stunden$geoLaenge=as.numeric(levels(stationen_stunden$geoLaenge))[stationen_stunden$geoLaenge]

#View(stationen_stunden)

#get the data from dwd and download
for(i in station_ids){
  #url dummy to get all stations
  url=paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/10_minutes/extreme_wind/recent/10minutenwerte_extrema_wind_",print(i),"_akt.zip", sep = "")
  print(url)
  #try to download
  try(
    download.file(url, dest=paste("~/R/data/dwd/station/hour/",print(i),".zip", sep=""), mode="wb"))
  setwd("~/R/data/dwd/station/hour")
  #unzip the files
  unzip(paste(print(i),".zip", sep=""))
}
