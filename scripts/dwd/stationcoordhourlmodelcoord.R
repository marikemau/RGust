#stationcoordinates hourly and wrfcoordinates merge

#get coordinates of the wrf similar to station coords

library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(scales)
library(RANN)

path_dwd="~/data/DWD/station"
coordwrf=read.csv("~/data/test/wrf/csv/testdaten/coordinates_wrf.csv")
coordwrf=coordwrf[,c("V5","V6")]

station_coordinat=read.csv(paste(path_dwd,"/station_daily.csv",sep=""), header= T)
station_ids=station_coordinat$Stations_id
#station_coordinat$Stations_id=as.numeric(levels(station_coordinat$Stations_id))
#station_coordinat$geoBreite=as.numeric(levels(station_coordinat$geoBreite))[station_coordinat$geoBreite]
#station_coordinat$geoLaenge=as.numeric(levels(station_coordinat$geoLaenge))[station_coordinat$geoLaenge]
#station_coordinat$Stationshoehe=as.numeric(levels(station_coordinat$Stationshoehe))[station_coordinat$Stationshoehe]
#station_coordinat=station_coordinat[-1,]
station_coordinat=subset(station_coordinat,grepl("2018", getElement(station_coordinat,"bis_datum")))
station_coordinat=station_coordinat[,c("geoLaenge","geoBreite")]


coordinateswrf=data.frame(V5=numeric(), V6=numeric(),station_x=numeric(),station_y=numeric())

for (i in 1:505) {
  cid=nn2(coordwrf,station_coordinat[i,],radius=0,searchtype = "priority", k=1)
  cid=as.integer(paste(cid))
  print(cid)
  
  x=coordwrf[cid,][1]
  x=x$V5
  x
  y=coordwrf[cid,][2]
  y=y$V6
  y
  newentryw=list(V5=x, V6=y, station_x=station_coordinat[i,]$geoLaenge, station_y=station_coordinat[i,]$geoBreite)
  
  #coordinateswrf=rbind(coordinateswrf,newentry)
  coordinateswrf=rbindlist(list(coordinateswrf,newentryw), use.names = F, fill = F, idcol = F)
  
}
setwd(path_dwd)
write.csv(coordinateswrf, "wrf_hour_coords.csv")