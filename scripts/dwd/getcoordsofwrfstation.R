#get coordinates of the wrf similar to station coords

library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(scales)
library(RANN)
#variabes
path_dwd="~/data/DWD/station"
coordwrf=read.csv("~/data/test/wrf/csv/wrf_2018012206z_30.csv",header=F)
coordico=read.csv("~/data/test/ico_2018031400z_048_7km.csv", header=F)
coordcos=read.csv("~/data/test/cos_2018031406z_027.csv", header=F)
coordgfs=read.csv("~/data/test/gfs2018031400h48.csv", header=F)
coordwrf=coordwrf[,c("V5","V6")]
coordico=subset(coordico, select=c("V5","V6"))
coordgfs=subset(coordgfs, select=c("V5","V6"))
coordcos=subset(coordcos, select=c("V5","V6"))
#coordsstation=read.csv("~/data/test/wrf/csv/testdaten/all_station.csv")
#station_coordinat=read.table(paste(path_dwd,"/stationen",sep=""), header= T)
station_coordinat=read.table("/home/m_meij02/data/DWD/station/stationen_10min.txt",header=T)
station_ids=station_coordinat$Stations_id
station_coordinat$Stations_id=as.numeric(levels(station_coordinat$Stations_id))
station_coordinat$geoBreite=as.numeric(levels(station_coordinat$geoBreite))[station_coordinat$geoBreite]
station_coordinat$geoLaenge=as.numeric(levels(station_coordinat$geoLaenge))[station_coordinat$geoLaenge]
station_coordinat$Stationshoehe=as.numeric(levels(station_coordinat$Stationshoehe))[station_coordinat$Stationshoehe]
station_coordinat=station_coordinat[-1,]
station_coordinat=subset(station_coordinat,grepl("2018", getElement(station_coordinat,"bis_datum")))
station_coordinat=station_coordinat[,c("geoLaenge","geoBreite")]
length=count(station_coordinat)

station_coordinat[1,]
coordwrf
coordinateswrf=data.frame(V5=numeric(), V6=numeric(),station_x=numeric(),station_y=numeric())
coordinatesgfs=data.frame(V5=numeric(), V6=numeric(),station_x=numeric(),station_y=numeric())
coordinatesico=data.frame(V5=numeric(), V6=numeric(),station_x=numeric(),station_y=numeric())
coordinatescos=data.frame(V5=numeric(), V6=numeric(),station_x=numeric(),station_y=numeric())
for (i in 1:269) {
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
  
  #####
  # iid=nn2(coordico,station_coordinat[i,],radius=0,searchtype = "priority", k=1)
  # iid=as.integer(paste(iid))
  # print(iid)
  # 
  # x=coordico[iid,][1]
  # x=x$V5
  # x
  # y=coordico[iid,][2]
  # y=y$V6
  # y
  # newentryi=list(V5=x, V6=y, station_x=station_coordinat[i,]$geoLaenge, station_y=station_coordinat[i,]$geoBreite)
  # 
  # #coordinateswrf=rbind(coordinateswrf,newentry)
  # coordinatesico=rbindlist(list(coordinatesico,newentryi), use.names = F, fill = F, idcol = F)
  # 
  # #####
  # gid=nn2(coordgfs,station_coordinat[i,],radius=0,searchtype = "priority", k=1)
  # gid=as.integer(paste(gid))
  # print(gid)
  # 
  # x=coordgfs[gid,][1]
  # x=x$V5
  # x
  # y=coordgfs[gid,][2]
  # y=y$V6
  # y
  # newentryg=list(V5=x, V6=y, station_x=station_coordinat[i,]$geoLaenge, station_y=station_coordinat[i,]$geoBreite)
  # 
  # #coordinateswrf=rbind(coordinateswrf,newentry)
  # coordinatesgfs=rbindlist(list(coordinatesgfs,newentryg), use.names = F, fill = F, idcol = F)
  # #####
  # coid=nn2(coordcos,station_coordinat[i,],radius=0,searchtype = "priority", k=1)
  # coid=as.integer(paste(coid))
  # print(coid)
  # 
  # x=coordcos[coid,][1]
  # x=x$V5
  # x
  # y=coordwrf[coid,][2]
  # y=y$V6
  # y
  # newentryc=list(V5=x, V6=y, station_x=station_coordinat[i,]$geoLaenge, station_y=station_coordinat[i,]$geoBreite)
  # 
  # #coordinateswrf=rbind(coordinateswrf,newentry)
  # coordinatescos=rbindlist(list(coordinatescos,newentryc), use.names = F, fill = F, idcol = F)
  # 
}
setwd("~/data/DWD/station")

write.csv(coordinateswrf, file="coord_station_wrf_10min.csv")
write.csv(coordinatesgfs, file="coord_station_gfs.csv")
write.csv(coordinatesico, file="coord_station_ico.csv")
write.csv(coordinatescos, file="coord_station_cos.csv")
#coordwrf=`wrf_2018011800z_*`[c("V5","V6")]
#coordwrf=as.matrix(coordwrf)
#coordwrf[2,]
#coords[1,]
#x=nn2(coordwrf,coords[1,],radius=0,searchtype = "priority", k=1)
#x=x[1][1]
#x=as.numeric(paste(x))
#x=as.numeric(x)
#xx=coordwrf[x,]