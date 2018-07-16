#Get daily data from DWD
#

#variables
path_dwd=("~/data/DWD/station")


station_coordinat=read.table(paste(path_dwd,"/stationen",sep=""), header= T)

station_ids=station_coordinat$Stations_id
station_coordinat$Stations_id=as.numeric(levels(station_coordinat$Stations_id))
station_coordinat$geoBreite=as.numeric(levels(station_coordinat$geoBreite))[station_coordinat$geoBreite]
station_coordinat$geoLaenge=as.numeric(levels(station_coordinat$geoLaenge))[station_coordinat$geoLaenge]
station_coordinat=station_coordinat[-1,]
#View(station_coordinates)
stationX=station_coordinat$geoBreite
stationY=station_coordinat$geoLaenge



#get the data from dwd and download
for(i in station_ids){
  #url dummy to get all stations
  url=paste("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_",print(i),"_akt.zip", sep = "")
  print(url)
  #try to download
  try(
    download.file(url, dest=paste("~/data/DWD/station/all/",print(i),".zip", sep=""), mode="wb"))
  setwd("~/data/DWD/station/all")
  #unzip the files
  unzip(paste(print(i),".zip", sep=""))
}



#load data to R and assign filename
#get all files of stations
filename=list.files(path = paste(path_dwd,"/all/",sep=""), pattern ="produkt")
#for each file in array import data/read table
for ( i in filename){
  setwd("~/data/DWD/station/all")
  assign(paste(i), read.table(file=paste(i),sep=";" ,header=T))
}

#for each file in array subset only data from 2018 and add geoBreite, geoLaenge and Hoehe by id
for (i in filename){
  
  assign(i,subset(get(i),grepl("2018", getElement((get(i)),"MESS_DATUM"))))
  
  assign(paste(i), cbind(get(i), "geoBreite" =(getElement(station_coordinat[is.element(station_coordinat$Stations_id, getElement(get(i),"STATIONS_ID")[1]),],"geoBreite"))))
  assign(paste(i), cbind(get(i), "geoLaenge"=(getElement(station_coordinat[is.element(station_coordinat$Stations_id, getElement(get(i), "STATIONS_ID")[1]),],"geoLaenge"))))
  assign(paste(i), cbind(get(i), "Stationshoehe"=(getElement(station_coordinat[is.element(station_coordinat$Stations_id, getElement(get(i), "STATIONS_ID")[1]),],"Stationshoehe"))))
}


#station_coordinat$Stations_id==getElement(get(i),"STATIONS_ID")[1]
#station_coordinat$Stations_id==getElement(get(filename[1]),"STATIONS_ID")[1]
#getElement(station_coordinat$Stations_id==getElement(get(filename[1]),"STATIONS_ID")[1], "geoBreite")
#geoBreite=station_coordinat$geoBreite[which(station_coordinat$stations_id==(getElement(get(filename[1]),"STATIONS_ID")[1]))]
#station_coordinat$geoBreite
#geoBreite=station_coordinat$Stations_id==getElement(get(filename[1]),"STATIONS_ID")[1]
#station_coordinat$Stations_id=row.names(station_coordinat)
#station_coordinat$geoBreite
#getElementstation_coordinat$Stations_id==44
#getElement(station_coordinat[is.element(station_coordinat$Stations_id, getElement(get(filename[1]),"STATIONS_ID")[1]),],"geoBreite")


#stations: get tables with mean and coordinates


station=ls(pattern="produkt")
n=NULL
for (i in station){
  x=subset(get(i), select=c("STATIONS_ID","MESS_DATUM","FM","FX","geoBreite","geoLaenge","Stationshoehe"))
  
  n=rbind(n,x)
  
  #assign(print(i),x)
  
}
assign("all_station",n)
write.csv(all_station, file="all_station_neu.csv")
rm(list=station)