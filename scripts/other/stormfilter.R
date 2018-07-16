#Script Stormfilter
#This script subset the values of storm events per day during the observation period.
#Input: all_station_hour_with data.csv
#Output: Storm_data_station

setwd("~/R/MA/dwd")
station_data=read.csv("all_station_neu.csv")
#delete rows with F=-999 
station_data_n=subset(station_data, station_data$FX!=-999)
#subset the observation period
station_data_n=subset(station_data_n, station_data_n$MESS_DATUM>20180117)
#subset storm events with F > 17.1 m/s 

station_data_n_storm=subset(station_data_n, station_data_n$FX>17.1)
station_data_n_storm$X=NULL
station_data_n_storm=unique(station_data_n_storm)


x=count(station_data_n_storm, MESS_DATUM)
#10 dates sorted by number of station which measure storm
x_n=x[order(x$n, decreasing = T),]
x_n=x_n[1:10,]
dates_n=unique(x_n$MESS_DATUM)

#50 highest gust measurments at at least two stations 
x_fx=count(station_data_n_storm, FX, MESS_DATUM)
x_fx=x_fx[order(x_fx$FX,decreasing =T),]
x_fx=x_fx[1:20,]

dates_fx=unique(x_fx$MESS_DATUM)

dates_fx
dates_n
dates=c(dates_n,dates_fx)
dates=unique(dates)
sort(dates)


# 
# 
# 
# 
# station_data_n=subset(station_data_n, station_data_n$Stationshoehe<1000)
# dat/Users/m_meij02/R/scripts/dwd/getStationsHour.Res=station_data_n_storm$MESS_DATUM
# dates=as.character(dates)
# dates=substr(dates,1,8)
# dates=as.integer(dates)
# dates=dates[!duplicated(dates)]
# dates
# #grepl("2018-01", model_acc_without$V1)
# storm_station_data=data.frame(X=integer(),STATION_ID=numeric(),MESS_DATUM=numeric(),f=numeric(),geoBreite=numeric(), geoLaenge=numeric(),Stationshoehe=numeric())
# for(i in dates){
#      x=subset(station_data_n, (grepl(i,station_data_n$MESS_DATUM)))
#      new=list(X=x$X,STATION_ID=x$STATIONS_ID,MESS_DATUM=x$MESS_DATUM,f=x$F,geoBreite=x$geoBreite, geoLaenge=x$geoLaenge,Stationshoehe=x$Stationshoehe)
#      storm_station_data=rbind(new,storm_station_data)
#      
# }
# 
# setwd("~/R/MA/dwd")
# write.csv(storm_station_data, "storm_station_data1.csv")
# 
# setwd("~/R/MA/dwd")
# storm_station_data=read.csv("storm_station_data1.csv", header=T)
# wrf_hour_cords=read.csv("~/R/MA/dwd/wrf_hour_coords.csv", header=T)
# 
# #get dates of storm events
# dates=storm_station_data$MESS_DATUM
# dates=as.character(dates)
# dates=substr(dates,1,8)
# dates=as.integer(dates)
# dates=dates[!duplicated(dates)]
# dates
# 
# 
# 
