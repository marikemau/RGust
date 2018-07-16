#merge data
#wind
#hourly/10min
path_dwd=("~/R/MA/dwd/station")
pathgfs="D:/Masterarbeit_micky/gfs"
setwd(path_dwd)
library(dplyr)
library(readr)
library(data.table) #for multimerge function

library(Metrics)
library(xts)


apply.hourly <- function(x, FUN,...) {
  ep <- endpoints(x, 'hours')
  period.apply(x, ep, FUN, ...)
}

#load data to R and assign filename
#get all files of stations
#for each file in array import data/read table
#filename=filename[1:3]

getStormDayinHours=function(d,m){
  minvalues=NULL
  path_dwd=("~/R/MA/dwd/station")
  filename=list.files(path = paste(path_dwd,"/hour/",sep=""), pattern ="produkt_zehn")

  
  pattern=paste("2018",m,d,sep="")
  
  for ( i in filename){
    setwd("~/R/MA/dwd/station/hour")
    x=read.table(file=i,sep=";" ,header=T)
    x$MESS_DATUM1=as.character(x$MESS_DATUM)
    x=subset(x,grepl(pattern = pattern,(substr(x$MESS_DATUM1,1,8))))
    if(!nrow(x)>1){next}
    x=subset(x,x$FX_10!=-999)
    x$date=substr(x$MESS_DATUM1,1,8)
    x$date=as.Date.character(x$date,format="%Y%m%d")
    x$time=paste0(substr(x$MESS_DATUM1,9,10),":",substr(x$MESS_DATUM1,11,12))
    y=paste(x$date, x$time)
    x$timestamp=as.POSIXct(y)
    x=subset(x,select=c(STATIONS_ID,MESS_DATUM,MESS_DATUM1,FX_10,timestamp,date,time))
    #xts(x$FX_10,as.POSIXct(x$timestamp))
    h=apply.hourly(xts(x$FX_10,as.POSIXct(x$timestamp)),max)
    h=data.frame(date=index(h), FX_MAX=coredata(h))
    x1=merge(x,h, by.x="timestamp",by.y="date", all=T)
    x1=x1[complete.cases(x1), ]
    x1$time=paste0(substr(x1$time,1,2),":00")
    y <- paste(x1$date, x1$time)
    x1$timestamp2=as.POSIXct(y)
   x1=subset(x1,select=c(STATIONS_ID,MESS_DATUM1,FX_MAX,timestamp2,date,time))
    minvalues=rbindlist(list(x1,minvalues))
    print(i)
  }#end for-loop
  
  station_coords=read.table("~/R/MA/dwd/stationen_10min.txt",header=T)
  station_coords=station_coords[-1,]
  #stationid=station_coords$Stations_id
  station_coords$Stations_id=as.numeric(levels(station_coords$Stations_id))[station_coords$Stations_id]
  print("getStorm")
  station_jan=merge(x=minvalues,y=station_coords, by.x="STATIONS_ID",by.y="Stations_id")
  print(station_jan[1])
  return(station_jan)
}
#stormday=getStormDayinHours("18","03")



#Step 1
#merge the needed prediction hours for each run 
#createpattern for filenameconditions 
createpattern= function(n,m,d){
  pattern=list()
  if (n == "gfs") {
    run=list("00","06","12","18")
    
    if (d<10){
      for (i in 1:4){
        p= ((paste(n,"20180",m,"0",d,run[i],"h", sep="")))
        pattern=c(pattern,p)
      }}
    
    else{ 
      for (i in 1:4){
        p= ((paste(n,"20180",m,d,run[i],"h", sep="")))
        pattern=c(pattern,p)
      }
    }
    
    return(pattern)}
  
  
  
  else return ("wrong")
}
#testi=createpattern("gfs",01,17)

#function to import all data into one list
#multmerge
multmerge = function(path,pattern){
  print("Step1")
  pathgfs="D:/Masterarbeit_Micky/gfs/"
  setwd(pathgfs)
  #l=list("00","03","06","09","12", "15", "18", "21", "24", "27", "30", "33", "36", "39", "42", "45", "48")
  l00=list( "24", "27", "30", "33", "36", "39", "42", "45")
  l06=list("18", "21", "24", "27", "30", "33", "36", "39")
  l12=list("12", "15", "18", "21", "24", "27", "30", "33")
  l18=list("06","09","12", "15", "18", "21", "24", "27")
  if (grepl("00h",substr(pattern,12,14))){
    filenames=list()
    #only need the 24hours of the next day, so cutteed here the predictionhours
    for (i in l00){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    #return(rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))})))
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))}))
    x$RUN="00"
    return(x)
  }
  if (grepl("06h",substr(pattern,12,14))){
    filenames=list()
    for (i in l06){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    #return(rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))})))
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))}))
    x$RUN="06"
    return(x)
  }
  if (grepl("12h",substr(pattern,12,14))){
    filenames=list()
    for (i in l12){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    #return(rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))})))
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))}))
    x$RUN="12"
    return(x)
  }
  if (grepl("18h",substr(pattern,12,14))){
    filenames=list()
    for (i in l18){
      # filenames=list.files(path=path, pattern=paste(pattern,0,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #return(rbindlist(lapply(filenames, function(x){read.csv(x, header= F)})))
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))}))
    x$RUN="18"
    return(x)
  }
  else("wrong")
  
  
  
}
#l=read.csv("gfs_2018012018z_05.csv")

#gfs_test=multmerge(pathgfs,testi[1])

#Step2 
#croop coordinates by station coordinates
#used filter function for package dplyr
cutgfsCoordinates=function(gfs_file){
  if(is.data.frame(gfs_file)){
    #import station data
    setwd("~/R/MA/dwd/")
    coord_station_gfs=read.csv("coord_station_wrf_10min.csv")  
    #print(coord_station_gfs)
    gfs_new=data.frame(V1=factor(),V2=factor(),V5=numeric(), V6=numeric(),V7=numeric(),RUN=factor())
    n=nrow(coord_station_gfs)
    print("Step2")
    for (i in 1:n ){
      gfs=filter(gfs_file, (getElement(gfs_file,"V5")==coord_station_gfs$V5[i] & getElement(gfs_file,"V6")==coord_station_gfs$V6[i]))
      new=list(V1=gfs$V1, V2=gfs$V2, V5=gfs$V5 , V6=gfs$V6, V7=gfs$V7, RUN=gfs$RUN )
      gfs_new=rbindlist(list(gfs_new,new),use.names = F, fill = F, idcol = F)
      # gfs_new=rbind(gfs_new,gfs)
      
    }
    print(gfs_new[1])
    return(gfs_new)
  }
  
  else{
    print("gfs_file empty")
    return("empty")}
}


#gfs_cutted2=cutgfsCoordinates(gfs_test)
###########################################################################
#
###########################################################################
#station_file=stormday
#model_file=gfs_cutted2
mergeStationModel=function(model_file,station_file){
  print("h")
  model_file$timestamp=as.POSIXct(model_file$V2)
  #model_file$timestamp=model_file$timestamp+60*60*2
  setwd("~/R/MA/dwd")
  coord_station_gfs=read.csv("coord_station_wrf_10min.csv") 
  station_file$geoBreite=as.numeric(levels(station_file$geoBreite))[station_file$geoBreite]
  station_file$geoLaenge=as.numeric(levels(station_file$geoLaenge))[station_file$geoLaenge]
  
  station_data=merge(station_file,coord_station_gfs, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
  print(station_data[1])
  station_model_file=merge(x=model_file,y=station_data, by.x=c("V5","V6","timestamp"),by.y=c("V5","V6","timestamp2"))
  
  return(station_model_file)
}


#station_model_merged2=mergeStationModel(gfs_cutted2,stormday)

getAccuracy=function(station_model_file){
  require(Metrics)
 # accu=NULL
  accu=read.csv("~/R/MA/gfs/pattern/stormgfs/stormaccu_gfs.csv")
  accu$timestamp=as.POSIXct(accu$timestamp)
  accu$X=NULL
  station_model_file=subset(station_model_file, select=c("V5","V6","timestamp","RUN","V1","V2","STATIONS_ID","V7","FX_MAX","Stationshoehe","date","time","MESS_DATUM1"))
  for(i in unique(station_model_file$timestamp)){
    x=subset(station_model_file,station_model_file$timestamp==i)
    acc_new=data.frame(timestamp=x$timestamp[1],MESS_DATUM1=x$MESS_DATUM1[1],time=x$time[1],RUN=x$RUN[1], MAX_gfs=max(x$V7),MAX_STATION=max(x$FX_MAX), BIAS=bias(x$FX_MAX,x$V7),RMSE=rmse(x$FX_MAX,x$V7),Accuracy=accuracy(x$FX_MAX,x$V7))
    
    accu=rbindlist(list(acc_new,accu))
    
  }
  
  # accu=read.csv("~/R/MA/gfs/pattern/accuracy_gfs.csv")
  # accu$X=NULL
  # acc_new=rbindlist(list(accu,acc_new),use.names = F,fill=F,idcol=F)
  write.csv(accu,"~/R/MA/gfs/pattern/stormgfs/stormaccu_gfs.csv")
  return(station_model_file)
}
# stormday=getStormDayinHours("18","03")
# testi=createpattern("gfs",01,17)
# gfs_test=multmerge(pathgfs,testi[1])
# gfs_cutted2=cutgfsCoordinates(gfs_test)
# station_model_merged2=mergeStationModel(gfs_cutted2,stormday)
# accu_test=getAccuracy(station_model_merged2)
# station_model_file=station_model_merged2
getContigencyTable=function(station_model_file){
  timestamps=unique(station_model_file$timestamp)
  stationid=unique(station_model_file$STATIONS_ID)
  valueprediction=NULL
  for (i in timestamps){
    for (j in stationid) {
      x=subset(station_model_file,station_model_file$timestamp==i & station_model_file$STATIONS_ID==j)
      if(nrow(x)==0){next}
      if(x$V7>17.1 && x$FX_MAX >17.1){
        new=list(date=x$date[1], timestamp=x$timestamp[1], STATIONS_ID=x$STATIONS_ID[1], V5=x$V5[1],V6=x$V6[1],V7=x$V7[1],FX_MAX=x$FX_MAX[1],RUN=x$RUN[1], Stationshoehe=x$Stationshoehe[1], A=1,B=0,C=0,D=0)}
      else if(x$V7>17.1 && x$FX_MAX <17.2){
        new=list(date=x$date[1], timestamp=x$timestamp[1], STATIONS_ID=x$STATIONS_ID[1], V5=x$V5[1],V6=x$V6[1],V7=x$V7[1],FX_MAX=x$FX_MAX[1],RUN=x$RUN[1], Stationshoehe=x$Stationshoehe[1], A=0,B=1,C=0,D=0)}
      else if(x$V7<17.2 && x$FX_MAX >17.1){
        new=list(date=x$date[1], timestamp=x$timestamp[1], STATIONS_ID=x$STATIONS_ID[1], V5=x$V5[1],V6=x$V6[1],V7=x$V7[1],FX_MAX=x$FX_MAX[1], RUN=x$RUN[1],Stationshoehe=x$Stationshoehe[1], A=0,B=0,C=1,D=0)}
      else if(x$V7<17.2 && x$FX_MAX <17.2){
        new=list(date=x$date[1], timestamp=x$timestamp[1], STATIONS_ID=x$STATIONS_ID[1], V5=x$V5[1],V6=x$V6[1],V7=x$V7[1],FX_MAX=x$FX_MAX[1], RUN=x$RUN[1],Stationshoehe=x$Stationshoehe[1], A=0,B=0,C=0,D=1)}
      else return("error")
      
      valueprediction=rbindlist(list(new,valueprediction),use.names = F,fill=F)
      
    }
  }
  write.csv(valueprediction,paste("~/R/MA/gfs/pattern/stormgfs/fp/fp_storm_",valueprediction$date[1],"_",valueprediction$RUN[1],".csv",sep=""))
  return(station_model_file)
}

#x=getContigencyTable(station_model_merged2)

switchRun=function(storm_gfs_new){
  timestamps=unique(storm_gfs_new$timestamp)
  gfs_per_day_all_runs=NULL
  stationid=unique(storm_gfs_new$STATIONS_ID)
  
  for (i in timestamps){
    for (j in stationid){
      
      x=subset(storm_gfs_new,storm_gfs_new$timestamp==i & storm_gfs_new$STATIONS_ID==j)
      xnew=data.frame(date=x$date[1],timestamp=x$timestamp[1],MESS_DATUM1=x$MESS_DATUM1[1],time=x$time[1],STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7=max(x$V7),FX_MAX=max(x$FX_MAX), BIAS=bias(x$FX_MAX,x$V7),RMSE=rmse(x$FX_MAX,x$V7),Accuracy=accuracy(x$FX_MAX,x$V7),Stationshoehe=x$Stationshoehe[1])
      
      
      gfs_per_day_all_runs=rbindlist(list(xnew,gfs_per_day_all_runs),use.names = F,fill=F)
      
    }
  }
  
  write.csv(gfs_per_day_all_runs,paste("~/R/MA/gfs/pattern/stormgfs/gfs_all_no_runs_",gfs_per_day_all_runs$date[1],".csv",sep=""))
  
}

getStormEvents=function(month,day,fmonth,fday){
  gfs_storm=NULL
  gfs_storm_new=NULL
  stormday=getStormDayinHours(day,month)
  pattern=createpattern("gfs",fmonth,fday)
  for (i in pattern){
    print(i)
    h=cutgfsCoordinates(multmerge(pathgfs,i))
    storm=mergeStationModel(h,stormday)
    storm=getAccuracy(storm)
    storm=getContigencyTable(storm)
    gfs_storm=rbindlist(list(gfs_storm,storm),use.names = F, fill = F, idcol = F)
  }
  for(i in 1:nrow(gfs_storm)) {
    row <- gfs_storm[i,]
    row$rmse=rmse(row$FX_MAX,row$V7)
    gfs_storm_new=rbindlist(list(row, gfs_storm_new))
  }
  
  
  write.csv(gfs_storm_new, paste("~/R/MA/gfs/pattern/stormgfs/stormgfs_",month,day,".csv",sep=""))
  switchRun(gfs_storm_new)
  
  
}




#getStormEvents("01","18",01,17)
#getStormEvents("01","20",01,19)
#getStormEvents("01","29",01,28)
#getStormEvents("01","31",01,30)
#getStormEvents("03","17",03,16)
getStormEvents("03","18",03,17)
# getStormEvents("03","28",03,27)
# getStormEvents("04","04",04,03)
# getStormEvents("04","05",04,04)
# getStormEvents("04","12",04,11)
# getStormEvents("04","25",04,24)
# getStormEvents("04","29",04,28)
# getStormEvents("04","30",04,29)

