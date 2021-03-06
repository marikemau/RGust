#merge data
#wind
#hourly/10min
path_dwd=("~/data/DWD/station")
pathcos="~/data/cosmo/csv/"
setwd(path_dwd)
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
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
  path_dwd=("~/data/DWD/station")
  filename=list.files(path = paste(path_dwd,"/hour/",sep=""), pattern ="produkt_zehn")
  pattern=paste("2018",m,d,sep="")
  #m="01"
  #d="18"
  for ( i in filename){
    setwd("~/data/DWD/station/hour")
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
  }
  
  
  #minvalues_jan=subset(minvalues,grepl(pattern,(substr(minvalues$MESS_DATUM1,1,8))))
  station_coords=read.table("/home/m_meij02/data/DWD/station/stationen_10min.txt",header=T)
  station_coords=station_coords[-1,]
  #stationid=station_coords$Stations_id
  station_coords$Stations_id=as.numeric(station_coords$Stations_id)
  print("getStorm")
  station_jan=merge(x=minvalues,y=station_coords, by.x="STATIONS_ID",by.y="Stations_id")
  print(station_jan[1])
  return(station_jan)
}
#stormday=getStormDayinHours("18","01")




#Step 1
#merge the needed prediction hours for each run 
#createpattern for filenameconditions 
createpattern= function(n,m,d){
  pattern=list()
  if (n == "cos") {
    run=list("00","06","12","18")
    
    if (d<10){
      for (i in 1:4){
        p= ((paste(n,"_20180",m,"0",d,run[i],"z_", sep="")))
        pattern=c(pattern,p)
      }}
    
    else{ 
      for (i in 1:4){
        p= ((paste(n,"_20180",m,d,run[i],"z_", sep="")))
        pattern=c(pattern,p)
      }
    }
    
    return(pattern)}
  
  
  
  else return ("wrong")
}
#testi=createpattern("cos",01,17)

#function to import all data into one list
#multmerge
multmerge = function(path,pattern){
  print("Step1")
  pathcos="~/data/cosmo/csv/"
  setwd(pathcos)
  
  if (grepl("00z",substr(pattern,13,15))){
    filenames=list()
    #only need the 24hours of the next day, so cutteed here the predictionhours
    for (i in 22:46){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))}))
    x$RUN="00"
    return(x)
    
  }
  if (grepl("06z",substr(pattern,13,15))){
    filenames=list()
    for (i in 16:39){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))}))
    x$RUN="06"
    return(x)
  }
  if (grepl("12z",substr(pattern,13,15))){
    filenames=list()
    for (i in 10:33){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    print(filenames)
    x=rbindlist(lapply(filenames, function(x){read.csv(x, header= F)}))
    x$RUN="12"
    return(x)
  }
  if (grepl("18z",substr(pattern,13,15))){
    filenames=list()
    
    for (i in 4:27){
      # filenames=list.files(path=path, pattern=paste(pattern,0,i,".csv")
      if( i<10){
        filenames=rbind(filenames,paste(pattern,"0",i,".csv",sep="")) 
        
      }
      
      else{ 
        filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))
        
      }
      # print(filenames)
      
    }
    #filenames=rbind(filenames1,filenames2)
    print(filenames[1])
    
    #xx=try(rbindlist(lapply(filenames, function(x){read.csv(x, header= F)})))
    xx=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F),error=function(e) NULL)}))
    
    xx$RUN="18"
    return(xx)
  }
  
}
#l=read.csv("cos_2018012018z_05.csv")

#cos_test=multmerge(pathcos,testi[1])

#Step2 
#croop coordinates by station coordinates
#used filter function for package dplyr
cutcosCoordinates=function(cos_file){
  if(is.data.frame(cos_file)){
    #import station data
    setwd("~/data/DWD/station")
    coord_station_cos=read.csv("coord_station_wrf_10min.csv")  
    #print(coord_station_cos)
    cos_new=data.frame(V1=factor(),V2=factor(),V5=numeric(), V6=numeric(),V7=numeric(),RUN=factor())
    n=nrow(coord_station_cos)
    print("Step2")
    for (i in 1:n ){
      cos=filter(cos_file, (getElement(cos_file,"V5")==coord_station_cos$V5[i] & getElement(cos_file,"V6")==coord_station_cos$V6[i]))
      new=list(V1=cos$V1, V2=cos$V2, V5=cos$V5 , V6=cos$V6, V7=cos$V7, RUN=cos$RUN )
      cos_new=rbindlist(list(cos_new,new),use.names = F, fill = F, idcol = F)
      # cos_new=rbind(cos_new,cos)
      
    }
    print(cos_new[1])
    return(cos_new)
  }
  
  else{
    print("cos_file empty")
    return("empty")}
}


#cos_cutted2=cutcosCoordinates(cos_test)
###########################################################################
#
###########################################################################
#station_file=stormday
#model_file=cos_cutted2
mergeStationModel=function(model_file,station_file){
  print("h")
  model_file$timestamp=as.POSIXct(model_file$V2)
  model_file$timestamp=model_file$timestamp+60*60*2
  setwd("~/data/DWD/station")
  coord_station_cos=read.csv("coord_station_wrf_10min.csv") 
  station_file$geoBreite=as.numeric(levels(station_file$geoBreite))[station_file$geoBreite]
  station_file$geoLaenge=as.numeric(levels(station_file$geoLaenge))[station_file$geoLaenge]
  
  station_data=merge(station_file,coord_station_cos, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
  print(station_data[1])
  station_model_file=merge(x=model_file,y=station_data, by.x=c("V5","V6","timestamp"),by.y=c("V5","V6","timestamp2"))
  
  return(station_model_file)
}


#station_model_merged2=mergeStationModel(cos_cutted2,station_jan)


getStormEvents=function(month,day,fmonth,fday){
  cos_storm=NULL
  stormday=getStormDayinHours(day,month)
  pattern=createpattern("cos",fmonth,fday)
  for (i in pattern){
    print(i)
    h=cutcosCoordinates(multmerge(pathcos,i))
    storm=mergeStationModel(h,stormday)
    cos_storm=rbindlist(list(cos_storm,storm),use.names = F, fill = F, idcol = F)
  }
  
  write.csv(cos_storm, paste("/home/m_meij02/data/cosmo/csv/pattern/stormcos/stormcos_",month,day,".csv",sep=""))
}

getStormEvents("01","18",01,17)
