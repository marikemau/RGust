#Detect false positives

#gfs
#whole qeue

#libaries
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(Metrics)
#setwd
setwd("~/data/gfs/csv/")

pathgfs="~/data/gfs/csv/"



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
  pathgfs="~/data/gfs/csv/"
  setwd(pathgfs)
  l=list("00","03","06","09","12", "15", "18", "21", "24", "27", "30", "33", "36", "39", "42", "45", "48")
  l00=list("21", "24", "27", "30", "33", "36", "39", "42", "45")
  l06=list("15", "18", "21", "24", "27", "30", "33", "36", "39")
  l12=list("09","12", "15", "18", "21", "24", "27", "30", "33")
  l18=list("03","06","09","12", "15", "18", "21", "24", "27")
  if (grepl("00h",substr(pattern,12,14))){
    filenames=list()
    #only need the 24hours of the next day, so cutteed here the predictionhours
    for (i in l00){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    return(rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))})))
    
  }
  if (grepl("06h",substr(pattern,12,14))){
    filenames=list()
    for (i in l06){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    return(rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))})))
  }
  if (grepl("12h",substr(pattern,12,14))){
    filenames=list()
    for (i in l12){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    return(rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))})))
  }
  if (grepl("18h",substr(pattern,12,14))){
    filenames=list()
    for (i in l18){
      # filenames=list.files(path=path, pattern=paste(pattern,0,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    return(rbindlist(lapply(filenames, function(x){read.csv(x, header= F)})))
  }
  else("wrong")
  
  
  
}
#read.csv("gfs_2018012112h14.csv")

#gfs_test=multmerge(pathgfs,testi[4])

#Step2 
#croop coordinates by station coordinates
#used filter function for package dplyr
cutgfsCoordinates=function(gfs_file){
  #import station data
  setwd("~/data/DWD/station")
  coord_station_gfs=read.csv("wrf_hour_coords.csv")  
  #print(coord_station_gfs)
  gfs_new=data.frame(V1=factor(),V2=factor(),V5=numeric(), V6=numeric(),V7=numeric())
  n=nrow(coord_station_gfs)
  print("Step2")
  for (i in 1:n ){
    gfs=filter(gfs_file, (getElement(gfs_file,"V5")==coord_station_gfs$V5[i] & getElement(gfs_file,"V6")==coord_station_gfs$V6[i]))
    new=list(V1=gfs$V1, V2=gfs$V2, V5=gfs$V5 , V6=gfs$V6, V7=gfs$V7 )
    gfs_new=rbindlist(list(gfs_new,new),use.names = F, fill = F, idcol = F)
    # gfs_new=rbind(gfs_new,gfs)
    #gfs_c_t=merge(x=gfs_test,y=coord_station_gfs, by.x=c("V5","V6"), by.y=c("V5","V6"))
    
  }
  return(gfs_new)
}
# 
# gfs_c_t=merge(x=gfs_test,y=coord_station_gfs, by.x=c("V5","V6"), by.y=c("V5","V6"))
# #gfs_cutted=cutgfsCoordinates(gfs_test)
# station_data=station_data[station_data$V5 %in% storm_file$V5,]
# station_data=station_data[station_data$V6 %in% storm_file$V6,]
# station_data=filter()
# require(dplyr)
# x=filter(station_data,  station_data$Hours %in% storm_file$Hours)
# #storm=filter(gfs_cutted, gfs_cutted$V7>17.1 )

#Step3
#filter storm 17.2m/s
getStorm=function(gfs_file_cutted){
  print("Step3")
  
  storm=filter(gfs_file_cutted, gfs_file_cutted$V7>17.1 )
  
  return(storm)
}

# #storm=getStorm(gfs_cutted)
# #storm1=getStorm(gfs_c_t)
# 
# 
# 
# #storm1$Hours=as.POSIXct(storm1$V2)
# #storm1$Hours=storm1$Hours+2*60*60
# station_gfs_storm=merge(x=storm1,y=station_data, by.x=c("Hours"),by.y=c("Hours"))
# 
# 
# storm_new=data.frame(MESS_DATUM=factor(), V5=numeric(), V6=numeric(), StationID=numeric(), Stationshoehe=numeric(), FW=numeric(), Hours=factor())
# n=nrow(storm_file)
# for (i in 1:n){
#   f=filter(station_data, station_data$V5==storm_file$V5[i] &  station_data$V6==storm_file$V6[i]  &  station_data$Hours==storm_file$Hours[i])
#   new=list(MESS_DATUM=f$MESS_DATUM, V5=f$V5, V6=f$V6, StationID=f$STATIONS_ID ,Stationshoehe=f$Stationshoehe, FW=f$F, Hours=f$Hours)
#   storm_new=rbindlist(list(storm_new,new),use.names = F, fill = F, idcol = F)
# }
# 
# test=subset(station_data, station$V5)
#Step4

#compare to station data
compareStormToStation=function(storm_file){
  print("Step4")
  storm_file$Hours=as.POSIXct(storm_file$V2)
  storm_file$Hours=storm_file$Hours+2*60*60
  storm_file$date=as.Date(storm_file$Hours[1])
  date=storm_file$date[1]
  setwd("~/data/DWD/station")
  coord_station_gfs=read.csv("coord_station_gfs.csv") 
  station_data=read.csv("all_station_hour_with_data.csv")
  station_data$Hours=as.character(station_data$MESS_DATUM)
  station_data$Hours=as.POSIXct(station_data$Hours,format="%Y%m%d%H")
  station_data$MESS_DATUM=as.Date(station_data$Hours)
  station_data=station_data[station_data$MESS_DATUM %in% date,]
  station_data=merge(station_data,coord_station_gfs, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
  #station_data$Hours=as.POSIXct(as.character(station_data$MESS_DATUM),format="%Y%m%d%H")
  station_gfs_storm=merge(x=storm_file,y=station_data, by.x=c("V5","V6","Hours"),by.y=c("V5","V6","Hours"))
  station_gfs_storm=subset(station_gfs_storm, select=c("MESS_DATUM","Hours","STATIONS_ID","V5","V6","V7","F","Stationshoehe","geoBreite","geoLaenge"))
  
  
  return(station_gfs_storm)
}

#station_gfs_storm=compareStormToStation(storm)

#station_gfs_storm_file=station_gfs_storm

#Step5
getFalsePositive=function(station_gfs_storm_file){
  print("Step5")
  n=nrow(station_gfs_storm_file)
  for (i in 1:n){
    if (station_gfs_storm_file$V7[i] >= station_gfs_storm_file$F[i] && station_gfs_storm_file$F[i]< 17.2)
      (station_gfs_storm_file$FalsePositve[i]=T)
    else (station_gfs_storm_file$FalsePositve[i]=F)
    
  }
  return(station_gfs_storm_file)
  
}

#te=getFalsePositives(station_gfs_storm)


main=function(path,modelname,month,day){
  print(day)
  
  path=path
  fp=data.frame(MESS_DATUM=factor(),Hours=factor(), V5=numeric() ,V6=numeric(), V7=numeric(),StationID=numeric() ,Stationshoehe=numeric(), FW=numeric(), geoBreite=numeric(),geoLaenge=numeric(), FalsePositve=logical() )
  #   storm_new=rbindlist(list(storm_new,new),use.names = F, fill = F, idcol = F)
  path=path
  pattern=createpattern(modelname,month,day)
  for (i in pattern){
    
    h=getFalsePositive(compareStormToStation(getStorm(cutgfsCoordinates(multmerge(pathgfs,i)))))
    fp=rbindlist(list(fp,h),use.names = F, fill = F, idcol = F)
    
    
  }
  setwd("~/data/gfs/csv/pattern/falsepositives")
  if(day > 9){ 
    write.csv(fp, file=paste(modelname,"_","0",month,"_",day,"_", "fp",".csv",sep=""))}
  else {write.csv(fp, file=paste(modelname,"_", "0",month,"_", "0",day,"_","fp",".csv",sep=""))}
  setwd(pathgfs)
  return(fp)
}


main(pathgfs,"gfs",01,23)