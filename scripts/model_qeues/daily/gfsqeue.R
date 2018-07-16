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
#read.csv("gfs_2018012112h14.csv")

#gfs_test=multmerge(pathgfs,testi[1])
#gfs_test2=multmerge(pathgfs,testi[2])
#gfs_test3=multmerge(pathgfs,testi[3])
#gfs_test4=multmerge(pathgfs,testi[4])


# #Step2 
#croop coordinates by station coordinates
#used filter function for package dplyr
cutgfsCoordinates=function(gfs_file){
  #import station data
  setwd("~/data/DWD/station")
  coord_station_gfs=read.csv("coord_station_gfs.csv")
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
  return(gfs_new)
}

#gfs_cutted=cutgfsCoordinates(gfs_test)

# 
# cutWRFCoordinates=function(wrf_file){
#   if(is.data.frame(wrf_file)){
#     #import station data
#     setwd("~/data/DWD/station")
#     coord_station_wrf=read.csv("coord_station_wrf.csv")  
#     #print(coord_station_wrf)
#     wrf_new=data.frame(V1=factor(),V2=factor(),V5=numeric(), V6=numeric(),V7=numeric(),RUN=factor())
#     n=nrow(coord_station_wrf)
#     print("Step2")
#     for (i in 1:n ){
#       wrf=filter(wrf_file, (getElement(wrf_file,"V5")==coord_station_wrf$V5[i] & getElement(wrf_file,"V6")==coord_station_wrf$V6[i]))
#       new=list(V1=wrf$V1, V2=wrf$V2, V5=wrf$V5 , V6=wrf$V6, V7=wrf$V7, RUN=wrf$RUN )
#       wrf_new=rbindlist(list(wrf_new,new),use.names = F, fill = F, idcol = F)
#       # wrf_new=rbind(wrf_new,wrf)
#       
#     }
#     print(wrf_new[1])
#     return(wrf_new)
#   }
#   
#   else{
#     print("wrf_file empty")
#     return("empty")}
# }


#Step3 
#Get means per Day for all runs 
# getMeanPerDay=function(gfs_file_cutted){
#   gfs_new=data.frame(V1=factor(),Messdatum=factor(),V5=numeric(), V6=numeric(),meangfs=numeric(),geoBreite=numeric(), geoLaenge=numeric(),meanstation=numeric(),RMSE=numeric(),ST_ID=numeric(),ST_hoehe=numeric())
#   setwd("~/data/DWD/station")
#   coord_station_gfs=read.csv("coord_station_gfs.csv") 
#   gfs_file_cutted$V2=as.POSIXct(gfs_file_cutted$V2)
#   setwd("/home/m_meij02/data/DWD/station")
#   station_data=read.csv("all_station_per_day_with_data.csv") 
#   stationid=station_data$STATIONS_ID
#   stationid=stationid[!duplicated(stationid)]
#   station_data$MESS_DATUM=as.Date(as.character(station_data$MESS_DATUM), format="%Y%m%d")
#   station_data=filter(station_data, MESS_DATUM==as.Date(getElement(gfs_file_cutted,"V2")[1]))
#   print(station_data$MESS_DATUM)
#   station_data=merge(station_data,coord_station_gfs, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
#   merged_station_gfs=merge(station_data,gfs_file_cutted,  by.x=c("V5","V6"),by.y=c("V5","V6"))
#   merged_station_gfs=subset(merged_station_gfs,select=c("MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7","FM","Stationshoehe"))
#   merged_station_gfs=merged_station_gfs[!duplicated(merged_station_gfs),]
#   print("Step3")
#   for (i in stationid ){
#     #gfs=filter(gfs_file_cutted, (getElement(gfs_file_cutted,"V5")==coord_station_gfs$V5[i] & getElement(gfs_file_cutted,"V6")==coord_station_gfs$V6[i]))
#     #station=filter(station_data, station_data$geoBreite==coord_station_gfs$station_y[i] & station_data$geoLaenge==coord_station_gfs$station_x[i])
#     m=filter(merged_station_gfs, merged_station_gfs$STATIONS_ID==i)
#     new=list( V1=m$V1[1],  Messdatum=as.character(m$V2[1]), V5=m$V5[1], V6=m$V6[1], meangfs=mean(m$V7), geoBreite=m$geoBreite[1], geoLaenge=m$geoLaenge[1], meanstation=mean(m$FM), RMSE=rmse(mean(m$V7),mean(m$FM)), ST_ID=m$STATIONS_ID[1], ST_hoehe=m$Stationshoehe[1])
#     #print(station)
#     #new=list(V1=gfs$V1[1], V5=gfs$V5[1] , V6=gfs$V6[1], meangfs=mean(gfs$V7), geoBreite=coord_station_gfs$station_y[i], geoLaenge=coord_station_gfs$station_x[i],meanstation=mean(station$FM),RMSE=rmse((mean(gfs$V7)),(mean(station$FM))) )
#     gfs_new=rbindlist(list(gfs_new,new),use.names = F, fill = F, idcol = F)
#     # gfs_new=rbind(gfs_new,gfs)
#     
#     
#   }
#   
#   
#   return(gfs_new)
# }
#gfs_cutted_means=getMeanPerDay(gfs_cutted)
getMeanPerDay=function(gfs_file_cutted){
  #new dataframe
  #gfs_new=data.frame(V1=factor(),Messdatum=factor() ,V5=numeric(), V6=numeric(),meangfs=numeric(),geoBreite=numeric(), geoLaenge=numeric(),meanstation=numeric(),RMSE=numeric(),ST_ID=numeric(),ST_hoehe=numeric(),RUN=factor())
  #hours and date transform
  gfs_file_cutted$Hours=as.POSIXct(gfs_file_cutted$V2)
  gfs_file_cutted$Hours=gfs_file_cutted$Hours+2*60*60
  gfs_file_cutted$Predate=as.Date(gfs_file_cutted$Hours[1])
  
  #coordinates and station data
  setwd("~/data/DWD/station")
  coord_station_gfs=read.csv("coord_station_wrf.csv") 
  station_data=read.csv("all/all_station_neu.csv")
  stationid=station_data$STATIONS_ID
  stationid=stationid[!duplicated(stationid)]
  station_data$MESS_DATUM=as.Date(as.character(station_data$MESS_DATUM), format="%Y%m%d")
  #filter station data by date 
  station_data=filter(station_data, MESS_DATUM==gfs_file_cutted$Predate[1])
  
  #merge station data with the coordinates of gfs
  station_data=merge(station_data,coord_station_gfs, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
  merged_station_gfs=merge(station_data,gfs_file_cutted,  by.x=c("V5","V6"),by.y=c("V5","V6"))
  merged_station_gfs=subset(merged_station_gfs,select=c("MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7","FX","Stationshoehe","RUN","Predate","Hours"))
  #merged_station_gfs=merged_station_gfs[!duplicated(merged_station_gfs),]
  print(merged_station_gfs[1])
  merged_station_gfs=subset(merged_station_gfs, merged_station_gfs$FX!=-999 )
 
  gfs_new=NULL
  stationid=merged_station_gfs$STATIONS_ID
  stationid=stationid[!duplicated(stationid)]
  for (i in stationid ){
    m=filter(merged_station_gfs, merged_station_gfs$STATIONS_ID==i)
    new=list(MESS_DATUM=m$MESS_DATUM[1],V1=m$V1[1],V2=m$V2[1],STATIONS_ID=m$STATIONS_ID[1],V5=m$V5[1],V6=m$V6[1],geoLaenge=m$geoLaenge[1],geoBreite=m$geoBreit[1], V7_MAX_DAY=max(m$V7)[1],FX=max(m$FX)[1],RMSE_daily=rmse(max(m$FX),max(m$V7)),Stationshoehe=m$Stationshoehe[1],RUN=m$RUN[1],Predate=m$Predate[1],Hours=m$Hours[1])
    #print(station)
 
  
    #new=list(MESS_DATUM=m$MESS_DATUM[1],V1=m$V1[1],V2=m$V2[1],STATIONS_ID=m$STATIONS_ID[1],V5=m$V5[1],V6=m$V6[1],geoLaenge=m$geoLaenge[1],geoBreite=m$geoBreit[1], V7_mean=mean(m$V7),FM_mean=mean(m$FM),FX_mean=mean(m$FX),RMSE_daily=rmse(mean(m$FX),mean(m$V7)),Stationshoehe=m$Stationshoehe[1],RUN=m$RUN[1],Predate=m$Predate[1],Hours=m$Hours[1])
    #print(station)
    
    #new=list(V1=gfs$V1[1], V5=gfs$V5[1] , V6=gfs$V6[1], meangfs=mean(gfs$V7), geoBreite=coord_station_gfs$station_y[i], geoLaenge=coord_station_gfs$station_x[i],meanstation=mean(station$FM),RMSE=rmse((mean(gfs$V7)),(mean(station$FM))) )
    gfs_new=rbindlist(list(gfs_new,new),use.names = F, fill = F, idcol = F)
    #   # gfs_new=rbind(gfs_new,gfs)
    
    
  }
  
  #gfs_new=subset(gfs_new, gfs_new$FX!=-999 )
  print(gfs_new[1])
  return(gfs_new)
}

#gfs_cutted_means=getMeanPerDay(gfs_cutted)
#gfs_cutted_means_file=gfs_cutted_means

getAccuracy=function(gfs_cutted_means_file){
  require(Metrics)
  gfs_cutted_means_file=subset(gfs_cutted_means_file, select=c("Predate","MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7_MAX_DAY","FX","RMSE_daily","Stationshoehe","RUN"))
  
  acc_new=data.frame(Date=as.character(gfs_cutted_means_file$Predate[1]),RUN=gfs_cutted_means_file$RUN[1],Mean_gfs=mean(gfs_cutted_means_file$V7_MAX_DAY)[1], MAX_gfs=max(gfs_cutted_means_file$V7_MAX_DAY)[1],MeanStation=mean(gfs_cutted_means_file$FX)[1],MAX_STATION=max(gfs_cutted_means_file$FX)[1], BIAS=bias(gfs_cutted_means_file$FX,gfs_cutted_means_file$V7_MAX_DAY)[1],RMSE_allvalues=rmse(gfs_cutted_means_file$FX,gfs_cutted_means_file$V7_MAX_DAY)[1],Accuracy=accuracy(gfs_cutted_means_file$FX,gfs_cutted_means_file$V7_MAX_DAY)[1])
  accu=read.csv("/home/m_meij02/data/gfs/csv/pattern/accuracy_gfs.csv")
  accu$X=NULL
  acc_new=rbindlist(list(accu,acc_new),use.names = F,fill=F,idcol=F)
  write.csv(acc_new,"/home/m_meij02/data/gfs/csv/pattern/accuracy_gfs.csv")
  
  return(gfs_cutted_means_file)
}
#te=getAccuracy(gfs_cutted_means_file)

#main function consist of aheads function

#values per day
# 
# getAccuracyPerDay=function(path,modelname,month,day){
#   gfs_new=data.frame(V1=factor(),Messdatum=factor(),V5=numeric(), V6=numeric(),meangfs=numeric(),geoBreite=numeric(), geoLaenge=numeric(),meanstation=numeric(),RMSE=numeric(),ST_ID=numeric(),ST_hoehe=numeric())
#   path=path
#   pattern=createpattern(modelname,month,day)
#   for (i in pattern){
#     
#     h=getMeanPerDay(cutgfsCoordinates(multmerge(pathgfs,i)))
#     gfs_new=rbindlist(list(gfs_new,h),use.names = F, fill = F, idcol = F)
#     
#     
#   }
#   gfs_new=subset(gfs_new, gfs_new$meanstation!=-999)
#   setwd("~/data/gfs/csv/pattern/gfsperday2")
#   if(day > 9){ 
#     write.csv(gfs_new, file=paste(modelname,"_","0",month,"_",day,"_", "acc",".csv",sep=""))}
#   else {write.csv(gfs_new, file=paste(modelname,"_", "0",month,"_", "0",day,"_","acc",".csv",sep=""))}
#   setwd(pathgfs)
#   
# }

getAccuracyPerDay=function(path,modelname,month,day){
  path=path
  pattern=createpattern(modelname,month,day)
  gfs_new=NULL
  for (i in pattern){
    print(i)
    
    h=getAccuracy(getMeanPerDay(cutgfsCoordinates(multmerge(pathgfs,i))))
    gfs_new=rbindlist(list(gfs_new,h),use.names = F, fill = F, idcol = F)
  }
  
  #return(gfs_new)
  
  
  setwd("~/data/gfs/csv/pattern/gfsperday")
  if(is.data.frame(gfs_new) && nrow(gfs_new)>0){
    if(day > 9){ 
      write.csv(gfs_new, file=paste(modelname,"_","0",month,"_",day,"_", "acc",".csv",sep=""))}
    else {write.csv(gfs_new, file=paste(modelname,"_", "0",month,"_", "0",day,"_","acc",".csv",sep=""))}
  }
  setwd(pathgfs)
  
}

#getAccuracyPerDay(pathgfs,"gfs",01,17)


for(i in 4:4){
  for (j in 24:31){
    
    testdate=createpattern("gfs",i,j)
    # if(testdate[4]!="gfs_2018012018z_"){
    
    l=list.files(path=pathgfs, pattern=paste(testdate[1],"*",sep=""))
    if(!is.na(l[1])){
      print("date input exist")
      
      if(j<10){
        setwd("~/data/gfs/csv/pattern/gfsperday")
        if(!file.exists(paste("gfs_0",i,"_0",j,"_acc.csv",sep=""))){
          #print(j)
          getAccuracyPerDay(pathgfs,"gfs",i,j)
        }
        else{print(paste("gfs",i,j, "exist"))}
      }
      else
        if(j>9){
          setwd("~/data/gfs/csv/pattern/gfsperday")
          if(!file.exists(paste("gfs_0",i,"_",j,"_acc.csv",sep=""))){
            getAccuracyPerDay(pathgfs,"gfs",i,j)
          }
          else{print(paste("gfs",i,j, "exist"))}
        }
      
    }
    
    else{next}
    
    # }
    # else{next}
  }
}

# for (i in 1:1){
#   for (j in 17:31){
#     
#     setwd("~/data/gfs/csv/pattern/gfsperday2")
#     if(!file.exists(paste("gfs",i,j,"acc.csv",sep="_"))){
#       print(j)
#       getAccuracyPerDay(pathgfs,"gfs",i,j)
#     }
#     else (print(paste("gfs",i,j, "exist")))
#     #
#     
#   }
# }
