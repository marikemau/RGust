#cos
#whole qeue

#libaries
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(Metrics)
#setwd
setwd("~/data/cosmo/csv/")

pathcos="~/data/cosmo/csv/"



#Step 1
#merge the needed prediction hours for each run 
#createpattern for filenameconditions 
createpattern= function(n,m,d){
  pattern=list()
  if (n == "cos") {
    run=list("00","06","12","18")
    
    if (d<10){
      for (i in 1:4){
        p= ((paste(n,"_20180",m,"0",d,run[i],"z_0", sep="")))
        pattern=c(pattern,p)
      }}
    
    else{ 
      for (i in 1:4){
        p= ((paste(n,"_20180",m,d,run[i],"z_0", sep="")))
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
  pathwrf="~/data/cosmo/csv/"
  setwd(pathwrf)
  
  if (grepl("00z",substr(pattern,13,15))){
    filenames=list()
    #only need the 24hours of the next day, so cutteed here the predictionhours
    for (i in 22:27){
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
    for (i in 16:27){
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
    for (i in 10:27){
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
#read.csv("cos_2018012018z_14.csv")

#cos_test=multmerge(pathcos,testi[1])


#Step2 
#croop coordinates by station coordinates
#used filter function for package dplyr
# cutcosCoordinates=function(cos_file){
#   #import station data
#   setwd("~/data/DWD/station")
#   coord_station_cos=read.csv("coord_station_cos.csv")  
#   #print(coord_station_cos)
#   cos_new=data.frame(V1=factor(),V2=factor(),V5=numeric(), V6=numeric(),V7=numeric())
#   n=nrow(coord_station_cos)
#   print("Step2")
#   for (i in 1:n ){
#     cos=filter(cos_file, (getElement(cos_file,"V5")==coord_station_cos$V5[i] & getElement(cos_file,"V6")==coord_station_cos$V6[i]))
#     new=list(V1=cos$V1, V2=cos$V2, V5=cos$V5 , V6=cos$V6, V7=cos$V7 )
#     cos_new=rbindlist(list(cos_new,new),use.names = F, fill = F, idcol = F)
#     # cos_new=rbind(cos_new,cos)
#     
#     
#   }
#   return(cos_new)
# }

cutCosCoordinates=function(cos_test_file){
  if(is.data.frame(cos_test_file)){
    #import station data
    setwd("~/data/DWD/station")
    coord_station_wrf=read.csv("coord_station_wrf.csv")  
    #print(coord_station_wrf)
    cos_new=data.frame(V1=factor(),V2=factor(),V5=numeric(), V6=numeric(),V7=numeric(),RUN=factor())
    n=nrow(coord_station_wrf)
    print("Step2")
    for (i in 1:n ){
      cos=filter(cos_test_file, (getElement(cos_test_file,"V5")==coord_station_wrf$V5[i] & getElement(cos_test_file,"V6")==coord_station_wrf$V6[i]))
      new=list(V1=cos$V1, V2=cos$V2, V5=cos$V5 , V6=cos$V6, V7=cos$V7, RUN=cos$RUN )
      cos_new=rbindlist(list(cos_new,new),use.names = F, fill = F, idcol = F)
      # wrf_new=rbind(wrf_new,wrf)
      
    }
    print(cos_new[1])
    return(cos_new)
  }
  
  else{
    print("cos_file empty")
    return("empty")}
}


#cos_cutted=cutCosCoordinates(cos_test)



#Step3 
#Get means per Day for all runs 
getMeanPerDay=function(cos_file_cutted){
  #new dataframe
  #cos_new=data.frame(V1=factor(),Messdatum=factor() ,V5=numeric(), V6=numeric(),meancos=numeric(),geoBreite=numeric(), geoLaenge=numeric(),meanstation=numeric(),RMSE=numeric(),ST_ID=numeric(),ST_hoehe=numeric(),RUN=factor())
  #hours and date transform
  cos_file_cutted$Hours=as.POSIXct(cos_file_cutted$V2)
  cos_file_cutted$Hours=cos_file_cutted$Hours+2*60*60
  cos_file_cutted$Predate=as.Date(cos_file_cutted$Hours[1])
  
  #coordinates and station data
  setwd("~/data/DWD/station")
  coord_station_wrf=read.csv("coord_station_wrf.csv") 
  station_data=read.csv("all/all_station_neu.csv")
  stationid=station_data$STATIONS_ID
  stationid=stationid[!duplicated(stationid)]
  station_data$MESS_DATUM=as.Date(as.character(station_data$MESS_DATUM), format="%Y%m%d")
  #filter station data by date 
  station_data=filter(station_data, MESS_DATUM==cos_file_cutted$Predate[1])
  
  #merge station data with the coordinates of cos
  station_data=merge(station_data,coord_station_wrf, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
  merged_station_cos=merge(station_data,cos_file_cutted,  by.x=c("V5","V6"),by.y=c("V5","V6"))
  merged_station_cos=subset(merged_station_cos,select=c("MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7","FX","Stationshoehe","RUN","Predate","Hours"))
  #merged_station_cos=subset(merged_station_cos,select=c("MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7","FX","Stationshoehe","RUN","Predate","Hours"))
  #merged_station_cos=merged_station_cos[!duplicated(merged_station_cos),]
  print(merged_station_cos[1])
  merged_station_cos=subset(merged_station_cos, merged_station_cos$FX!=-999 )
 
  
  stationid=merged_station_cos$STATIONS_ID
  stationid=stationid[!duplicated(stationid)]
  cos_new=NULL
  for (i in stationid ){
    m=filter(merged_station_cos, merged_station_cos$STATIONS_ID==i)
    #new=list(MESS_DATUM=m$MESS_DATUM[1],V1=m$V1[1],V2=m$V2[1],STATIONS_ID=m$STATIONS_ID[1],V5=m$V5[1],V6=m$V6[1],geoLaenge=m$geoLaenge[1],geoBreite=m$geoBreit[1], V7_mean=mean(m$V7),FM_mean=mean(m$FM),FX_mean=mean(m$FX),RMSE_daily=rmse(mean(m$FX),mean(m$V7)),Stationshoehe=m$Stationshoehe[1],RUN=m$RUN[1],Predate=m$Predate[1],Hours=m$Hours[1])
    new=list(MESS_DATUM=m$MESS_DATUM[1],V1=m$V1[1],V2=m$V2[1],STATIONS_ID=m$STATIONS_ID[1],V5=m$V5[1],V6=m$V6[1],geoLaenge=m$geoLaenge[1],geoBreite=m$geoBreit[1], V7_MAX_DAY=max(m$V7)[1],FX=max(m$FX)[1],RMSE_daily=rmse(max(m$FX),max(m$V7)),Stationshoehe=m$Stationshoehe[1],RUN=m$RUN[1],Predate=m$Predate[1],Hours=m$Hours[1])
    #print(station)
    #print(station)
    
    #new=list(V1=cos$V1[1], V5=cos$V5[1] , V6=cos$V6[1], meancos=mean(cos$V7), geoBreite=coord_station_cos$station_y[i], geoLaenge=coord_station_cos$station_x[i],meanstation=mean(station$FM),RMSE=rmse((mean(cos$V7)),(mean(station$FM))) )
    cos_new=rbindlist(list(cos_new,new),use.names = F, fill = F, idcol = F)
    #   # cos_new=rbind(cos_new,cos)
    
    
  }
  
 # cos_new=subset(cos_new, cos_new$FX!=-999 )
  print(cos_new[1])
  return(cos_new)
}
#cos_cutted_means=getMeanPerDay(cos_cutted)

#cos_cutted_means_file=cos_cutted_means
getAccuracy=function(cos_cutted_means_file){
  require(Metrics)
  #cos_cutted_means_file=subset(cos_cutted_means_file, select=c("Predate","MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7_mean","FM_mean","FX_mean","RMSE_daily","Stationshoehe","RUN"))
  #acc_new=data.frame(Date=as.character(cos_cutted_means_file$Predate[1]),RUN=cos_cutted_means_file$RUN[1],Meancos=mean(cos_cutted_means_file$V7_mean)[1], MeanStation=mean(cos_cutted_means_file$FX_mean)[1], BIAS=bias(cos_cutted_means_file$FX_mean,cos_cutted_means_file$V7_mean)[1],RMSE=rmse(cos_cutted_means_file$FX_mean,cos_cutted_means_file$V7_mean)[1],Accuracy=accuracy(cos_cutted_means_file$FX_mean,cos_cutted_means_file$V7_mean)[1],ST_DEV=sd(cos_cutted_means_file$FX_mean,cos_cutted_means_file$V7_mean)[1])
  cos_cutted_means_file=subset(cos_cutted_means_file, select=c("Predate","MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7_MAX_DAY","FX","RMSE_daily","Stationshoehe","RUN"))
  acc_new=data.frame(Date=as.character(cos_cutted_means_file$Predate[1]),RUN=cos_cutted_means_file$RUN[1],Mean_cos=mean(cos_cutted_means_file$V7_MAX_DAY)[1], MAX_cos=max(cos_cutted_means_file$V7_MAX_DAY)[1],MeanStation=mean(cos_cutted_means_file$FX)[1],MAX_STATION=max(cos_cutted_means_file$FX)[1], BIAS=bias(cos_cutted_means_file$FX,max(cos_cutted_means_file$V7_MAX_DAY))[1],RMSE=rmse(cos_cutted_means_file$FX,cos_cutted_means_file$V7_MAX_DAY),Accuracy=accuracy(cos_cutted_means_file$FX,cos_cutted_means_file$V7_MAX_DAY))
  
  accu=read.csv("/home/m_meij02/data/cosmo/csv/pattern/accuracy_cos.csv")
  accu$X=NULL
  acc_new=rbindlist(list(accu,acc_new),use.names = F,fill=F,idcol=F)
  write.csv(acc_new,"/home/m_meij02/data/cosmo/csv/pattern/accuracy_cos.csv")
  
  return(cos_cutted_means_file)
}

#acc_cos=getAccuracy(cos_cutted_means)

#main function consist of aheads function

#values per day


getAccuracyPerDay=function(path,modelname,month,day){
  path=path
  pattern=createpattern(modelname,month,day)
  cos_new=NULL
  for (i in pattern){
    print(i)
    
    h=getAccuracy(getMeanPerDay(cutCosCoordinates(multmerge(pathcos,i))))
    cos_new=rbindlist(list(cos_new,h),use.names = F, fill = F, idcol = F)
  }
  
  #return(cos_new)
  
  
  setwd("~/data/cosmo/csv/pattern/cosmoperday")
  if(is.data.frame(cos_new) && nrow(cos_new)>0){
    if(day > 9){ 
      write.csv(cos_new, file=paste(modelname,"_","0",month,"_",day,"_", "acc",".csv",sep=""))}
    else {write.csv(cos_new, file=paste(modelname,"_", "0",month,"_", "0",day,"_","acc",".csv",sep=""))}
  }
  setwd(pathcos)
  
}



#getAccuracyPerDay(pathcos,"cos",01,17)


for(i in 4:4){
  for (j in 11:31){
    
    testdate=createpattern("cos",i,j)
    # if(testdate[4]!="wrf_2018012018z_"){
    
    l=list.files(path=pathcos, pattern=paste(testdate[1],"*",sep=""))
    if(!is.na(l[1])){
      print("date input exist")
      
      if(j<10){
        setwd("~/data/cosmo/csv/pattern/cosmoperday")
        if(!file.exists(paste("cos_0",i,"_0",j,"_acc.csv",sep=""))){
          #print(j)
          getAccuracyPerDay(pathcos,"cos",i,j)
        }
        else{print(paste("cos",i,j, "exist"))}
      }
      else
        if(j>9){
          setwd("~/data/cosmo/csv/pattern/cosmoperday")
          if(!file.exists(paste("cos_0",i,"_",j,"_acc.csv",sep=""))){
            getAccuracyPerDay(pathcos,"cos",i,j)
          }
          else{print(paste("cos",i,j, "exist"))}
        }
      
    }
    
    else{next}
    
    # }
    # else{next}
  }
}

# for (i in 1:1){
#   for (j in 17:18){
#     
#     setwd("~/data/cosmo/csv/pattern/cosmoperday2")
#     if(j<10){
#       {if(!file.exists(paste("cos_0",i,"_0",j,"_acc.csv",sep=""))){
#         print(j)
#         getAccuracyPerDay(pathcos,"cos",i,j)
#       }
#         else(print(paste("cos",i,j, "exist")))
#       }
#     }
#     else{ {if(!file.exists(paste("cos_0",i,"_",j,"_acc.csv",sep=""))){
#       print(j)
#       getAccuracyPerDay(pathcos,"cos",i,j)
#     }
#       else(print(paste("cos",i,j, "exist")))
#     }}
#     #cos_2018011700z_028.csv
#     
#   }
# }

# for (i in 1:1){
#   for (j in 17:18){
#     
#     getAccuracyPerDay(pathcos,"cos",i,j)
#   }}
