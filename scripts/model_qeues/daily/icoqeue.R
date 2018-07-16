#Script icon-whole qeue
#Input: Station Data and Icon csv files
#Output: Accuracy file for each day 

#libaries
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(Metrics)
#setwd
setwd("~/data/icon/csv/")
pathico="~/data/icon/csv/"



#Step 1
#merge the needed prediction hours for each run 
#createpattern for filenameconditions 
createpattern= function(n,m,d){
  pattern=list()
  if (n == "ico") {
    run=list("00","06","18")
    
    if (d<10){
      for (i in 1:3){
        p= ((paste(n,"_20180",m,"0",d,run[i],"z_0", sep="")))
        pattern=c(pattern,p)
      }}
    
    else{ 
      for (i in 1:3){
        p= ((paste(n,"_20180",m,d,run[i],"z_0", sep="")))
        pattern=c(pattern,p)
      }
    }
    
    return(pattern)}
  
  
  
  else return ("wrong")
}
pattern_ico=createpattern("ico",01,17)


#multmerge
#function to import all csv.-data into one list
multmerge = function(path,pattern){
  
  setwd(pathico)
  if (grepl("00z",substr(pattern,13,15))){
    filenames=list()
    #only need the 24hours of the next day, so cutted here the predictionhours
    for (i in 22:46){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,"_7km.csv",sep=""))}
    #rbindlist( read.csv(x, header= F))}
    #return(rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))})))
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))}))
    x$RUN="00"
    return(x)
  }
  if (grepl("06z",substr(pattern,13,15))){
    filenames=list()
    for (i in 16:39){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,"_7km.csv",sep=""))}
   
    #rbindlist( read.csv(x, header= F))}
    #return(rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))})))
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))}))
    x$RUN="06"
    return(x)
  }
  # if (grepl("12z",pattern)){
  #   filename=list()
  #   for (i in 10:33){
  #     # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
  #     
  #   filename=rbind(filename,paste(pattern,i,"_7km.csv",sep=""))}
  #   print(filename)
  #   #rbindlist( read.csv(x, header= F))}
  #   test=rbindlist(lapply(filename, function(x){tryCatch(read.csv(x, header= F))}))
  #   print(test)
  #   return(test)
  # }
  if (grepl("18z",substr(pattern,13,15))){
    filenames=list()
    for (i in 4:27){
      # filenames=list.files(path=path, pattern=paste(pattern,0,i,".csv")
      if( i<10){
        filenames=rbind(filenames,paste(pattern,"0",i,"_7km.csv",sep=""))}
      else
        filenames=rbind(filenames,paste(pattern,i,"_7km.csv",sep=""))}
    #print(filenames)
    #return(rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))})))
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F))}))
    x$RUN="18"
    return(x)
  }
  
  
  
}

# ico_merged1=multmerge(pathico,pattern_ico[1])
# ico_merged2=multmerge(pathico,pattern_ico[2])
# ico_merged3=multmerge(pathico,pattern_ico[3])
# ico_merged4=multmerge(pathico,pattern_ico[4])



#Step2 
#croop coordinates by station coordinates
#used filter function of package dplyr
cuticoCoordinates=function(ico_file){
  if(is.data.frame(ico_file)){
    #import station data
    setwd("~/data/DWD/station")
    coord_station_ico=read.csv("coord_station_wrf.csv")  
    #print(coord_station_wrf)
    ico_new=data.frame(V1=factor(),V2=factor(),V5=numeric(), V6=numeric(),V7=numeric(),RUN=factor())
    n=nrow(coord_station_ico)
    print("Step2")
    for (i in 1:n ){
      ico=filter(ico_file, (getElement(ico_file,"V5")==coord_station_ico$V5[i] & getElement(ico_file,"V6")==coord_station_ico$V6[i]))
      new=list(V1=ico$V1, V2=ico$V2, V5=ico$V5 , V6=ico$V6, V7=ico$V7, RUN=ico$RUN )
      ico_new=rbindlist(list(ico_new,new),use.names = F, fill = F, idcol = F)
      # ico_new=rbind(ico_new,ico)
      
    }
    print(ico_new[1])
    return(ico_new)
  }
  
  else{
    print("ico_file empty")
    return("empty")}
}

# #ico_cutted=cuticoCoordinates(ico_merged)
# ico_cutted1=cuticoCoordinates(ico_merged1)
# ico_cutted2=cuticoCoordinates(ico_merged2)
# ico_cutted3=cuticoCoordinates(ico_merged3)
# ico_cutted4=cuticoCoordinates(ico_merged4)
# 


#Step3 
#Get RMSE and mean per Day for all runs 
# getMeanPerDay=function(ico_file_cutted){
#   ico_new=data.frame(V1=factor(),Messdatum=factor() ,V5=numeric(), V6=numeric(),meanico=numeric(),geoBreite=numeric(), geoLaenge=numeric(),meanstation=numeric(),RMSE=numeric(),ST_ID=numeric(),ST_hoehe=numeric())
#   setwd("~/data/DWD/station")
#   coord_station_ico=read.csv("coord_station_ico.csv") 
#   ico_file_cutted$V2=as.POSIXct(ico_file_cutted$V2)
#   setwd("/home/m_meij02/data/DWD/station")
#   station_data=read.csv("all_station_per_day_with_data.csv")
#   stationid=station_data$STATIONS_ID
#   stationid=stationid[!duplicated(stationid)]
#   station_data$MESS_DATUM=as.Date(as.character(station_data$MESS_DATUM), format="%Y%m%d")
#   station_data=filter(station_data, MESS_DATUM==as.Date(getElement(ico_file_cutted,"V2")[1]))
#   print(station_data$MESS_DATUM)
#   station_data=merge(station_data,coord_station_ico, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
#   mergeddd=merge(station_data,ico_file_cutted,  by.x=c("V5","V6"),by.y=c("V5","V6"))
#   mergeddd=subset(mergeddd,select=c("MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7","FM","Stationshoehe"))
#   mergeddd=mergeddd[!duplicated(mergeddd),]
# 
#   print("Step3")
#   for (i in stationid){
#     #ico=filter(ico_file_cutted, (getElement(ico_file_cutted,"V5")==coord_station_ico$V5[i] & getElement(ico_file_cutted,"V6")==coord_station_ico$V6[i]))
#     #station=filter(station_data, station_data$geoBreite==coord_station_ico$station_y[i] & station_data$geoLaenge==coord_station_ico$station_x[i])
#      m=filter(mergeddd, mergeddd$STATIONS_ID==i)
#      new=list( V1=m$V1[1],  Messdatum=as.character(m$V2[1]), V5=m$V5[1], V6=m$V6[1], meanico=mean(m$V7), geoBreite=m$geoBreite[1], geoLaenge=m$geoLaenge[1], meanstation=mean(m$FM), RMSE=rmse(mean(m$V7),mean(m$FM)), ST_ID=m$STATIONS_ID[1], ST_hoehe=m$Stationshoehe[1])
#     
#     #new=list(V1=ico$V1[1], V5=ico$V5[1] , V6=ico$V6[1], meanico=mean(ico$V7), geoBreite=coord_station_ico$station_y[i], geoLaenge=coord_station_ico$station_x[i],meanstation=mean(station$FM),RMSE=rmse((mean(ico$V7)),(mean(station$FM))),ST_ID=station$STATIONS_ID[1] ,ST_hoehe=station$Stationshoehe[1] )
#     ico_new=rbindlist(list(ico_new,new),use.names = F, fill = F, idcol = F)
#     #ico_new=rbind(ico_new,ico)
#     
#     
#   }
#   
#   
#   return(ico_new)
# }
getMeanPerDay=function(ico_file_cutted){
  #new dataframe
  #ico_new=data.frame(V1=factor(),Messdatum=factor() ,V5=numeric(), V6=numeric(),meanico=numeric(),geoBreite=numeric(), geoLaenge=numeric(),meanstation=numeric(),RMSE=numeric(),ST_ID=numeric(),ST_hoehe=numeric(),RUN=factor())
  #hours and date transform
  ico_file_cutted$Hours=as.POSIXct(ico_file_cutted$V2)
  ico_file_cutted$Hours=ico_file_cutted$Hours+2*60*60
  ico_file_cutted$Predate=as.Date(ico_file_cutted$Hours[1])
  
  #coordinates and station data
  setwd("~/data/DWD/station")
  coord_station_ico=read.csv("coord_station_wrf.csv") 
  station_data=read.csv("all/all_station_neu.csv")
  stationid=station_data$STATIONS_ID
  stationid=stationid[!duplicated(stationid)]
  station_data$MESS_DATUM=as.Date(as.character(station_data$MESS_DATUM), format="%Y%m%d")
  #filter station data by date 
  station_data=filter(station_data, MESS_DATUM==ico_file_cutted$Predate[1])
  
  #merge station data with the coordinates of ico
  station_data=merge(station_data,coord_station_ico, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
  merged_station_ico=merge(station_data,ico_file_cutted,  by.x=c("V5","V6"),by.y=c("V5","V6"))
  merged_station_ico=subset(merged_station_ico,select=c("MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7","FX","Stationshoehe","RUN","Predate","Hours"))
  #merged_station_ico=merged_station_ico[!duplicated(merged_station_ico),]
  print(merged_station_ico$MESS_DATUM[1])
  merged_station_ico=subset(merged_station_ico, merged_station_ico$FX!=-999 )
  ico_new=NULL
  
  stationid=merged_station_ico$STATIONS_ID
  stationid=stationid[!duplicated(stationid)]
  for (i in stationid ){
    m=filter(merged_station_ico, merged_station_ico$STATIONS_ID==i)
    new=list(MESS_DATUM=m$MESS_DATUM[1],V1=m$V1[1],V2=m$V2[1],STATIONS_ID=m$STATIONS_ID[1],V5=m$V5[1],V6=m$V6[1],geoLaenge=m$geoLaenge[1],geoBreite=m$geoBreit[1], V7_MAX_DAY=max(m$V7)[1],FX=max(m$FX)[1],RMSE_daily=rmse(max(m$FX),max(m$V7)),Stationshoehe=m$Stationshoehe[1],RUN=m$RUN[1],Predate=m$Predate[1],Hours=m$Hours[1])
    #print(station)
    
    #new=list(V1=ico$V1[1], V5=ico$V5[1] , V6=ico$V6[1], meanico=mean(ico$V7), geoBreite=coord_station_ico$station_y[i], geoLaenge=coord_station_ico$station_x[i],meanstation=mean(station$FM),RMSE=rmse((mean(ico$V7)),(mean(station$FM))) )
    ico_new=rbindlist(list(ico_new,new),use.names = F, fill = F, idcol = F)
    #   # ico_new=rbind(ico_new,ico)
    
    
  }
  
  
  print(ico_new[1])
  return(ico_new)
}



#ico_cutted_means=getMeanPerDay(ico_cutted1)
#ico_cutted_means_file=ico_cutted_means
getAccuracy=function(ico_cutted_means_file){
  require(Metrics)
  ico_cutted_means_file=subset(ico_cutted_means_file, select=c("Predate","MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7_MAX_DAY","FX","RMSE_daily","Stationshoehe","RUN"))
  acc_new=data.frame(Date=as.character(ico_cutted_means_file$Predate[1]),RUN=ico_cutted_means_file$RUN[1],Mean_ico=mean(ico_cutted_means_file$V7_MAX_DAY)[1], MAX_ico=max(ico_cutted_means_file$V7_MAX_DAY)[1],MeanStation=mean(ico_cutted_means_file$FX)[1],MAX_STATION=max(ico_cutted_means_file$FX)[1], BIAS=bias(ico_cutted_means_file$FX,ico_cutted_means_file$V7_MAX_DAY)[1],RMSE=rmse(ico_cutted_means_file$FX,ico_cutted_means_file$V7_MAX_DAY)[1],Accuracy=accuracy(ico_cutted_means_file$FX,ico_cutted_means_file$V7_MAX_DAY)[1])
  accu=read.csv("/home/m_meij02/data/icon/csv/pattern/accuracy_ico.csv")
  accu$X=NULL
  acc_new=rbindlist(list(accu,acc_new),use.names = F,fill=F,idcol=F)
  write.csv(acc_new,"/home/m_meij02/data/icon/csv/pattern/accuracy_ico.csv")
  
  return(ico_cutted_means_file)
}

#acc=getAccuracy(ico_cutted_means)



#main function consist of aheads function

#values per day

getAccuracyPerDay=function(path,modelname,month,day){
  path=path
  pattern=createpattern(modelname,month,day)
  ico_new=NULL
  for (i in pattern){
    print(i)
    
    h=getAccuracy(getMeanPerDay(cuticoCoordinates(multmerge(pathico,i))))
    ico_new=rbindlist(list(ico_new,h),use.names = F, fill = F, idcol = F)
  }
  
  #return(ico_new)
  
  
  setwd("~/data/icon/csv/pattern/iconperday")
  if(is.data.frame(ico_new) && nrow(ico_new)>0){
    if(day > 9){ 
      write.csv(ico_new, file=paste(modelname,"_","0",month,"_",day,"_", "acc",".csv",sep=""))}
    else {write.csv(ico_new, file=paste(modelname,"_", "0",month,"_", "0",day,"_","acc",".csv",sep=""))}
  }
  setwd(pathico)
  
}

for(i in 4:4){
  for (j in 24:31){
    
    testdate=createpattern("ico",i,j)
    # if(testdate[4]!="ico_2018012018z_"){
    
    l=list.files(path=pathico, pattern=paste(testdate[1],"*",sep=""))
    if(!is.na(l[1])){
      print("date input exist")
      
      if(j<10){
        setwd("~/data/icon/csv/pattern/iconperday")
        if(!file.exists(paste("ico_0",i,"_0",j,"_acc.csv",sep=""))){
          #print(j)
          getAccuracyPerDay(pathico,"ico",i,j)
        }
        else{print(paste("ico",i,j, "exist"))}
      }
      else
        if(j>9){
          setwd("~/data/icon/csv/pattern/iconperday")
          if(!file.exists(paste("ico_0",i,"_",j,"_acc.csv",sep=""))){
            getAccuracyPerDay(pathico,"ico",i,j)
          }
          else{print(paste("ico",i,j, "exist"))}
        }
      
    }
    
    else{next}
    
    # }
    # else{next}
  }
}

#test=getAccuracyPerDay(pathico,"ico",03,19)
# 
# for (i in 1:4){
#   for (j in 1:21){
#     
#     setwd("~/data/icon/csv/pattern/iconperday")
#     if(!file.exists(paste("ico",i,j,"acc.csv",sep="_"))){
#       print(j)
#       getAccuracyPerDay(pathico,"ico",i,j)
#     }
#     else (print(paste("ico",i,j, "exist")))
#     
#     
#   }
# }
