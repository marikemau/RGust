#wrf
#whole qeue

#libaries
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(Metrics)
#setwd
setwd("~/R/MA/wrf/csv/")

pathwrf="~/R/MA/wrf/csv/"
 
#Step 1
#merge the needed prediction hours for each run 
#createpattern for filenameconditions 
createpattern= function(n,m,d){
  pattern=list()
  if (n == "wrf") {
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
#testi=createpattern("wrf",01,23)

#function to import all data into one list
#multmerge
multmerge = function(path,pattern){
  print("Step1")
  pathwrf="~/R/MA/wrf/csv/"
  setwd(pathwrf)
  
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
#l=read.csv("wrf_2018012018z_05.csv")

#wrf_test1=multmerge(pathwrf,testi[2])

#Step2 
#croop coordinates by station coordinates
#used filter function for package dplyr
cutWRFCoordinates=function(wrf_file){
  if(is.data.frame(wrf_file)){
 
#import station data
setwd("~/R/MA/dwd/")
coord_station_wrf=read.csv("coord_station_wrf.csv")  
#print(coord_station_wrf)
wrf_new=data.frame(V1=factor(),V2=factor(),V5=numeric(), V6=numeric(),V7=numeric(),RUN=factor())
n=nrow(coord_station_wrf)
print("Step2")
for (i in 1:n ){
  wrf=filter(wrf_file, (getElement(wrf_file,"V5")==coord_station_wrf$V5[i] & getElement(wrf_file,"V6")==coord_station_wrf$V6[i]))
  new=list(V1=wrf$V1, V2=wrf$V2, V5=wrf$V5 , V6=wrf$V6, V7=wrf$V7, RUN=wrf$RUN )
  wrf_new=rbindlist(list(wrf_new,new),use.names = F, fill = F, idcol = F)
 # wrf_new=rbind(wrf_new,wrf)

}
# wrf_new$V1=as.POSIXct(wrf_new$V1)
# wrf_new$V1=wrf_new$V1+48*60*60
# wrf_new$V2=as.POSIXct(wrf_new$V2)
# wrf_new$V2=wrf_new$V2+48*60*60
print(wrf_new[1])
return(wrf_new)
  }
  
  else{
    print("wrf_file empty")
    return("empty")}
}


#wrf_cutted=cutWRFCoordinates(wrf_test)


#wrf_file_cutted=wrf_cutted

#Step3 
#Get means per Day for all runs 
getMeanPerDay=function(wrf_file_cutted){
  #new dataframe
  #wrf_new=data.frame(V1=factor(),Messdatum=factor() ,V5=numeric(), V6=numeric(),meanwrf=numeric(),geoBreite=numeric(), geoLaenge=numeric(),meanstation=numeric(),RMSE=numeric(),ST_ID=numeric(),ST_hoehe=numeric(),RUN=factor())
  #hours and date transform
  wrf_file_cutted$Hours=as.POSIXct(wrf_file_cutted$V2)
  wrf_file_cutted$Hours=wrf_file_cutted$Hours+2*60*60
  wrf_file_cutted$Predate=as.Date(wrf_file_cutted$Hours[1])
  
  #coordinates and station data
  setwd("~/R/MA/dwd/")
  coord_station_wrf=read.csv("coord_station_wrf.csv") 
  station_data=read.csv("all_station_neu.csv")
  stationid=station_data$STATIONS_ID
  stationid=stationid[!duplicated(stationid)]
  station_data$MESS_DATUM=as.Date(as.character(station_data$MESS_DATUM), format="%Y%m%d")
  #filter station data by date 
  station_data=filter(station_data, MESS_DATUM==wrf_file_cutted$Predate[1])
  
  #merge station data with the coordinates of wrf
  station_data=merge(station_data,coord_station_wrf, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
  merged_station_wrf=merge(station_data,wrf_file_cutted,  by.x=c("V5","V6"),by.y=c("V5","V6"))
  merged_station_wrf=subset(merged_station_wrf,select=c("MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7","FX","Stationshoehe","RUN","Predate","Hours"))
  #merged_station_wrf=merged_station_wrf[!duplicated(merged_station_wrf),]
  print(merged_station_wrf$MESS_DATUM[1])
  merged_station_wrf=subset(merged_station_wrf, merged_station_wrf$FX!=-999 )
  wrf_new=NULL
  
  stationid=merged_station_wrf$STATIONS_ID
  stationid=stationid[!duplicated(stationid)]
  for (i in stationid ){
    m=filter(merged_station_wrf, merged_station_wrf$STATIONS_ID==i)
    new=list(MESS_DATUM=m$MESS_DATUM[1],V1=m$V1[1],V2=m$V2[1],STATIONS_ID=m$STATIONS_ID[1],V5=m$V5[1],V6=m$V6[1],geoLaenge=m$geoLaenge[1],geoBreite=m$geoBreit[1], V7_MAX_DAY=max(m$V7)[1],FX=max(m$FX)[1],RMSE_daily=rmse(max(m$FX),max(m$V7)),Stationshoehe=m$Stationshoehe[1],RUN=m$RUN[1],Predate=m$Predate[1],Hours=m$Hours[1])
    #print(station)

    #new=list(V1=wrf$V1[1], V5=wrf$V5[1] , V6=wrf$V6[1], meanwrf=mean(wrf$V7), geoBreite=coord_station_wrf$station_y[i], geoLaenge=coord_station_wrf$station_x[i],meanstation=mean(station$FM),RMSE=rmse((mean(wrf$V7)),(mean(station$FM))) )
    wrf_new=rbindlist(list(wrf_new,new),use.names = F, fill = F, idcol = F)
  #   # wrf_new=rbind(wrf_new,wrf)

    
  }
  
 
  print(wrf_new[1])
    return(wrf_new)
}
#wrf_cutted_means=getMeanPerDay(wrf_cutted)
#wrf_cutted_means_file=wrf_cutted_means
getAccuracy=function(wrf_cutted_means_file){
  require(Metrics)
  wrf_cutted_means_file=subset(wrf_cutted_means_file, select=c("Predate","MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","geoLaenge","geoBreite","V7_MAX_DAY","FX","RMSE_daily","Stationshoehe","RUN"))
  acc_new=data.frame(Date=as.character(wrf_cutted_means_file$Predate[1]),RUN=wrf_cutted_means_file$RUN[1],Mean_WRF=mean(wrf_cutted_means_file$V7_MAX_DAY)[1], MAX_WRF=max(wrf_cutted_means_file$V7_MAX_DAY)[1],MeanStation=mean(wrf_cutted_means_file$FX)[1],MAX_STATION=max(wrf_cutted_means_file$FX)[1], BIAS=bias(wrf_cutted_means_file$FX,wrf_cutted_means_file$V7_MAX_DAY)[1],RMSE=rmse(wrf_cutted_means_file$FX,wrf_cutted_means_file$V7_MAX_DAY)[1],Accuracy=accuracy(wrf_cutted_means_file$FX,wrf_cutted_means_file$V7_MAX_DAY)[1])
  accu=read.csv("~/R/MA/wrf/pattern/accuracy_wrf.csv")
  accu$X=NULL
  acc_new=rbindlist(list(accu,acc_new),use.names = F,fill=F,idcol=F)
  write.csv(acc_new,"~/R/MA/wrf/pattern/accuracy_wrf.csv")
   return(wrf_cutted_means_file)
}

#acc=getAccuracy(wrf_cutted_means)
#main function consist of aheads function

#values per day

getAccuracyPerDay=function(path,modelname,month,day){
 path=path
  pattern=createpattern(modelname,month,day)
  wrf_new=NULL
  for (i in pattern){
    print(i)
    
    h=getAccuracy(getMeanPerDay(cutWRFCoordinates(multmerge(pathwrf,i))))
    wrf_new=rbindlist(list(wrf_new,h),use.names = F, fill = F, idcol = F)
    }
  
    #return(wrf_new)
  
  
  setwd("~/R/MA/wrf/pattern/wrfperday")
  if(is.data.frame(wrf_new) && nrow(wrf_new)>0){
  if(day > 9){ 
  write.csv(wrf_new, file=paste(modelname,"_","0",month,"_",day,"_", "acc",".csv",sep=""))}
  else {write.csv(wrf_new, file=paste(modelname,"_", "0",month,"_", "0",day,"_","acc",".csv",sep=""))}
  }
  setwd(pathwrf)
  
}

#h=getAccuracyPerDay(pathwrf,"wrf",1,23)

for(i in 1:1){
  for (j in 24:24){
    
    testdate=createpattern("wrf",i,j)
   # if(testdate[4]!="wrf_2018012018z_"){
      
        l=list.files(path=pathwrf, pattern=paste(testdate[1],"*",sep=""))
        if(!is.na(l[1])){
            print("date input exist")
            
            if(j<10){
                setwd("~/R/MA/wrf/pattern/wrfperday")
                if(!file.exists(paste("wrf_0",i,"_0",j,"_acc.csv",sep=""))){
                #print(j)
                getAccuracyPerDay(pathwrf,"wrf",i,j)
                }
                else{print(paste("wrf",i,j, "exist"))}
            }
            else
              if(j>9){
              setwd("~/R/MA/wrf/pattern/wrfperday")
              if(!file.exists(paste("wrf_0",i,"_",j,"_acc.csv",sep=""))){
              getAccuracyPerDay(pathwrf,"wrf",i,j)
              }
              else{print(paste("wrf",i,j, "exist"))}
           }
      
        }
    
      else{next}
    
   # }
   # else{next}
  }
}

# 
# 
# for(i in 1:4){
#   for(j in 1:31){
#     testdate=createpattern("wrf",i,j)
#     l=list.files(path=pathwrf,pattern=paste(testdate[1],"*",sep=""))
#     if (testdate[4]=="wrf_2018012018z_"){next}
#     if(is.na(l[1])){print("no date input")}
#     else{
#       print("date exist")
#       setwd("~/data/wrf/csv/pattern/wrfperday")
#       if(j<10){
#            {if(!file.exists(paste("wrf_0",i,"_0",j,"_acc.csv",sep=""))){
#         print(j)
#                getAccuracyPerDay(pathwrf,"wrf",i,j)
#              }
#              else(print(paste("wrf",i,j, "exist")))
#              }
#              }
#             else{
#               if(!file.exists(paste("wrf_0",i,"_",j,"_acc.csv",sep=""))){
#                print(j)
#                getAccuracyPerDay(pathwrf,"wrf",i,j)
#         }
#                else(print(paste("wrf",i,j, "exist")))
#              }
#    
#     }
#     
#   }
# }


# for (i in 1:4){
#   for (j in  1:31){
# 
#     setwd("~/data/wrf/csv/pattern/wrfperday2")
#     if(j<10){
#     {if(!file.exists(paste("wrf_0",i,"_0",j,"_acc.csv",sep=""))){
#       print(j)
#       getAccuracyPerDay(pathwrf,"wrf",i,j)
#     }
#     else(print(paste("wrf",i,j, "exist")))
#     }
#     }
#     else{ {if(!file.exists(paste("wrf_0",i,"_",j,"_acc.csv",sep=""))){
#       print(j)
#       getAccuracyPerDay(pathwrf,"wrf",i,j)
#     }
#       else(print(paste("wrf",i,j, "exist")))
#     }}
#     #
# 
#   }
# }
 