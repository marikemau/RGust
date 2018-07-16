#wrf
#whole qeue

#libaries
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(Metrics)
#setwd
setwd("~/data/wrf/csv/")

pathwrf="~/data/wrf/csv/"

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
#testi=createpattern("wrf",01,17)

##############################################################################################################


##############################################################################################################


#function to import all data into one list
#multmerge
multmerge = function(path,pattern){
  print("Step1")
  pathwrf="~/data/wrf/csv/"
  setwd(pathwrf)
  
  if (grepl("00z",substr(pattern,13,15))){
    filenames=list()
    #only need the 24hours of the next day, so cutteed here the predictionhours
    for (i in 22:46){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F),error=function(e) NULL)}))
    x$RUN="00"
    print(x[1])
    return(x)
    
  }
  if (grepl("06z",substr(pattern,13,15))){
    filenames=list()
    for (i in 16:39){
      # filenames=list.files(path=path, pattern=paste(pattern,i,".csv")
      
      filenames=rbind(filenames,paste(pattern,i,".csv",sep=""))}
    #print(filenames)
    #rbindlist( read.csv(x, header= F))}
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F),error=function(e) NULL)}))
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
    x=rbindlist(lapply(filenames, function(x){tryCatch(read.csv(x, header= F),error=function(e) NULL)}))
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

#wrf_test=multmerge(pathwrf,testi[1])
##############################################################################################################

##############################################################################################################
#read.csv("wrf_2018012018z_14.csv")

#wrf_test=multmerge(pathwrf,testi[4])

#Step2 
#croop coordinates by station coordinates
#used filter function for package dplyr
#wrf_file=wrf_test
cutWRFCoordinates=function(wrf_file){
 print("here")
  print(wrf_file[1])
    #import station data
    setwd("~/data/DWD/station")
    coord_station_wrf=read.csv("coord_station_wrf.csv")  
    #print(coord_station_wrf)
    wrf_new=data.frame(V1=factor(),V2=factor(),V5=numeric(), V6=numeric(),V7=numeric(),RUN=factor())
    n=nrow(coord_station_wrf)
    print(n)
    for (i in 1:n ){
      wrf=filter(wrf_file, wrf_file$V5==coord_station_wrf$V5[i] & wrf_file$V6==coord_station_wrf$V6[i])
      print(wrf)
      new=list(V1=wrf$V1, V2=wrf$V2, V5=wrf$V5 , V6=wrf$V6, V7=wrf$V7, RUN=wrf$RUN )
      wrf_new=rbindlist(list(wrf_new,new),use.names = F, fill = F, idcol = F)
      # wrf_new=rbind(wrf_new,wrf)
      
    }
    print(wrf_new[1])
    return(wrf_new)
  }
  
 



#wrf_cutted=cutWRFCoordinates(wrf_test)
# 
# wrf_c_t=merge(x=wrf_test,y=coord_station_wrf, by.x=c("V5","V6"), by.y=c("V5","V6"))
# 
# station_data=station_data[station_data$V5 %in% storm_file$V5,]
# station_data=station_data[station_data$V6 %in% storm_file$V6,]
# station_data=filter()
# require(dplyr)
# x=filter(station_data,  station_data$Hours %in% storm_file$Hours)
# #storm=filter(wrf_cutted, wrf_cutted$V7>17.1 )
#wrf_file_cutted=wrf_new
#Step3
#filter storm 17.2m/s
getStorm=function(wrf_file_cutted){
  #print(wrf_file_cutted[1])
  #storm=list()
  wrf_file_cutted=filter(wrf_file_cutted, wrf_file_cutted$V7>17.1 )
  print(wrf_file_cutted)
  if((is.data.frame(wrf_file_cutted) && nrow(wrf_file_cutted)!=0)){
    print("not empty")
   return(wrf_file_cutted)
  }
  else{return("empty")}
}

# #storm=getStorm(wrf_new)
# #storm1=getStorm(wrf_c_t)
# 
# 
# 
# #storm1$Hours=as.POSIXct(storm1$V2)
# #storm1$Hours=storm1$Hours+2*60*60
# station_wrf_storm=merge(x=storm1,y=station_data, by.x=c("Hours"),by.y=c("Hours"))
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
#storm_file=storm
#compare to station data
compareStormToStation=function(storm_file){
  if((is.data.frame(storm_file) && (nrow(storm_file)!=0))){
    
   storm_file$Hours=as.POSIXct(storm_file$V2)
   storm_file$Hours=storm_file$Hours+2*60*60
   storm_file$date=as.Date(storm_file$Hours[1])
   date=storm_file$date[1]
   print(date)
   setwd("~/data/DWD/station")
   coord_station_wrf=read.csv("coord_station_wrf.csv") 
   station_data=read.csv("all/all_station_neu.csv")
   stationid=station_data$STATIONS_ID
   stationid=stationid[!duplicated(stationid)]
   station_data$MESS_DATUM=as.Date(as.character(station_data$MESS_DATUM), format="%Y%m%d")
   #filter station data by date 
   station_data=filter(station_data, MESS_DATUM==storm_file$date[1])
   station_data=merge(station_data,coord_station_wrf, by.x=c("geoBreite","geoLaenge"),by.y=c("station_y","station_x"))
   #station_data$Hours=as.POSIXct(as.character(station_data$MESS_DATUM),format="%Y%m%d%H")
   station_wrf_storm=merge(x=storm_file,y=station_data, by.x=c("V5","V6"),by.y=c("V5","V6"))
   station_wrf_storm=subset(station_wrf_storm, select=c("MESS_DATUM","V1","V2","STATIONS_ID","V5","V6","V7","FX","Stationshoehe","geoBreite","geoLaenge","RUN"))
   station_wrf_storm=subset(station_wrf_storm, station_wrf_storm$FX!=-999 )
   
  
   newfp=NULL
   for(i in stationid){
     x=subset(station_wrf_storm, station_wrf_storm$STATIONS_ID==i)
     fp=list(MESS_DATUM=x$MESS_DATUM[1],V1=x$V1[1],STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1],V7=mean(x$V7), FX=mean(x$FX), Stationshoehe=x$Stationshoehe[1], geoBreite=x$geoBreite[1],geoLaenge=x$geoLaenge[1],RUN=x$RUN[1] )
     newfp=rbindlist(list(fp,newfp),fill=T)
   }
   station_wrf_storm=newfp[complete.cases(newfp)]
   return(station_wrf_storm)
  }

  else return("empty")
}

#station_wrf_storm=compareStormToStation(storm)

#station_wrf_storm_file=station_wrf_storm

#Step5
getFalsePositive=function(station_wrf_storm_file){
  if(is.data.frame(station_wrf_storm_file) && nrow(station_wrf_storm_file)>0){
    
  n=nrow(station_wrf_storm_file)
  for (i in 1:n){
    print(i)
  if (station_wrf_storm_file$V7[i] >= station_wrf_storm_file$FX[i] && station_wrf_storm_file$FX[i]< 17.2)
    (station_wrf_storm_file$FalsePositve[i]=T)
 else (station_wrf_storm_file$FalsePositve[i]=F)
  
  }
  return(station_wrf_storm_file)
  }
  return("empty")
}

#te=getFalsePositive(station_wrf_storm)


mainwrf=function(path,modelname,month,day){
  print(day)
  path=path
  #fp=data.frame(MESS_DATUM=date(),Hours=factor(), StationID=numeric() ,V5=numeric() ,V6=numeric(), V7=numeric(), FW=numeric(),Stationshoehe=numeric(), geoBreite=numeric(),geoLaenge=numeric(), FalsePositve=logical() )
  #   storm_new=rbindlist(list(storm_new,new),use.names = F, fill = F, idcol = F)
  path=path
  fp=NULL
  pattern=createpattern(modelname,month,day)
  for (i in pattern){
    print(i)
    
    h=getFalsePositive(compareStormToStation(getStorm(cutWRFCoordinates(multmerge(pathwrf,pattern[i])))))
    print(h)
    if(is.data.frame(h)){
      
    fp=rbindlist(list(fp,h),use.names = F, fill = F, idcol = F)
    }
    else {next}
  }
  setwd("~/data/wrf/csv/pattern/falsepositives")
  #fp$Hours=as.character(fp$Hours)
  fp$MESS_DATUM=as.character(fp$MESS_DATUM)
  
  if(day > 9){ 
    
    write.csv(fp, file=paste(modelname,"_","0",month,"_",day,"_", "fp",".csv",sep=""))}
  else {write.csv(fp, file=paste(modelname,"_", "0",month,"_", "0",day,"_","fp",".csv",sep=""))}
  setwd(pathwrf)
 # return(fp)
  
}


mainwrf(pathwrf,"wrf",01,20)
# for(i in 1:1){
#   for (j in 1:31){
#     
#     testdate=createpattern("wrf",i,j)
#     # if(testdate[4]!="wrf_2018012018z_"){
#     
#     l=list.files(path=pathwrf, pattern=paste(testdate[1],"*",sep=""))
#     if(!is.na(l[1])){
#       print("date input exist")
#       
#       if(j<10){
#         setwd("~/data/wrf/csv/pattern/falsepositives")
#         if(!file.exists(paste("wrf_0",i,"_0",j,"_fp.csv",sep=""))){
#           #print(j)
#          main(pathwrf,"wrf",i,j)
#         }
#         else{print(paste("wrf",i,j, "exist"))}
#       }
#       else
#         if(j>9){
#           setwd("~/data/wrf/csv/pattern/falsepositives")
#           if(!file.exists(paste("wrf_0",i,"_",j,"_fp.csv",sep=""))){
#             main(pathwrf,"wrf",i,j)
#           }
#           else{print(paste("wrf",i,j, "exist"))}
#         }
#       
#     }
#     
#     else{next}
#     
#     # }
#     # else{next}
#   }
# }
 # for (i in 2:4){
 #  for (j in 1:28){
 # 
 #    setwd("~/data/wrf/csv/pattern/falsepositives")
 #    if(j<10){
 #      {if(!file.exists(paste("wrf_0",i,"_0",j,"_fp.csv",sep=""))){
 #        print(j)
 #        main(pathwrf,"wrf",i,j)
 #      }
 #        else(print(paste("wrf",i,j, "exist")))
 #      }
 #    }
 #    else{ {if(!file.exists(paste("wrf_0",i,"_",j,"_fp.csv",sep=""))){
 #      print(j)
 #      main(pathwrf,"wrf",i,j)
 #    }
 #      else(print(paste("wrf",i,j, "exist")))
 #     }}
 #     #
 # 
 #   }
 #  }