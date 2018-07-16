#Script to get one value for all models per day instead ofrun
setwd("~/data/cosmo/csv/pattern/cosmoperday")
cospath="~/data/cosmo/csv/pattern/cosmoperday"
wrfpath="~/data/wrf/csv/pattern/wrfperday"
gfspath="~/data/gfs/csv/pattern/gfsperday"
icopath="~/data/icon/csv/pattern/iconperday"

#libaries
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(Metrics)
library(sp)


#wrf_all
setwd(wrfpath)
wrf_filenames=list.files(path=wrfpath)
wrf_all=rbindlist(lapply(wrf_filenames, function(x){tryCatch(read.csv(x, header= T))}),fill=T)
dates=wrf_all$MESS_DATUM
dates=dates[!duplicated(dates)]
wrf_per_day_all_runs=NULL
x=list()
stationid=wrf_all$STATIONS_ID
stationid=stationid[!(duplicated(stationid))]
for (i in dates){
  for (j in stationid){
    
    wrf_per_day=subset(wrf_all,wrf_all$MESS_DATUM==i & wrf_all$STATIONS_ID==j)
    x=list(Predate=wrf_per_day$Predate[1], STATIONS_ID=wrf_per_day$STATIONS_ID[1],V5=wrf_per_day$V5[1],  V6=wrf_per_day$V6[1],V7_MAX_DAY=max(wrf_per_day$V7_MAX_DAY)[1],FX=wrf_per_day$FX[1],RMSE_daily=rmse(max(wrf_per_day$FX),max(wrf_per_day$V7_MAX_DAY)),Stationshoehe=wrf_per_day$Stationshoehe[1])
    
    wrf_per_day_all_runs=rbindlist(list(x,wrf_per_day_all_runs),use.names = F,fill=F)
    
  }
}
wrf_per_day_all_runs[complete.cases(wrf_per_day_all_runs), ]
wrf_per_day_all_runs <- wrf_per_day_all_runs[!is.infinite(wrf_per_day_all_runs$V7_MAX_DAY),]
wrf_per_day_all_runs[complete.cases(wrf_per_day_all_runs), ]
write.csv(wrf_per_day_all_runs,"/home/m_meij02/data/wrf/csv/pattern/wrf_all_no_runs.csv")


##so_all
setwd(cospath)
cos_filenames=list.files(path=cospath)
cos_all=rbindlist(lapply(cos_filenames, function(x){tryCatch(read.csv(x, header= T))}),fill=T)
dates_cos=cos_all$MESS_DATUM
dates_cos=dates_cos[!duplicated(dates_cos)]
cos_per_day_all_runs=NULL
xc=list()
stationid=cos_all$STATIONS_ID
stationid=stationid[!(duplicated(stationid))]
for (i in dates_cos){
  for (j in stationid){
    
    cos_per_day=subset(cos_all,cos_all$MESS_DATUM==i & cos_all$STATIONS_ID==j)
    x=list(Predate=cos_per_day$Predate[1], STATIONS_ID=cos_per_day$STATIONS_ID[1],V5=cos_per_day$V5[1],  V6=cos_per_day$V6[1],V7_MAX_DAY=max(cos_per_day$V7_MAX_DAY),FX=cos_per_day$FX[1],RMSE_daily=rmse(max(cos_per_day$FX),max(cos_per_day$V7_MAX_DAY)),Stationshoehe=cos_per_day$Stationshoehe[1])
    
    cos_per_day_all_runs=rbindlist(list(x,cos_per_day_all_runs),use.names = F,fill=F)
    
  }
}
cos_per_day_all_runs[complete.cases(cos_per_day_all_runs), ]
cos_per_day_all_runs <- cos_per_day_all_runs[!is.infinite(cos_per_day_all_runs$V7_MAX_DAY),]
cos_per_day_all_runs[complete.cases(cos_per_day_all_runs), ]
write.csv(cos_per_day_all_runs,"/home/m_meij02/data/cosmo/csv/pattern/cos_all_no_runs.csv")


################################


#ico_all
setwd(icopath)
ico_filenames=list.files(path=icopath)
ico_all=rbindlist(lapply(ico_filenames, function(x){tryCatch(read.csv(x, header= T))}),fill=T)
dates=ico_all$MESS_DATUM
dates=dates[!duplicated(dates)]
ico_per_day_all_runs=NULL
x=list()
stationid=ico_all$STATIONS_ID
stationid=stationid[!(duplicated(stationid))]
for (i in dates){
  for (j in stationid){
    
    ico_per_day=subset(ico_all,ico_all$MESS_DATUM==i & ico_all$STATIONS_ID==j)
    x=list(Predate=ico_per_day$Predate[1], STATIONS_ID=ico_per_day$STATIONS_ID[1],V5=ico_per_day$V5[1],  V6=ico_per_day$V6[1],V7_MAX_DAY=max(ico_per_day$V7_MAX_DAY),FX=ico_per_day$FX[1],RMSE_daily=rmse(max(ico_per_day$FX),max(ico_per_day$V7_MAX_DAY)),Stationshoehe=ico_per_day$Stationshoehe[1])
    
    ico_per_day_all_runs=rbindlist(list(x,ico_per_day_all_runs),use.names = F,fill=F)
    
  }
}
ico_per_day_all_runs[complete.cases(ico_per_day_all_runs), ]
ico_per_day_all_runs <- ico_per_day_all_runs[!is.infinite(ico_per_day_all_runs$V7_MAX_DAY),]
ico_per_day_all_runs[complete.cases(ico_per_day_all_runs), ]
write.csv(ico_per_day_all_runs,"/home/m_meij02/data/icon/csv/pattern/ico_all_no_runs.csv")


################################


#gfs_all
setwd(gfspath)
gfs_filenames=list.files(path=gfspath)
gfs_all=rbindlist(lapply(gfs_filenames, function(x){tryCatch(read.csv(x, header= T))}),fill=T)
dates=gfs_all$MESS_DATUM
dates=dates[!duplicated(dates)]
gfs_per_day_all_runs=NULL
x=list()
stationid=gfs_all$STATIONS_ID
stationid=stationid[!(duplicated(stationid))]
for (i in dates){
  for (j in stationid){
    
    gfs_per_day=subset(gfs_all,gfs_all$MESS_DATUM==i & gfs_all$STATIONS_ID==j)
    x=list(Predate=gfs_per_day$Predate[1], STATIONS_ID=gfs_per_day$STATIONS_ID[1],V5=gfs_per_day$V5[1],  V6=gfs_per_day$V6[1],V7_MAX_DAY=max(gfs_per_day$V7_MAX_DAY),FX=gfs_per_day$FX[1],RMSE_daily=rmse(max(gfs_per_day$FX),max(gfs_per_day$V7_MAX_DAY)),Stationshoehe=gfs_per_day$Stationshoehe[1])
    
    gfs_per_day_all_runs=rbindlist(list(x,gfs_per_day_all_runs),use.names = F,fill=F)
    
  }
}
gfs_per_day_all_runs[complete.cases(gfs_per_day_all_runs), ]
gfs_per_day_all_runs <- gfs_per_day_all_runs[!is.infinite(gfs_per_day_all_runs$V7_MAX_DAY),]
gfs_per_day_all_runs[complete.cases(gfs_per_day_all_runs), ]
write.csv(gfs_per_day_all_runs,"/home/m_meij02/data/gfs/csv/pattern/gfs_all_no_runs.csv")


