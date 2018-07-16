#get monthly and overall rmse

wrf_all_no_runs=read.csv("/R/data/wrf/wrf_all_no_runs.csv",header = T)

cos_all_no_runs=read.csv("/R/data/cosmo/cos_all_no_runs.csv",header = T)

ico_all_no_runs=read.csv("/R/data/icon/ico_all_no_runs.csv",header = T)

gfs_all_no_runs=read.csv("/R/data/gfs/gfs_all_no_runs.csv",header = T)


######
#WRF
######
dates=wrf_all_no_runs$Predate
dates=dates[!duplicated(dates)]

dates_jan=dates[85:97]
dates_feb=dates[57:84]
dates_mar=dates[26:56]
dates_apr=dates[1:25]

#subset wrf_data for each month 
wrf_all_jan=wrf_all_no_runs[wrf_all_no_runs$Predate %in% dates_jan, ] 
wrf_all_feb=wrf_all_no_runs[wrf_all_no_runs$Predate%in% dates_feb, ] 
wrf_all_mar=wrf_all_no_runs[wrf_all_no_runs$Predate %in% dates_mar, ] 
wrf_all_apr=wrf_all_no_runs[wrf_all_no_runs$Predate %in% dates_apr, ] 

#stationid
stationid=wrf_all_no_runs$STATIONS_ID
stationid=stationid[!duplicated(stationid)]
jan_all=NULL
feb_all=NULL
mar_all=NULL
apr_all=NULL
for (i in stationid){
  
  x=subset(wrf_all_jan,wrf_all_jan$STATIONS_ID==i)
 jan=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="Jan")
 jan_all=rbindlist(list(jan_all,jan),fill=F,use.names = F)
 x=subset(wrf_all_feb,wrf_all_feb$STATIONS_ID==i)
 feb=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="feb")
 feb_all=rbindlist(list(feb_all,feb),fill=F,use.names = F)
 x=subset(wrf_all_mar,wrf_all_mar$STATIONS_ID==i)
 mar=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="mar")
 mar_all=rbindlist(list(mar_all,mar),fill=F,use.names = F)
 x=subset(wrf_all_apr,wrf_all_apr$STATIONS_ID==i)
 apr=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="apr")
 apr_all=rbindlist(list(apr_all,apr),fill=F,use.names = F)
}


write.csv(jan_all,"/R/data/wrf/wrf_monthly_jan.csv")
write.csv(feb_all,"/R/data/wrf/wrf_monthly_feb.csv")
write.csv(mar_all,"/R/data/wrf/wrf_monthly_mar.csv")
write.csv(apr_all,"/R/data/wrf/wrf_monthly_apr.csv")

##################################################################################################################################

##################################################################################################################################
######
#COSMO
######
dates=cos_all_no_runs$Predate
dates=dates[!duplicated(dates)]

dates_jan=dates[70:83]
dates_feb=dates[42:69]
dates_mar=dates[11:41]
dates_apr=dates[1:10]

#subset cos_data for each month 
cos_all_jan=cos_all_no_runs[cos_all_no_runs$Predate %in% dates_jan, ] 
cos_all_feb=cos_all_no_runs[cos_all_no_runs$Predate%in% dates_feb, ] 
cos_all_mar=cos_all_no_runs[cos_all_no_runs$Predate %in% dates_mar, ] 
cos_all_apr=cos_all_no_runs[cos_all_no_runs$Predate %in% dates_apr, ] 

#stationid
stationid=cos_all_no_runs$STATIONS_ID
stationid=stationid[!duplicated(stationid)]
jan_all=NULL
feb_all=NULL
mar_all=NULL
apr_all=NULL
for (i in stationid){
  
  x=subset(cos_all_jan,cos_all_jan$STATIONS_ID==i)
  jan=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="Jan")
  jan_all=rbindlist(list(jan_all,jan),fill=F,use.names = F)
  x=subset(cos_all_feb,cos_all_feb$STATIONS_ID==i)
  feb=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="feb")
  feb_all=rbindlist(list(feb_all,feb),fill=F,use.names = F)
  x=subset(cos_all_mar,cos_all_mar$STATIONS_ID==i)
  mar=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="mar")
  mar_all=rbindlist(list(mar_all,mar),fill=F,use.names = F)
  x=subset(cos_all_apr,cos_all_apr$STATIONS_ID==i)
  apr=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="apr")
  apr_all=rbindlist(list(apr_all,apr),fill=F,use.names = F)
}


write.csv(jan_all,"/R/data/cosmo/cos_monthly_jan.csv")
write.csv(feb_all,"/R/data/cosmo/cos_monthly_feb.csv")
write.csv(mar_all,"/R/data/cosmo/cos_monthly_mar.csv")
write.csv(apr_all,"/R/data/cosmo/cos_monthly_apr.csv")
##################################################################################################################################

##################################################################################################################################
######
#icon
######
dates=ico_all_no_runs$Predate
dates=dates[!duplicated(dates)]

dates_jan=dates[91:104]
dates_feb=dates[63:90]
dates_mar=dates[32:62]
dates_apr=dates[1:32]

#subset ico_data for each month 
ico_all_jan=ico_all_no_runs[ico_all_no_runs$Predate %in% dates_jan, ] 
ico_all_feb=ico_all_no_runs[ico_all_no_runs$Predate%in% dates_feb, ] 
ico_all_mar=ico_all_no_runs[ico_all_no_runs$Predate %in% dates_mar, ] 
ico_all_apr=ico_all_no_runs[ico_all_no_runs$Predate %in% dates_apr, ] 

#stationid
stationid=ico_all_no_runs$STATIONS_ID
stationid=stationid[!duplicated(stationid)]
jan_all=NULL
feb_all=NULL
mar_all=NULL
apr_all=NULL
for (i in stationid){
  
  x=subset(ico_all_jan,ico_all_jan$STATIONS_ID==i)
  jan=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="Jan")
  jan_all=rbindlist(list(jan_all,jan),fill=F,use.names = F)
  x=subset(ico_all_feb,ico_all_feb$STATIONS_ID==i)
  feb=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="feb")
  feb_all=rbindlist(list(feb_all,feb),fill=F,use.names = F)
  x=subset(ico_all_mar,ico_all_mar$STATIONS_ID==i)
  mar=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="mar")
  mar_all=rbindlist(list(mar_all,mar),fill=F,use.names = F)
  x=subset(ico_all_apr,ico_all_apr$STATIONS_ID==i)
  apr=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="apr")
  apr_all=rbindlist(list(apr_all,apr),fill=F,use.names = F)
}


write.csv(jan_all,"/R/data/icon/ico_monthly_jan.csv")
write.csv(feb_all,"/R/data/icon/ico_monthly_feb.csv")
write.csv(mar_all,"/R/data/icon/ico_monthly_mar.csv")
write.csv(apr_all,"/R/data/icon/ico_monthly_apr.csv")
##################################################################################################################################

##################################################################################################################################
######
#gfsn
######
dates=gfs_all_no_runs$Predate
dates=dates[!duplicated(dates)]
#RMSE mean months
dates_jan=dates[85:98]
dates_feb=dates[57:84]
dates_mar=dates[26:56]
dates_apr=dates[1:25]

#subset gfs_data for each month 
gfs_all_jan=gfs_all_no_runs[gfs_all_no_runs$Predate %in% dates_jan, ] 
gfs_all_feb=gfs_all_no_runs[gfs_all_no_runs$Predate%in% dates_feb, ] 
gfs_all_mar=gfs_all_no_runs[gfs_all_no_runs$Predate %in% dates_mar, ] 
gfs_all_apr=gfs_all_no_runs[gfs_all_no_runs$Predate %in% dates_apr, ] 

#stationid
stationid=gfs_all_no_runs$STATIONS_ID
stationid=stationid[!duplicated(stationid)]
jan_all=NULL
feb_all=NULL
mar_all=NULL
apr_all=NULL
for (i in stationid){
  
  x=subset(gfs_all_jan,gfs_all_jan$STATIONS_ID==i)
  jan=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="Jan")
  jan_all=rbindlist(list(jan_all,jan),fill=F,use.names = F)
  x=subset(gfs_all_feb,gfs_all_feb$STATIONS_ID==i)
  feb=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="feb")
  feb_all=rbindlist(list(feb_all,feb),fill=F,use.names = F)
  x=subset(gfs_all_mar,gfs_all_mar$STATIONS_ID==i)
  mar=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="mar")
  mar_all=rbindlist(list(mar_all,mar),fill=F,use.names = F)
  x=subset(gfs_all_apr,gfs_all_apr$STATIONS_ID==i)
  apr=list(STATIONS_ID=x$STATIONS_ID[1],V5=x$V5[1],V6=x$V6[1], V7_MAX=max(x$V7_MAX_DAY),FX=max(x$FX),RMSE=rmse(x$FX,x$V7_MAX_DAY),Stationshoehe=x$Stationshoehe[1],Month="apr")
  apr_all=rbindlist(list(apr_all,apr),fill=F,use.names = F)
}


write.csv(jan_all,"/R/data/gfs/gfs_monthly_jan.csv")
write.csv(feb_all,"/R/data/gfs/gfs_monthly_feb.csv")
write.csv(mar_all,"/R/data/gfs/gfs_monthly_mar.csv")
write.csv(apr_all,"/R/data/gfs/gfs_monthly_apr.csv")


