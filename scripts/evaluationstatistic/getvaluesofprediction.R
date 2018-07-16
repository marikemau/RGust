#Kategorische Ma??zahlen
#A= P=y | O=y
#B= P=y | O=n
#C= P=n | O=y
#D= P=n | O=n
###############################################
#libaries
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
cospath="~/data/cosmo/csv/pattern/"
wrfpath="~/data/wrf/csv/pattern/falsepositives/"
gfspath="~/data/gfs/csv/pattern/"
icopath="~/data/icon/csv/pattern/"
####
#WRF
####
####################################################################################################
#setwd and get data
#setwd("~/data/wrf/csv/pattern/wrfperday")
#wrf_filenames=list.files(path="~/data/wrf/csv/pattern/wrfperday")
#wrf_all=rbindlist(lapply(wrf_filenames, function(x){tryCatch(read.csv(x, header= T))}),fill=T)
wrf_all_no_runs <- read.csv("~/data/wrf/csv/pattern/wrf_all_no_runs.csv",header=T)
#data.frame(modelname=character(),A=numeric(),B=numeric(),C=numeric(),D=numeric(),date=character())
getValueofPrediction=function(model_file,modelname){
  valueprediction=NULL
  dates=model_file$Predate
  dates=unique(dates)
  stationid=model_file$STATIONS_ID
  stationid=unique(stationid)
  for (i in dates){
    for (j in stationid) {
      x=subset(model_file,model_file$Predate==i & model_file$STATIONS_ID==j)
      if(nrow(x)==0){next}
      if(x$V7_MAX_DAY>17.1 && x$FX >17.1){
        new=list(Predate=x$Predate[1], STATIONS_ID=x$STATIONS_ID[1], V5=x$V5[1],V6=x$V6[1],V7_MAX_DAY=x$V7_MAX_DAY,FX=x$FX, Stationshoehe=x$Stationshoehe, A=1,B=0,C=0,D=0)}
      else if(x$V7_MAX_DAY>17.1 && x$FX <17.2){
        new=list(Predate=x$Predate[1], STATIONS_ID=x$STATIONS_ID[1], V5=x$V5,V6=x$V6,V7_MAX_DAY=x$V7_MAX_DAY,FX=x$FX, Stationshoehe=x$Stationshoehe, A=0,B=1,C=0,D=0)}
      else if(x$V7_MAX_DAY<17.2 && x$FX >17.1){
        new=list(Predate=x$Predate[1], STATIONS_ID=x$STATIONS_ID[1], V5=x$V5,V6=x$V6,V7_MAX_DAY=x$V7_MAX_DAY,FX=x$FX, Stationshoehe=x$Stationshoehe, A=0,B=0,C=1,D=0)}
      else if(x$V7_MAX_DAY<17.2 && x$FX <17.2){
        new=list(Predate=x$Predate[1], STATIONS_ID=x$STATIONS_ID[1], V5=x$V5,V6=x$V6,V7_MAX_DAY=x$V7_MAX_DAY,FX=x$FX, Stationshoehe=x$Stationshoehe, A=0,B=0,C=0,D=1)}
      else return("error")
      
      valueprediction=rbindlist(list(new,valueprediction),use.names = F,fill=F)
    }
  }
  print("here")
  if (modelname=="wrf"){write.csv(valueprediction, paste(wrfpath,"/fp_all_noruns_new.csv",sep=""))}
  else if (modelname=="gfs"){write.csv(valueprediction, paste(gfspath,"/fp_all_noruns_new.csv",sep=""))}
  else if (modelname=="icon"){write.csv(valueprediction, paste(icopath,"/fp_all_noruns_new.csv",sep=""))}
  else if (modelname =="cospath"){write.csv(valueprediction, paste(cospath,"/fp_all_noruns_new.csv",sep=""))}
  else 
    print("wrong")
  return(valueprediction)
}
x=getValueofPrediction(wrf_all_no_runs,"wrf")


write.csv(x, paste(wrfpath,"fp_all_noruns_new.csv",sep=""))


###################################################################################################################
###################################################################################################################

