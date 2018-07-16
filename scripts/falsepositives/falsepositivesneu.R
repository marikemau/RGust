#Detect false positives

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

setwd(wrfpath)
wrf_filenames=list.files(path=wrfpath)
wrf_all=rbindlist(lapply(wrf_filenames, function(x){tryCatch(read.csv(x, header= T))}),fill=T)
wrf_all_no_runs <- read.csv("~/data/wrf/csv/pattern/wrf_all_no_runs.csv",header=T)

#Step3
#filter storm 17.2m/s
getStorm=function(wrf_file_cutted){
  print("Step3")
  storm=filter(wrf_file_cutted, wrf_file_cutted$V7_MAX_DAY>17.1 )
  if(!(is.data.frame(storm) && nrow(storm)==0)){
    return(storm)
  }
  return("empty")
}

storm=getStorm(wrf_all_no_runs)

#Step5
getFalsePositive=function(storm_file){
  if(is.data.frame(storm_file) && nrow(storm_file)>0){
    
    print("Step5")
    n=nrow(storm_file)
    for (i in 1:n){
      if (storm_file$V7_MAX_DAY[i] >= storm_file$FX[i] && storm_file$FX[i]< 17.2)
        (storm_file$FalsePositve[i]=T)
      else (storm_file$FalsePositve[i]=F)
      
    }
    return(storm_file)
  }
  return("empty")
}

#te=getFalsePositive(storm)

write.csv(te, "/home/m_meij02/data/wrf/csv/pattern/falsepositives/fp_all_noruns.csv")


###################################################################################################################
###################################################################################################################
#Detect false positives

#cos
#whole qeue

#libaries
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(Metrics)
#setwd
setwd("~/data/cosmo/csv/pattern/cosmoperday")

pathcos="~/data/cosmo/csv/pattern/cosmoperday"

setwd(cospath)
cos_filenames=list.files(path=pathcos)
cos_all=rbindlist(lapply(cos_filenames, function(x){tryCatch(read.csv(x, header= T))}),fill=T)
cos_all_no_runs <- read.csv("~/data/cosmo/csv/pattern/cos_all_no_runs.csv",header=T)

#Step3
#filter storm 17.2m/s
getStorm=function(cos_file_cutted){
  print("Step3")
  storm=filter(cos_file_cutted, cos_file_cutted$V7_MAX_DAY>17.1 )
  if(!(is.data.frame(storm) && nrow(storm)==0)){
    return(storm)
  }
  return("empty")
}

storm=getStorm(cos_all_no_runs)
#Step5
getFalsePositive=function(storm_file){
  if(is.data.frame(storm_file) && nrow(storm_file)>0){
    
    print("Step5")
    n=nrow(storm_file)
    for (i in 1:n){
      if (storm_file$V7_MAX_DAY[i] >= storm_file$FX[i] && storm_file$FX[i]< 17.2)
        (storm_file$FalsePositve[i]=T)
      else (storm_file$FalsePositve[i]=F)
      
    }
    return(storm_file)
  }
  return("empty")
}

cos_fp=getFalsePositive(storm)


write.csv(cos_fp, "/home/m_meij02/data/cosmo/csv/pattern/fp_all_noruns.csv")




###################################################################################################################
###################################################################################################################
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
setwd("~/data/gfs/csv/pattern/gfsperday")

pathgfs="~/data/gfs/csv/pattern/gfsperday"
gfs_all_no_runs <- read.csv("~/data/gfs/csv/pattern/gfs_all_no_runs.csv",header=T)


setwd(gfspath)
gfs_filenames=list.files(path=pathgfs)
gfs_all=rbindlist(lapply(gfs_filenames, function(x){tryCatch(read.csv(x, header= T))}),fill=T)
#Step3
#filter storm 17.2m/s
getStorm=function(gfs_file_cutted){
  print("Step3")
  storm=filter(gfs_file_cutted, gfs_file_cutted$V7_MAX_DAY>17.1 )
  if(!(is.data.frame(storm) && nrow(storm)==0)){
    return(storm)
  }
  return("empty")
}

storm=getStorm(gfs_all_no_runs)
#Step5
getFalsePositive=function(storm_file){
  if(is.data.frame(storm_file) && nrow(storm_file)>0){
    
    print("Step5")
    n=nrow(storm_file)
    for (i in 1:n){
      if (storm_file$V7_MAX_DAY[i] >= storm_file$FX[i] && storm_file$FX[i]< 17.2)
        (storm_file$FalsePositve[i]=T)
      else (storm_file$FalsePositve[i]=F)
      
    }
    return(storm_file)
  }
  return("empty")
}

gfs_fp=getFalsePositive(storm)


write.csv(gfs_fp, "/home/m_meij02/data/gfs/csv/pattern/fp_all_noruns.csv")



###################################################################################################################
###################################################################################################################
#Detect false positives

#ico
#whole qeue

#libaries
library(dplyr)
library(readr)
library(data.table) #for multimerge function
library(tidyr)
library(Metrics)
#setwd
setwd("~/data/icon/csv/pattern/iconperday")

pathico="~/data/icon/csv/pattern/iconperday"
ico_all_no_runs <- read.csv("~/data/icon/csv/pattern/ico_all_no_runs.csv",header=T)

setwd(icopath)
ico_filenames=list.files(path=pathico)
ico_all=rbindlist(lapply(ico_filenames, function(x){tryCatch(read.csv(x, header= T))}),fill=T)
#Step3
#filter storm 17.2m/s
getStorm=function(ico_file_cutted){
  print("Step3")
  storm=filter(ico_file_cutted, ico_file_cutted$V7_MAX_DAY>17.1 )
  if(!(is.data.frame(storm) && nrow(storm)==0)){
    return(storm)
  }
  return("empty")
}

storm=getStorm(ico_all_no_runs)
#Step5
getFalsePositive=function(storm_file){
  if(is.data.frame(storm_file) && nrow(storm_file)>0){
    
    print("Step5")
    n=nrow(storm_file)
    for (i in 1:n){
      if (storm_file$V7_MAX_DAY[i] >= storm_file$FX[i] && storm_file$FX[i]< 17.2)
        (storm_file$FalsePositve[i]=T)
      else (storm_file$FalsePositve[i]=F)
      
    }
    return(storm_file)
  }
  return("empty")
}

ico_fp=getFalsePositive(storm)


write.csv(ico_fp, "/home/m_meij02/data/icon/csv/pattern/fp_all_noruns.csv")