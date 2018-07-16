#get monthly and all rmse

wrf_all_no_runs=read.csv("~/Desktop/R/MA/wrf/pattern/falsepositives/fp_all_noruns_new_wrf.csv",header = T)

cos_all_no_runs=read.csv("~/Desktop/R/MA/cosmo/pattern/fp_all_noruns_new_cos.csv",header = T)

ico_all_no_runs=read.csv("~/Desktop/R/MA/icon/pattern/fp_all_noruns_new_ico.csv",header = T)

gfs_all_no_runs=read.csv("~/Desktop/R/MA/gfs/pattern/fp_all_noruns_new_gfs.csv",header = T)


######
#WRF
######
dates=wrf_all_no_runs$Predate
dates=dates[!duplicated(dates)]
#RMSE Mittelwerte fuer Monate
dates_jan=dates[1:13]
dates_feb=dates[14:41]
dates_mar=dates[42:72]
dates_apr=dates[73:102]

#subset wrf_data for each month 
wrf_all_jan=wrf_all_no_runs[wrf_all_no_runs$Predate %in% dates_jan, ] 
wrf_all_feb=wrf_all_no_runs[wrf_all_no_runs$Predate%in% dates_feb, ] 
wrf_all_mar=wrf_all_no_runs[wrf_all_no_runs$Predate %in% dates_mar, ] 
wrf_all_apr=wrf_all_no_runs[wrf_all_no_runs$Predate %in% dates_apr, ] 


write.csv(wrf_all_jan,"~/Desktop/R/MA/wrf/pattern/fp_wrf_monthly_jan.csv")
write.csv(wrf_all_feb,"~/Desktop/R/MA/wrf/pattern/fp_wrf_monthly_feb.csv")
write.csv(wrf_all_mar,"~/Desktop/R/MA/wrf/pattern/fp_wrf_monthly_mar.csv")
write.csv(wrf_all_apr,"~/Desktop/R/MA/wrf/pattern/fp_wrf_monthly_apr.csv")

##################################################################################################################################

##################################################################################################################################
######
#COSMO
######


#subset cos_data for each month 
cos_all_jan=cos_all_no_runs[cos_all_no_runs$Predate %in% dates_jan, ] 
cos_all_feb=cos_all_no_runs[cos_all_no_runs$Predate%in% dates_feb, ] 
cos_all_mar=cos_all_no_runs[cos_all_no_runs$Predate %in% dates_mar, ] 
cos_all_apr=cos_all_no_runs[cos_all_no_runs$Predate %in% dates_apr, ] 




write.csv(cos_all_jan,"~/Desktop/R/MA/cosmo/pattern/fp_cos_monthly_jan.csv")
write.csv(cos_all_feb,"~/Desktop/R/MA/cosmo/pattern/fp_cos_monthly_feb.csv")
write.csv(cos_all_mar,"~/Desktop/R/MA/cosmo/pattern/fp_cos_monthly_mar.csv")
write.csv(cos_all_apr,"~/Desktop/R/MA/cosmo/pattern/fp_cos_monthly_apr.csv")
##################################################################################################################################

##################################################################################################################################
######
#icon
######


#subset ico_data for each month 
ico_all_jan=ico_all_no_runs[ico_all_no_runs$Predate %in% dates_jan, ] 
ico_all_feb=ico_all_no_runs[ico_all_no_runs$Predate%in% dates_feb, ] 
ico_all_mar=ico_all_no_runs[ico_all_no_runs$Predate %in% dates_mar, ] 
ico_all_apr=ico_all_no_runs[ico_all_no_runs$Predate %in% dates_apr, ] 


write.csv(ico_all_jan,"~/Desktop/R/MA/icon/pattern/fp_ico_monthly_jan.csv")
write.csv(ico_all_feb,"~/Desktop/R/MA/icon/pattern/fp_ico_monthly_feb.csv")
write.csv(ico_all_mar,"~/Desktop/R/MA/icon/pattern/fp_ico_monthly_mar.csv")
write.csv(ico_all_apr,"~/Desktop/R/MA/icon/pattern/fp_ico_monthly_apr.csv")
##################################################################################################################################

##################################################################################################################################
######
gfs

#subset gfs_data for each month 
gfs_all_jan=gfs_all_no_runs[gfs_all_no_runs$Predate %in% dates_jan, ] 
gfs_all_feb=gfs_all_no_runs[gfs_all_no_runs$Predate%in% dates_feb, ] 
gfs_all_mar=gfs_all_no_runs[gfs_all_no_runs$Predate %in% dates_mar, ] 
gfs_all_apr=gfs_all_no_runs[gfs_all_no_runs$Predate %in% dates_apr, ] 




write.csv(gfs_all_jan,"~/Desktop/R/MA/gfs/pattern/fp_gfs_monthly_jan.csv")
write.csv(gfs_all_feb,"~/Desktop/R/MA/gfs/pattern/fp_gfs_monthly_feb.csv")
write.csv(gfs_all_mar,"~/Desktop/R/MA/gfs/pattern/fp_gfs_monthly_mar.csv")
write.csv(gfs_all_apr,"~/Desktop/R/MA/gfs/pattern/fp_gfs_monthly_apr.csv")