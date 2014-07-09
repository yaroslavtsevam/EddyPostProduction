source(file="Eddy_postproduction.r", local=TRUE)
function = DrawVariablesbyMonth(){}


DataFolderA = 'Data_A/'
DataFolderB = 'Data_B/'
Site_A = c(410041, 6188869)
Site_B = c(410155, 6188893)
site_polygon  = data.frame(as.numeric(c(409957,410179,410243,410014)),as.numeric(c(6188984,6189058,6188849,6188774)))
forest_polygon  = data.frame(as.numeric(c(409472,409943,409532,408959,408587,408471,408353,408124,408048,408083,408088,408041,408038,408237,408524,408547,409090 ,409323,409550 ,409328,409535,409454)),as.numeric(c(6186623,6186720,6187669,6187536,6187517,6187632,6187882,6188235,6188193,6187844,6187575,6187405,6187222 ,6186987,6186310,6186133,6185574,6185407,6185467,6186180,6186267,6186542)))


events_A = 'Data_A/events.csv'
events_B = 'Data_B/events.csv'
Site_coord_and_zone = c(55.837631, 37.564302, 4)
AllData_A = FullEddyPostProcess (DataFolderA,Site_A,site_polygon,events_A,Site_coord_and_zone)
AllData_B = FullEddyPostProcess (DataFolderB,Site_B,site_polygon,events_B,Site_coord_and_zone)



#new_names = paste(names(AllData_A),"_a", sep="")
#names(AllData_A) = new_names
#new_names = paste(names(AllData_B),"_b", sep="")
#names(AllData_B) = new_names

#setnames(AllData_A,"DateTime_a","DateTime")
#setnames(AllData_B,"DateTime_b","DateTime")
#setkey(AllData_A,"DateTime")
#setkey(AllData_B,"DateTime")


#moving average for week



PlotWindRoses = function(EddyData, wind_speed, wind_dir)
{ windRose(EddyData, ws=wind_speed, wd=wind_dir)
  # one windRose for each year
  windRose(EddyData, ws=wind_speed, wd=wind_dir,type = "season")
  windRose(EddyData, ws=wind_speed, wd=wind_dir,type = "year")
  # windRose in 10 degree intervals with gridlines and width adjusted
  ## Not run:
  windRose(EddyData, ws=wind_speed, wd=wind_dir, angle = 10, width = 0.2, grid.line = 1)
  ## End(Not run)
  # pollutionRose of nox
  pollutionRose(EddyData, ws=wind_speed, wd=wind_dir, pollutant = "x_70%")
  ## source apportionment plot - contribution to mean
  ## Not run:
  pollutionRose(EddyData, ws=wind_speed, wd=wind_dir, pollutant = "x_70%", type = "season", statistic = "prop.mean")
}
ma  = function(x,n=7){
  filter(x,rep(1/n,n), sides=2)
}

daily_data = function(Data){
  new_names = c()
  for (name in names(Data)){
    if ((class(AllData_A[[name]])[1] == 'numeric' ) | (class(AllData_A[[name]])[1] == 'integer' ) | (class(AllData_A[[name]])[1] == 'character' )){  
      daily_sum = tapply(as.numeric(Data[[name]]), Data$Doy, sum, na.rm = TRUE)
      daily_mean = tapply(as.numeric(Data[[name]]), Data$Doy, mean, na.rm = TRUE)
      daily_error = tapply(as.numeric(Data[[name]]), Data$Doy, sd)
      
      daily_error = daily_error/sqrt(length(Data[[name]]))*1.96 
      
      if (exists('daily_sums')) {
        daily_sums = cbind(daily_sums, daily_sum)
        daily_means = cbind(daily_means, daily_mean)
        daily_errors = cbind(daily_errors, daily_error)
      } 
      else{
        daily_sums = data.frame(daily_sum)
        daily_means = data.frame(daily_mean)
        daily_errors = data.frame(daily_error)
      }
      new_names = c(new_names,name)
    }
  }
  names(daily_sums) = paste(new_names,"sums", sep="_")
  names(daily_means) = new_names
  names(daily_errors) = paste(new_names,"errors", sep="_")
  
   PAR_margin_for_night = 5
   Reco  = as.vector(by(Data[,c(16,52), with=FALSE], Data$Doy, function(x) mean(x[['NEE_f']][x[['PAR_Den_Avg']] < PAR_margin_for_night & x[['NEE_f']] > 0], na.rm=TRUE)*48 ))
   GPP  = as.vector(by(Data[,c(16,52), with=FALSE], Data$Doy, function(x) sum( mean(x[['NEE_f']][x[['PAR_Den_Avg']] < PAR_margin_for_night& x[['NEE_f']] < 0 ], na.rm=TRUE) - x[['NEE_f']][x[['PAR_Den_Avg']] > PAR_margin_for_night ] , na.rm=TRUE)))
  
  
  NA_count = tapply(as.numeric(Data[['NEE']]), Data$Doy, function(x) length(which(is.na(x))))
  NA_marker = NA_count
  
  NA_marker[NA_marker < 16] = 1
   
  NA_marker[NA_marker >= 16] = 0.1

#add posix dates
return(cbind(daily_sums, daily_means,daily_errors, NA_count,NA_marker, Reco, GPP))
#return(cbind(daily_sums, daily_means,daily_errors))
}


weekly_data = function(Data){
  new_names = c()
  for (name in names(Data)){
    if ((class(AllData_A[[name]])[1] == 'numeric' ) | (class(AllData_A[[name]])[1] == 'integer' ) | (class(AllData_A[[name]])[1] == 'character' )){  
      weekly_sum = tapply(as.numeric(Data[[name]]), Data$week, sum, na.rm = TRUE)
      weekly_mean = tapply(as.numeric(Data[[name]]), Data$week, mean, na.rm = TRUE)
      weekly_error = tapply(as.numeric(Data[[name]]), Data$week, sd) 
      weekly_error = weekly_error/sqrt(length(Data[[name]]))*1.96 
      
      if (exists('weekly_sums')) {
        weekly_sums = cbind(weekly_sums, weekly_sum)
        weekly_means = cbind(weekly_means, weekly_mean)
        weekly_errors = cbind(weekly_errors, weekly_error)
      } 
      else{
        weekly_sums = data.frame(weekly_sum)
        weekly_means = data.frame(weekly_mean)
        weekly_errors = data.frame(weekly_error)
      }
      new_names = c(new_names,name)
    }
  }
  names(weekly_sums) = paste(new_names,"sums", sep="_")
  names(weekly_means) = new_names
  names(weekly_errors) = paste(new_names,"errors", sep="_")
  return(cbind(weekly_sums, weekly_means, weekly_errors))
}
#redo like day
month_data = function(Data){
  mnew_names = c()
  for (name in names(Data)){
    if ((class(AllData_A[[name]])[1] == 'numeric' ) | (class(AllData_A[[name]])[1] == 'integer' ) | (class(AllData_A[[name]])[1] == 'character' )){  
      month_sum = tapply(as.numeric(Data[[name]]), Data$month_number, sum, na.rm = TRUE )
      month_mean = tapply(as.numeric(Data[[name]]), Data$month_number, mean, na.rm = TRUE)
      month_error = tapply(as.numeric(Data[[name]]), Data$month_number, sd) 
      month_error = month_error/sqrt(length(Data[[name]]))*1.96
      
      if (exists('month_sums')) {
        month_sums = cbind(month_sums, month_sum)
        month_means = cbind(month_means, month_mean)
        month_errors = cbind(month_errors, month_error)
      } 
      else{
        month_sums = data.frame(month_sum)
        month_means = data.frame(month_mean)
        month_errors = data.frame(month_error)
      }
      
      mnew_names = c(mnew_names,name)
    }
  }

  names(month_sums) = paste(mnew_names,"sums", sep="_")
  names(month_means) = mnew_names
  names(month_errors) = paste(mnew_names,"errors", sep="_")
  #add posix dates
  return(cbind(month_sums, month_means, month_errors))
}
###Hourly by month
hourly_data = function(AllData){
  hour_means = c()
  hour_errors = c()
  hour_months  = c()

  for (m in 1:12) {
    
   Data = AllData[AllData[['month_number']] == m,]
  
   hour_mean = tapply(Data[['NEE_f']], Data$hour, mean)
   hour_error = tapply(Data[['NEE_f']], Data$hour, sd)
   hour_error = hour_error/sqrt(length(hour_error))*1.96
   hour_month = rep(m, length(hour_error))
   hour_means = c(hour_means, hour_mean)
   hour_errors = c(hour_errors, hour_error)
   hour_months  = c(hour_months, hour_month)  

  }
  hour = as.integer(names(hour_mean))
   return(data.frame(cbind(hour_means,hour_errors,hour_months,hour)))
}


hourly_NEE_period = function(AllData,start_date,stop_date){
  hour_means = c()
  hour_errors = c()
  hour_months  = c()
   
    AllData = AllData[(AllData[['DateTime']] > as.POSIXct(start_date) & AllData[['DateTime']] < as.POSIXct(stop_date)),]
    nmax = max(na.exclude(AllData[['month_number']]))
    nmin = min(na.exclude(AllData[['month_number']]))
  for (m in nmin:nmax) {
    
    Data = AllData[AllData[['month_number']] == m,]   
  
    hour_mean = tapply(Data[['NEE_f']], Data$hour, mean)
    hour_error = tapply(Data[['NEE_f']], Data$hour, sd)
    hour_error = hour_error/sqrt(length(hour_error))*1.96
    hour_month = tapply(Data[['month_number']], Data$hour, mean)
    hour_means = c(hour_means, hour_mean)
    hour_errors = c(hour_errors, hour_error)
    hour_months  = c(hour_months, hour_month)  
    
  }
  hour = as.integer(names(hour_mean))
  return(data.frame(cbind(hour_means,hour_errors,hour_months,hour)))
}


hourly_data_for_event = function(AllData, event_name){
  hour_means = c()
  hour_errors = c()

    
    Data = AllData[AllData[[event_name]],]
    
    hour_mean = tapply(Data[['NEE_f']], Data$hour, mean)
    hour_error = tapply(Data[['NEE_f']], Data$hour, sd)
    hour_error = hour_error/sqrt(length(hour_error))*1.96

    hour_means = c(hour_means, hour_mean)
    hour_errors = c(hour_errors, hour_error)

    

  hour = as.integer(names(hour_mean))
  return(data.frame(cbind(hour_means,hour_errors,hour)))
}



PlotWindRoses(AllData_A, 'wind_speed', 'wind_dir')
NA_count = tapply(as.numeric(AllData_A['NEE']),AllData_A$Doy, function(x) x)

hourly_data_A = hourly_data(AllData_A)
hourly_data_B = hourly_data(AllData_B)
AllData_A_daily = daily_data(AllData_A)
AllData_B_daily = daily_data(AllData_B)
AllData_A_weekly = weekly_data(AllData_A)
AllData_B_weekly = weekly_data(AllData_B)
AllData_A_monthly = month_data(AllData_A)
AllData_B_monthly = month_data(AllData_B)
hourly_A_snow = hourly_NEE_period(AllData_A,"2013-01-01","2013-04-16")
hourly_A_16_04_5_05 = hourly_NEE_period(AllData_A,"2013-04-16","2013-05-05")
hourly_A_6_05_15_05 = hourly_NEE_period(AllData_A,"2013-05-05","2013-05-15")
hourly_A_15_05_10_06 = hourly_NEE_period(AllData_A,"2013-05-15","2013-06-10")
hourly_A_10_06_17_06 = hourly_NEE_period(AllData_A,"2013-06-10","2013-06-17")
hourly_A_17_06_24_06 = hourly_NEE_period(AllData_A,"2013-06-17","2013-06-24")
hourly_A_24_06_01_07 = hourly_NEE_period(AllData_A,"2013-06-24","2013-07-01")
hourly_A_01_07_08_07 = hourly_NEE_period(AllData_A,"2013-07-01","2013-07-08")
hourly_A_08_07_15_07 = hourly_NEE_period(AllData_A,"2013-07-08","2013-07-15")
hourly_A_15_07_25_07 = hourly_NEE_period(AllData_A,"2013-07-15","2013-07-25")
hourly_A_25_07_08_08 = hourly_NEE_period(AllData_A,"2013-07-25","2013-08-08")
hourly_A_08_08_31_13 = hourly_NEE_period(AllData_A,"2013-08-08","2013-12-31")
hourly_B_snow = hourly_NEE_period(AllData_B,"2013-01-01","2013-04-16")
hourly_B_16_04_5_05 = hourly_NEE_period(AllData_B,"2013-04-16","2013-05-05")
hourly_B_6_05_15_05 = hourly_NEE_period(AllData_B,"2013-05-05","2013-05-15")
hourly_B_15_05_10_06 = hourly_NEE_period(AllData_B,"2013-05-15","2013-06-10")
hourly_B_10_06_17_06 = hourly_NEE_period(AllData_B,"2013-06-10","2013-06-17")
hourly_B_17_06_24_06 = hourly_NEE_period(AllData_B,"2013-06-17","2013-06-24")
hourly_B_24_06_01_07 = hourly_NEE_period(AllData_B,"2013-06-24","2013-07-01")
hourly_B_01_07_08_07 = hourly_NEE_period(AllData_B,"2013-07-01","2013-07-08")
hourly_B_08_07_15_07 = hourly_NEE_period(AllData_B,"2013-07-08","2013-07-15")
hourly_B_15_07_25_07 = hourly_NEE_period(AllData_B,"2013-07-15","2013-07-25")
hourly_B_25_07_08_08 = hourly_NEE_period(AllData_B,"2013-07-25","2013-08-08")
hourly_B_08_08_31_13 = hourly_NEE_period(AllData_B,"2013-08-08","2013-12-31")
hourly_snow_cover_A = hourly_data_for_event(AllData_A,'snow_cover')
hourly_snow_cover_B = hourly_data_for_event(AllData_B,'snow_cover')
Daily_A_114 =  AllData_A_daily[AllData_A_daily[['Doy']] >114,]
Daily_B_114 =  AllData_B_daily[AllData_B_daily[['Doy']] >114,]
AllData_A_114 =  AllData_A[AllData_A[['Doy']] >114,]
Daily_A_114_b = Daily_A_114[Daily_A_114[['NA_count']]>47,]
Daily_B_114_b = Daily_B_114[Daily_B_114[['NA_count']]>47,]
AllData_B_114 =  AllData_B[AllData_B[['Doy']] >114,]
Weekly_A_114 =  AllData_A_weekly[AllData_A_weekly[['Doy']] >114,]
Weekly_A_90 =  AllData_A_weekly[AllData_A_weekly[['Doy']] >90,]
Weekly_B_114 =  AllData_B_weekly[AllData_B_weekly[['Doy']] >114,]
periods_a =c('hourly_A_snow','hourly_A_6_05_15_05','hourly_A_15_05_10_06','hourly_A_10_06_17_06','hourly_A_17_06_24_06','hourly_A_24_06_01_07','hourly_A_01_07_08_07','hourly_A_08_07_15_07','hourly_A_15_07_25_07','hourly_A_25_07_08_08','hourly_A_08_08_31_13') 
periods_b = c('hourly_A_snow','hourly_B_6_05_15_05','hourly_B_15_05_10_06','hourly_B_10_06_17_06','hourly_B_17_06_24_06','hourly_B_24_06_01_07','hourly_B_01_07_08_07','hourly_B_08_07_15_07','hourly_B_15_07_25_07','hourly_B_25_07_08_08','hourly_B_08_08_31_13')
  
############ NEE_f for two towers, hourly#########################

pd <- position_dodge(.1) # move them .05 to the left and right
hourly_data_As =hourly_data_A[hourly_data_A$hour_months > 1 & hourly_data_A$hour_months < 12,]
hourly_data_Bs =hourly_data_B[hourly_data_B$hour_months > 1 & hourly_data_B$hour_months < 12 ,]
ggplot() + 
  geom_errorbar(data =hourly_data_As, aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), linetype=2, width=.1, position=pd) +
  geom_line(data =hourly_data_As, aes(x=hour, y=hour_means),position=pd,size=.5, linetype=2) +
  geom_point(data = hourly_data_As, aes(x=hour, y=hour_means),position=pd,size=2, shape=21, fill="white")+
  geom_errorbar(data = hourly_data_Bs, aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors),linetype=2, width=.1, position=pd) +
  geom_line(data =hourly_data_Bs, aes(x=hour, y=hour_means),position=pd,size=.5) +
  geom_point(data = hourly_data_Bs, aes(x=hour, y=hour_means),position=pd,size=2, shape=21, fill="black")+
  geom_hline(yintercept = 0, linetype=2)+
  facet_wrap(~hour_months, ncol =3)+
  xlab("Time of day (Hour)")+
  ylab(expression(paste(bold("NEE")," ( ",mu,"mol "," ",CO[2]," ",m^-2," ",s^-1, " )",sep="")))+
#μmol CO2 m-2s-1)")+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold")) #+
  ggtitle("NEE_f for two towers, hourly")



############ NEE_f cumulation for two towers total#########################


ggplot() + 
  geom_line(data = Daily_A_114, aes(x=Doy, y=cumsum((NEE_f_sums * 12*18 /10000))), size=1, position=pd) +
  geom_line(data = Daily_B_114, aes(x=Doy, y=cumsum((NEE_f_sums * 12*18 /10000))),size=1, linetype =2, position=pd) +
  xlab("Day of the year ")+
  ylab(expression(paste(bold("Cumulative NEE")," ( g "," ",C[CO[2]]," ",m^-2," "," )",sep="")))+
  #μmol CO2 m-2s-1)")+
  geom_hline(yintercept = 0, linetype=2)+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold")) +
  ggtitle("NEE_f cumulation for two towers total")



############ NEE_f daily sums for all year ################################


ggplot() + 
  geom_line(data = Daily_A_114, aes(x=Doy, y=ma(NEE_f_sums * 12*18 /10000)), size=1, position=pd) +
  geom_line(data = Daily_B_114, aes(x=Doy, y=ma(NEE_f_sums* 12*18 /10000)),size=1, linetype =2, position=pd)+
  #geom_line(data =Weekly_A_114 , aes(x=Doy, y=NEE_f_sums * 12 * 18/70000),position=pd) +
  geom_point(data = Daily_A_114 , aes(x=Doy, y=NEE_f_sums* 12 * 18/10000),position=pd,size=2, shape=21, fill="white", alpha=Daily_A_114$NA_marker)+
  
  #geom_line(data =Weekly_B_114, aes(x=Doy, y=NEE_f_sums* 12 * 18/70000),position=pd, linetype = 2) +
  geom_point(data = Daily_B_114, aes(x=Doy, y=NEE_f_sums* 12 * 18/10000),position=pd,size=2, shape=21, fill="black", alpha=Daily_B_114$NA_marker)+
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  #geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)+
  xlab("Day of the year")+
  ylab(expression(paste(bold("NEE")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  scale_x_continuous(breaks = round(seq(120, max(Weekly_B_114$Doy), by = 50),1))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold")) 
  #ggtitle("NEE_f daily sums for all year ")

###### Reco
ggplot() + 
  geom_line(data = Daily_A_114, aes(x=Doy, y=ma(Reco * 12*18 /10000)), size=1, position=pd) +
  #geom_line(data =Weekly_A_114 , aes(x=Doy, y=NEE_f_sums * 12 * 18/70000),position=pd) +
  geom_point(data = Daily_A_114 , aes(x=Doy, y=Reco* 12 * 18/10000),position=pd,size=2, shape=21, fill="white")+
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  #geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)+
  xlab("Day of the year")+
  ylab(expression(paste(bold("Reco")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  scale_x_continuous(breaks = round(seq(120, max(Daily_B_114$Doy), by = 50),1))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold")) 
 # ggtitle("Reco daily sums for all year ")

###### GPP
ggplot() + 
  geom_line(data = Daily_A_114, aes(x=Doy, y=ma(GPP * 12*18 /10000)), size=1, position=pd) +
  geom_point(data = Daily_A_114 , aes(x=Doy, y=GPP* 12 * 18/10000),position=pd,size=2, shape=21, fill="white",)+ 
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  #geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)+
  xlab("Day of the year")+
  ylab(expression(paste(bold("GPP")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  scale_x_continuous(breaks = round(seq(120, max(Daily_B_114$Doy), by = 50),1))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold")) 
  #ggtitle("GPP daily sums for all year ")

#### For defined time gap ###############
pd <- position_dodge(1)
for (i in 1:length(periods_a))
  {
  #pdf(paste("period_",i,".pdf", sep=""))
  print(
    ggplot() + 
      geom_errorbar(data = get(periods_a[i]), aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), width=.5,alpha=.3, linetype=2) +
      geom_line(data =get(periods_a[i]), aes(x=hour, y=hour_means),linetype=2) +
      geom_point(data = get(periods_a[i]), aes(x=hour, y=hour_means),size=3, shape=21, fill="white")+
    geom_errorbar(data =get(periods_b[i]), aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), width=.5,alpha=.3, position=pd, linetype=2) +
    geom_line(data =get(periods_b[i]), aes(x=hour, y=hour_means),position=pd) +
    geom_point(data = get(periods_b[i]), aes(x=hour, y=hour_means),position=pd,size=3, shape=21, fill="black")+
      geom_vline(xintercept = 4, size=3, alpha=.2)+
      geom_hline(yintercept = 0, size=.5, linetype = 2)+
      facet_wrap(~hour_months)+
      xlab("Time of day (Hour)")+
      ylab(expression(paste(bold("NEE")," ( ",mu,"mol "," ",CO[2]," ",m^-2," ",s^-1, " )",sep="")))+
      #μmol CO2 m-2s-1)")+
      theme_few(base_size = 15, base_family = "serif")+
      theme(axis.title.y = element_text(size = 15, face="bold")) +
      theme(axis.title.x = element_text(size =15, face="bold"))+
      ggtitle(periods_a[i])
  )
  #dev.off()
}
#### Dependecies from factors  - useless for NEE, we need to deconstruct it###########

ggplot() + 
  
  geom_point(data = Daily_A_114 , aes(x=VWC_1_Avg*100, y=NEE_f_sums* 12 * 18/10000),position=pd,size=2, shape=21, fill="white")+
  
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  coord_cartesian(xlim = c(0, 50))+
  xlab("Soil water content (%)")+
  ylab(expression(paste(bold("NEE")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold")) +
  ggtitle("Dependecies from factors  - useless for NEE, we need to deconstruct it ")

#### Dependecies from factors  - useless for NEE, we need to deconstruct it###########

ggplot() + 
  
  geom_point(data = Daily_A_114 , aes(x=Tsoil_f, y=NEE_f_sums* 12 * 18/10000),position=pd,size=2, shape=21, fill="white")+
  
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  coord_cartesian(xlim = c(0, 50))+
  xlab("Temperature (C)")+
  ylab(expression(paste(bold("NEE")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold")) +
  ggtitle("Dependecies from factors  - useless for NEE, we need to deconstruct it ")

#########################################################


#### DRAW mean PAR

ggplot() +
  geom_line(data =  AllData_A_daily, aes(x=Doy, y=ma(PAR_Den_Avg)),position=pd) +
  geom_point(data = AllData_A_daily, aes(x=Doy, y=PAR_Den_Avg),position=pd,size=3, shape=21, fill="white")+

    xlab("Day of the year") + 
    ylab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep=""))) + 
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
  #ggtitle("DRAW mean PAR")

ggplot() +
  geom_line(data =  Daily_A_114, aes(x=Doy, y=ma(PAR_Den_Avg)),position=pd) +
  geom_point(data = Daily_A_114, aes(x=Doy, y=PAR_Den_Avg),position=pd,size=3, shape=21, fill="white")+
  coord_cartesian(xlim = c(80, 365))+
  xlab("Day of the year") + 
  ylab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep=""))) + 
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))+
  ggtitle("DRAW mean PAR")

temp  =  AllData_A[AllData_A[['DateTime']] > as.POSIXct("2013-12-02") & AllData_A[['DateTime']] < as.POSIXct("2013-12-03"), ]
ggplot() +
  geom_point(data =temp, aes(x=hour, y=SolElev), position=pd,size=3, shape=21, fill="white") +
  #coord_cartesian(ylim = c(0, 600)) +
  xlab("Day of the year") + 
  ylab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep=""))) + 
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
  
  
  
 # ggtitle("DRAW mean PAR")

#####Tsoil A and B


ggplot() +

  geom_line(data = Daily_A_114, aes(x=Doy, y=ma(Tsoil_f)),position=pd) +
  geom_point(data = Daily_A_114, aes(x=Doy, y=Tsoil_f),position=pd,size=3, shape=21, fill="white")+
 
  geom_line(data = Daily_B_114, aes(x=Doy, y=ma(Tsoil_f)),position=pd) +
  geom_point(data = Daily_B_114, aes(x=Doy, y=Tsoil_f),position=pd,size=3, shape=21, fill="black")+
  xlab("Day of the year") + 
  ylab(expression(bold(paste(T["soil"]," at 5cm depth "," (", ring("C"),")",sep="")))) +
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
  #ggtitle("Tsoil A and B")

#####Volumetric water content VWC A and B

ggplot() +  
  geom_line(data =  Daily_A_114, aes(x=Doy, y=ma(VWC_1_Avg*100)),position=pd) +
  geom_point(data = Daily_A_114, aes(x=Doy, y=(VWC_1_Avg*100)),position=pd,size=3, shape=21, fill="white")+
  coord_cartesian(xlim = c(110, 365),ylim = c(10, 40))+
  xlab("Day of the year") + 
  ylab(expression(bold(paste("SWC at 5cm depth (%)"," ",sep="")))) +
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))+
  ggtitle("Volumetric water content VWC A and BR")

##### Precipitation

ggplot() +  
  geom_rect(data =  Daily_A_114, aes(x=Doy,xmin=Doy-1,xmax=Doy+1, y=Rain_mm_Tot_sums, ymin=0, xmin=3),  position=pd, size=5) +
  coord_cartesian(xlim = c(110, 365))+
  xlab("Day of the year") + 
  ylab(expression(bold(paste("Precipitation (mm)"," ",sep="")))) +
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))+
  ggtitle("Precipitation")









----------------------------------




ggplot() + 
  geom_point(data = AllData_A[AllData_A[['DateTime']] < as.POSIXct("2013-08-01"),], aes(x = as.POSIXct(DateTime) , y = as.double(NEE_f), color=snow_cover  )) + 
  geom_point(data = AllData_B[AllData_B[['DateTime']] < as.POSIXct("2013-08-01"),], aes(x = as.POSIXct(DateTime) , y = as.double(NEE_f)   ), color='black') + 
  xlab("Дата") +
  ylab("NEE") + 
  ggtitle("Предварительный просмотр")


ggplot() + 
  geom_point(data = AllData_A, aes(x = as.POSIXct(DateTime), y = as.double(NEE_f), color="month_number"  )) + 
  xlab("Дата") +
  ylab("NEE_f") + 
  ggtitle("Предварительный просмотр")

ggplot() + 
  geom_point(data = AllData_B, aes(x = as.POSIXct(DateTime), y = as.double(NEE), color="month_number"  )) + 
  xlab("Дата") +
  ylab("NEE") + 
  ggtitle("Предварительный просмотр")

ggplot() + 
  geom_point(data = Combined, aes(x = as.POSIXct(DateTime), y = as.double(NEE_f_b), color=hour  )) + 
  xlab("Дата") +
  ylab("NEE_f") +  
  ggtitle("Предварительный просмотр")

ggplot() +
  geom_point(data = Combined, aes(x = DOM_a, y = NEE_f_a),size = 1) +
  facet_wrap(~month_number_a) + 
  xlab("Date") + 
  ylab("NEE") + 
  ggtitle("Two towers")

ggplot() +
  geom_point(data = Combined, aes(x = DOM_b, y = NEE_f_b),size = 1,color='red') +
  geom_point(data = Combined, aes(x = DOM_a, y = NEE_f_a),size = 1,color='green') +
  facet_wrap(~month_number_b) + 
  xlab("Date") + 
  ylab("NEE") + 
  ggtitle("Two towers")


ggplot() +
  geom_point(data = Combined, aes(x = hour, y = NEE_f_b,size = 1, color=spring_stubble_a)) +
  geom_point(data = Combined, aes(x = hour, y = NEE_f_a,size = 1, color=spring_stubble_a)) +
  facet_wrap(~month_number_b) + 
  xlab("Date") + 
  ylab("NEE") + 
  ggtitle("Two towers")

ggplot() +
  geom_point(data = Combined, aes(x = DOM_a, y = NEE_f_a,size = 1, color = 'green')) +
  geom_point(data = Combined, aes(x = DOM_a, y = NEE_f_b,size = 1, color = 'red')) +
  facet_grid(month_number_a~summer_vegetation_b) + 
  xlab("Date") + 
  ylab("NEE") + 
  ggtitle("Two towers")



ggplot() +
  geom_point( aes(x = as.numeric(names(dayly_NEE_sums_a)), y = dayly_NEE_sums_a,size = 1, color = 'green')) +
  geom_point(aes(x = as.numeric(names(dayly_NEE_sums_b)), y = dayly_NEE_sums_b,size = 1, color = 'red')) + 
  xlab("Date") + 
  ylab("NEE") + 
  ggtitle("Two towers")



ggplot() +
  geom_point(data=Combined, aes(x = DOM_a, y = dayly_NEE_sums_a,size = 1, color = 'green')) +
  geom_point(data=Combined, aes(x = DOM_a, y = dayly_NEE_sums_b,size = 1, color = 'red')) + 
  facet_wrap(~month_number_b) +
  xlab("Date") + 
  ylab("NEE") + 
  ggtitle("Two towers")
Combined[['dayly_NEE_sums_a']]



ggplot() +
  stat_boxplot(data = AllData_A, aes(factor(hour),  NEE_f))
geom_boxplot(data = AllData_A, aes(factor(hour),  NEE_f),size = 1, color = 'green') +
  stat_boxplot(data = AllData_B, aes(factor(hour),  NEE_f))
geom_boxplot(data = AllData_B, aes(factor(hour),  NEE_f),size = 1, color = 'red') +
  facet_wrap(~month_number) + 
  xlab("Date") + 
  ylab("NEE") + 
  ggtitle("Two towers") +
  theme_bw(base_size = 12, base_family = "")


#### Draw PAR per day - в китайской статье s-1 но написано, что это daily
dayly_PAR_sums_a <- tapply(as.numeric(AllData_A$PAR_Den_Avg), AllData_A$Doy, sum)

Dates = sapply(as.numeric(names(dayly_PAR_sums_a)),  function(x) min(as.numeric(AllData_A[['DateTime']][AllData_A[['Doy']] == x ])))
Dates = as.POSIXct(Dates, origin="1970-01-01")
ggplot() +
  geom_line( aes(x = Dates,  y = dayly_PAR_sums_a),size = 1, color = 'black') +
  xlab("Day of the year") + 
  ylab("PAR, umol m-2 d-1") + 
  theme_bw(base_size = 12, base_family = "") +
  ggtitle("Two towers")
