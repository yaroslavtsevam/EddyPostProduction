#setwd('Reddyproc')
library("REddyProc")
library("data.table")
library('plyr')
library("ggplot2")
library('openair')
library('ggthemes')
library('pastecs')
# Reading Data Function
read_eddy_data = function(data_path)
{
  setwd(data_path)
  file_list <- list.files( pattern="eddypro*")
  print("Found next eddypro result files:")
  print(file_list)
  for (file in file_list){

    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset = fread(file, header = "auto", sep = "auto")
      print(paste(paste("File",file, sep = " "), "read", sep=" "))
      if (names(temp_dataset)[1] != "filename") {
        print(temp_dataset)
        print("Whoops strange csv, may be you've allready edited it in Excel? Trying to fix")
          names(temp_dataset) = (as.character(temp_dataset[2, ]))
          print('Names gotten')
          temp_dataset = temp_dataset[4:length(temp_dataset[['date']]), ]
          print('Removing junk data')
          temp_dataset[['date']] = paste(substr(temp_dataset[['date']],7,10),substr(temp_dataset[['date']],4,5),substr(temp_dataset[['date']],1,2),sep="-")
          print('Constructing new date')
          inter = na.exclude(match(names(temp_dataset),names(dataset)))
          temp_dataset = temp_dataset[,inter, with = FALSE]
          dataset = data.table(dataset)[,inter, with = FALSE]
          print(temp_dataset)
        
      }
      #print(names(temp_dataset))
      stop_date_data = max(as.double(dataset[['DOY']]), na.rm=TRUE)
      print( paste("Stop date:", stop_date_data, sep=" "))
      start_date_temp = min(as.double(temp_dataset[['DOY']]), na.rm=TRUE)
      print( paste("Start date:", start_date_temp, sep=" "))
      if (start_date_temp < stop_date_data) {
         temp_dataset = temp_dataset[as.double(temp_dataset[['DOY']]) > stop_date_data ]
         print( paste("Rows read:",length(temp_dataset[['DOY']]), sep=" "))
      }
      print( paste("Rows read:",length(temp_dataset[['DOY']]), sep=" "))
      dataset = rbind.fill(dataset, temp_dataset)
      rm(temp_dataset)
    }
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      print(paste(paste("File",file, sep = " "), "read", sep=" "))
      dataset = data.table(fread(file, header = "auto", sep = "auto"))
      print( paste("Rows read:", length(dataset[['DOY']]), sep=" "))
    }
  }
  setwd('../')
  return (dataset)
}

read_biomet_data = function(data_path)
{
  setwd(data_path)
  file_list <- list.files( pattern="*.dat")

  for (file in file_list){

   # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset = fread(file, header = "auto", sep = "auto")
      dataset = rbind.fill(dataset, temp_dataset)
      rm(temp_dataset)
    }
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset = fread(file, header = "auto", sep = "auto")
    }

  }
  setwd('../')
  return (data.table(dataset))
}

# Fill gap by date

join_for_gapfilling = function(data_,biometdata_){
  data_[data_ == -9999.0] = NA
  biometdata_[['TIMESTAMP']] = as.POSIXct(as.POSIXlt(biometdata_[['TIMESTAMP']],format="%Y-%m-%d %H:%M:%S", tz="MSK"))

  hours = as.numeric(substr(data_[['time']],0,2))
  mins = as.numeric(substr(data_[['time']],4,6))
  mins[ mins > 0 ] = 0.5
  hours = mins+hours

  EddyData.F = data.frame(as.integer(substr(data_[['date']],0,4)),
                          as.integer(data_[['DOY']]),
                          hours,data_[['date']],
                          as.numeric(data_[['co2_flux']]) + as.numeric(data_[['co2_strg']]),
                          as.numeric(data_[['h2o_flux']]) + as.numeric(data_[['h2o_strg']]),
                          as.numeric(data_[['LE']]), as.numeric(data_[['H']]),
                          as.numeric(data_[['air_temperature']]) - 273,
                          as.numeric(data_[['RH']]),
                          as.numeric(data_[['VPD']]) / 1000000000,
                          as.numeric(data_[['u*']]),
                          as.numeric(data_[['wind_speed']]),
                          as.numeric(data_[['wind_dir']]),
                          as.numeric(data_[['x_70%']]),
                          as.numeric(data_[['x_90%']]),
                          as.numeric(data_[['qc_h2o_flux']]),
                          as.numeric(data_[['rand_err_h2o_flux']]),
                          as.numeric(data_[['rand_err_co2_flux']]),
                          as.numeric(data_[['qc_co2_flux']]))

  names(EddyData.F) = c('Year','DoY','Hour','date','NEE','H2O_NEE','LE','H','Tair','rH','VPD','Ustar','wind_speed','wind_dir','x_70%','x_90%','QF_h2o','rand_err_h2o_flux','rand_err_co2_flux','QF')
  EddyDataWithPosix.F <- fConvertTimeToPosix(na.exclude(EddyData.F), 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
  EddyDataWithPosixNew.F = fill_gap_by_date(EddyDataWithPosix.F,"DateTime",21)
  print("Filled gaps")
  setnames(biometdata_,'TIMESTAMP','DateTime')
  setkey(biometdata_,'DateTime')
  setkey(EddyDataWithPosixNew.F,'DateTime')

  joined = merge(biometdata_[,c(1,5,6,7,8,15,16,17,20,23,33,36,37,38,39,40,44,45,46,47,48,49,50,51,52,53,54,56,57), with=FALSE],EddyDataWithPosixNew.F, all=TRUE, by=c('DateTime'),allow.cartesian=TRUE)
  #join = EddyDataWithPosixNew.F[biometdata_[,c(1,5,6,7,8,15,16,17,20,23,33,36,37,38,39,40,44,45,46,47,48,49,50,51,52,53,54,56,57), with=FALSE],roll=FALSE]
  print('Joined')
  #Converting column names to satisfy REddyProc convention
  setnames(joined,"TSoil_1_Avg","Tsoil")
  joined[['Tsoil']]=as.numeric(joined[['Tsoil']])
  setnames(joined,"UpTot_Avg","Rg")
  joined[['Rg']]=as.numeric(joined[['Rg']])
  joined = joined[3:length(joined[,1, with=FALSE][[1]])]
  if ( length(which(duplicated(joined))) > 0 ) {
    print ("Found duplicated lines:")
    print( length(which(duplicated(joined))) )
    joined = joined[!which(duplicated(joined))]
  }
  print("Starting big Gap fill")
  joined.tf = fill_gap_by_date(joined,"DateTime",49)
  print('Stoped big gap fill')
  joined.tf[joined.tf == "NaN" | joined.tf == "NAN"] = NA
  #joined.tf[which(duplicated(joined.tf))] = NULL

  if ( length(which(is.na(joined.tf[['DateTime']]))) > 0 ) {
    joined.tf = joined.tf[!which(is.na(joined.tf[['DateTime']]))]
  }
  return(joined.tf)
}

fill_gap_by_date  = function(oldData,TimeVariableName,newDataNumberofColumns){
  mask = as.numeric(oldData[[TimeVariableName]][2:length(oldData[[TimeVariableName]])])-as.numeric(oldData[[TimeVariableName]][1:(length(oldData[[TimeVariableName]])-1)]) == 1800
  gap_length = 0
  NewData = oldData
  print("Starting iteration")
  #print(which(!mask))
  for (i in which(!mask))
  {
    #num_numeric_dates = seq(as.numeric(EddyDataWithPosix.F[['DateTime']][i]),as.numeric(EddyDataWithPosix.F[['DateTime']][i+1]), by = 1800)
    num_posix_dates = seq.POSIXt(oldData[[TimeVariableName]][(i)], oldData[[TimeVariableName]][(i+1)], by = as.difftime(30,units = "mins"))
    num_posix_dates = num_posix_dates[2:(length(num_posix_dates)-1)]
    gap = as.data.frame(cbind(num_posix_dates))
    gap[,2:newDataNumberofColumns] = "NA"
    #gap[,1] = as.POSIXlt(gap[,1], tz="GMT","1970-01-01 00:00:00")
    NewData = rbindlist(list(NewData[1:(i+gap_length),], gap, NewData[(i+1+gap_length):length(NewData[[TimeVariableName]]),]))
    gap_length = gap_length + length(num_posix_dates)
    print(paste(paste("Date gap ",i,"")," filled",""))
  }
  NewData[NewData == "NA"] = NA
  return (NewData)
}



footprint_for_angle = function(angle_v, ks_, bs_, xp_, yp_)
{
  typeof(angle_v)
  kp = tan((90 - angle_v)/180*pi)
  bp = yp_ - kp*xp_

  xc = (bs_ - bp)/(kp - ks_)
  yc = (kp*bs_ - bp*ks_)/(kp - ks_)

  if (is.na(angle_v)) {return(NA)}
  if (angle_v <= 180) {
    xcs = xc[xc >= xp_]
    ycs = yc[xc >= xp_]
  }

  if (angle_v > 180) {
    xcs = xc[xc <= xp_]
    ycs = yc[xc <= xp_]
  }

  footprint  = min(((ycs - yp_)^2+(xcs - xp_)^2)^.5)
  return(footprint)
}
max_footprints = function(site_point, s_polygon, alldata, wind_var)
{
  angle_vector = alldata[[wind_var]]
  xp = as.double(site_point[1]+ 0.0)
  yp = as.double(site_point[2]+ 0.0)
  index = 1:length(s_polygon[,1])
  shift_index = c( 2:length(s_polygon[,1]), 1)
  ks  = (s_polygon[,2][shift_index]-s_polygon[,2][index])/(s_polygon[,1][shift_index]-s_polygon[,1][index])
  bs = (s_polygon[,2][shift_index]*s_polygon[,1][index]-s_polygon[,2][index]*s_polygon[,1][shift_index])/(s_polygon[,1][index]-s_polygon[,1][shift_index])


  A_max_footprints = sapply(angle_vector, function(x) footprint_for_angle(x, ks, bs, xp, yp))
  return(alldata[, max_footprint:= A_max_footprints])
}


filter_by_quality = function(join_){
  print("Starting filtering")
  join_[join_ < -9000] = NA
  
  join_.sigma = sd(join_[['NEE']], na.rm = TRUE)
  join_.mean = mean(join_[['NEE']], na.rm = TRUE)
  print(join_.sigma)
  print(join_.mean)
  
  print("Going to filter out next amount of NEE data:")
  print(length(join_[['NEE']][join_[['NEE']] < join_.mean - 3*join_.sigma]))
  join_[['NEE']][join_[['NEE']] < join_.mean - 3*join_.sigma] = NA
  join_[['NEE']][join_[['NEE']] > join_.mean + 3*join_.sigma ]= NA
  join_[['NEE']][join_[['x_70%']] > join_[['max_footprint']] ] = NA
  join_[['NEE']][join_[['x_70%']] < 2 ] = NA
  join_[['NEE']][join_[['QF']] > 7 ]= NA
  join_[['H2O_NEE']][join_[['QF_h2o']] > 5 ]= NA
  join_[['H2O_NEE']][join_[['x_70%']] > join_[['max_footprint']]]= NA
  join_[['H20_NEE']][join_[['x_70%']] < 2 ]= NA

  return(join_)
}

reddyproc_gapfill = function(join_){
  EddyProc.C <- sEddyProc$new('Join', join_, c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'rH', 'VPD'))
  EddyProc.C$sMDSGapFill('NEE', FillAll.b=TRUE)
  EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('LE', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('H', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('rH', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('VPD', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('Tsoil', FillAll.b=FALSE)
  TempDT = data.table(EddyProc.C$sTEMP[c('sDateTime','NEE_f','Rg_f','LE_f','H_f','Tair_f','rH_f','VPD_f','Tsoil_f')])
  TempDT[[1]] = TempDT[[1]] + as.difftime(15, units="mins")
  setkey(TempDT,'sDateTime')
  setkey(join_,'DateTime')
  print('sDateTime')
  print(TempDT[['sDateTime']][1])
  print('DateTime')
  print(TempDT[['sDateTime']][1])
  cat ("Press [enter] to continue")
  line <- readline()
  FullData_ = join_[TempDT[,c('sDateTime','NEE_f','Rg_f','LE_f','H_f','Tair_f','rH_f','VPD_f','Tsoil_f'), with=FALSE],roll=FALSE]

  return(FullData_)
}
add_separators = function(FullData_, lat, lon, zone){
  doy = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%j"))
  dom = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%d"))
  hour =  as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%H"))
  month_name = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%B"))
  month_number = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%m"))
  week = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%U"))
  decimal_hour = as.numeric(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%H"))+as.numeric(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%M"))/60
  sunpos = fCalcSunPosition(doy, decimal_hour, lat, lon, zone)

  separations = data.table(data.frame( FullData_[['DateTime']],doy, dom, hour, month_name, month_number,week, sunpos[['SolTime']],sunpos[['SolDecl']],sunpos[['SolElev']]))
  names(separations) = c('sDateTime', 'Doy','DOM','hour','month_name','month_number','week','SolTime','SolDecl', 'SolElev')
  setkey(separations,'sDateTime')
  with_sep =  FullData_[separations[,c('sDateTime','Doy','DOM','hour','month_number','week','SolTime','SolDecl','SolElev'), with=FALSE],roll=FALSE]
  return(with_sep)
}


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

insert_event_mask = function(dt, datetime_column_name, event_start_date, event_stop_date, event_name){
  if (class(event_start_date)[1] == "POSIXct" && class(event_stop_date)[1] == "POSIXct") {
    event_mask = dt[[datetime_column_name]] > event_start_date & dt[[datetime_column_name]] < event_stop_date
    dt = data.table(dt)
    new_dt = dt[, event_name:= event_mask]
    setnames(new_dt, 'event_name',event_name)
    return (new_dt)
  }
  else
  {
    return (NULL)
  }
}

add_events = function(events_file, allData, DateVarName){
  with_events = allData
  allevents = fread(events_file, header = "auto", sep = "auto")
  for (i in 1:length(allevents[,1, with=FALSE][[1]])){
    start_date = as.POSIXct(as.character(allevents[i,1, with =FALSE]))
    stop_date  = as.POSIXct(as.character(allevents[i,2, with =FALSE]))
    with_events = insert_event_mask(with_events,DateVarName,start_date,stop_date,as.character(allevents[i,3, with =FALSE]))
  }
  return(with_events)
}
#+++ Aligning biomet and eddypro data to data.table

#================================================================================

FullEddyPostProcess = function(DataFolder,SiteUTM,SitePolygon,events_file,SiteCoordZone){
  # Reading Data
  data = read_eddy_data(DataFolder)
  biometdata = read_biomet_data(DataFolder)

  # Forming data set for gap filling
  joined_data = join_for_gapfilling(data, biometdata)

  #Generating column of max footprints
  joined_data = max_footprints(SiteUTM, SitePolygon, joined_data,'wind_dir')
  # Pre Gap filling Filtering
  join.filtered = filter_by_quality(joined_data)

  #+++ Fill gaps in variables with MDS gap filling algorithm
  # Creating DataTable with filled and biomet data
  FullData  = reddyproc_gapfill(join.filtered)
  #Adding time periods
  FullData_with_Sep = add_separators(FullData,SiteCoordZone[1],SiteCoordZone[2],SiteCoordZone[3] )
  #Read event filed and add event's masks
  WithEvents = add_events(events_file,FullData_with_Sep,'DateTime')
  # WindRose
  PlotWindRoses(FullData, 'wind_speed', 'wind_dir')
  return(WithEvents)
}
#package.skeleton(list = c("read_eddy_data","read_biomet_data","join_for_gapfilling","max_footprints","filter_by_quality","reddyproc_gapfill","add_separators","add_events","PlotWindRoses","footprint_for_angle","fill_gap_by_date"), name = "EddyPostProcess")




#write.csv(AllData_A, file="Site_A_all.csv")
#write.csv(Combined, file="Two_towers.csv")





