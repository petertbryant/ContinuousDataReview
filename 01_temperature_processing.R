library(readxl)
library(plyr)

#This script expects as input an excel file with worksheets named:
#    SiteMasterInfo
#    PrePostResults
#    FieldAuditResults
#    And a single sheet for each unique logger ID
#
#Expected format for SiteMasterInfo:
#Header info takes up rows 1 through 5 with column names in row 6
#Columns to include in this exact format:
#    Logger ID
#    LASAR ID
#    Station Description
#
#Expected format for PrePostResults:
#No header info. Column names in row 1.
#Columns to include in this exact format:
#    LOGGER ID 
#    DATA QUALITY LEVEL
#Column LOGGER ID must at least contain the same logger ids as are in SiteMasterInfo
#
#Expected format for FieldAuditResults:
#No head info. Column names in row 1.
#Columns to include in this exact format:
#    LOGGER ID
#    DATE
#    TIME
#    AUDIT RESULT
#    COMMENTS
#Column COMMENTS must contain 'deployed' or 'retrieved' as the only text in the field
#
#Expected format for worksheets with logger ID as their name:
#Header infro on rows 1-4. COlumn names in row 5.
#    DATE
#    TIME
#    TEMP
#    DQL
#Note: the last column name is DQL because TEMP is contained in row 4 which is a skipped row

#Set the data file path and file that you want to process
src_file <- '//deqlead02/Vol_Data/Hood River/2012/4R_2012_MFID_LL.xls'

#Set the output location where the shiny app can use it
save_dir <- '//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data'

#Grab the master info sheet that has the logger ids
capture.output(smi <- read_excel(src_file, sheet = 'SiteMasterInfo', skip = 5), file = "nul")
smi <- smi[!is.na(smi$'Logger ID'),]

#Grab the PrePostResults for getting teh bath_dql
capture.output(ppcheck <- read_excel(src_file, sheet = 'PrePostResults'), file = "nul")
ppcheck <- ppcheck[!is.na(ppcheck$'LOGGER ID'),]

#Get the audit sheet which has the deploy and retrieval times as well
capture.output(audits <- read_excel(src_file, sheet = 'FieldAuditResults'), file = "nul")
audits <- audits[!is.na(audits$"LOGGER ID"),]
audits$date_char <- strftime(audits$DATE, format = "%Y-%m-%d", tz = 'GMT')
audits$time_char <- strftime(audits$TIME, format = '%H:%M:%S', tz ='GMT')
audits$datetime <- paste(audits$date_char, audits$time_char)
audits$AUDIT_DATETIME <- as.POSIXct(strptime(audits$datetime, format = "%Y-%m-%d %H:%M:%S"))

#Pull out the logger ids for looping through
loggers <- unique(smi$"Logger ID")

for (i in 1:length(loggers)) {
  start.time <- Sys.time()
  print(paste("Starting logger", loggers[i], start.time))
  capture.output(tmp_data <- read_excel(src_file, sheet = as.character(loggers[i]), skip = 4), file = "nul")
  tmp_data <- tmp_data[!is.na(tmp_data$TEMP),]
  tmp_data$date_char <- strftime(tmp_data$DATE, format = "%Y-%m-%d", tz = 'GMT')
  tmp_data$time_char <- strftime(tmp_data$TIME, format = '%H:%M:%S', tz ='GMT')
  tmp_data$datetime <- paste(tmp_data$date_char, tmp_data$time_char)
  tmp_data$DATETIME <- as.POSIXct(strptime(tmp_data$datetime, format = "%Y-%m-%d %H:%M:%S"))
  
  dr_info <- audits[grep(loggers[i], audits$"LOGGER ID"),c('AUDIT_DATETIME','AUDIT RESULT','COMMENTS')]
  dr_info <- rename(dr_info, c('AUDIT RESULT' = 'AUDIT_RESULT'))
  dr_info <- as.data.frame(dr_info)
  dr_info <- dr_info[order(dr_info$AUDIT_DATETIME), ]
  # Set TRUE FALSE for before and after deployment
  tmp_data$dbf <- ifelse(tmp_data$DATETIME < dr_info[1, 'AUDIT_DATETIME'], 
                         FALSE, TRUE)
  tmp_data$raf <- ifelse(tmp_data$DATETIME > dr_info[nrow(dr_info), 'AUDIT_DATETIME'], 
                         FALSE, TRUE)
  
  # get the probe values at the time of the audits  
  deploy_ind <- min(which(tmp_data$dbf))
  obs.dt <- tmp_data[deploy_ind, c("DATETIME","TEMP")]
  
  retrieve_ind <- max(which(tmp_data$raf))
  obs.rt <- tmp_data[retrieve_ind, c("DATETIME","TEMP")]
  
  # Grading
  tmp_data$field_audit_grade <- NA
  
  # calc differences
  diff.d <- abs(dr_info[1, 'AUDIT_RESULT'] - obs.dt$TEMP)
  diff.r <- abs(dr_info[nrow(dr_info), 'AUDIT_RESULT'] - obs.rt$TEMP)
  
  # deploy grade
  obs.dt$AUDIT_GRADE <- ifelse(is.na(diff.d),"E",
                               ifelse(diff.d < 1.51,"A",
                                      ifelse(diff.d < 2.01,"B", "C")))
  
  # retrieval grade
  obs.rt$AUDIT_GRADE <- ifelse(is.na(diff.r),"E",
                               ifelse(diff.r < 1.51,"A",
                                      ifelse(diff.r < 2.01,"B", "C")))
  
  obs.dt$IND <- deploy_ind
  obs.rt$IND <- retrieve_ind
  dr_obs <- rbind(obs.dt[,c('DATETIME','TEMP', 'AUDIT_GRADE', 'IND')], 
                  obs.rt[,c('DATETIME','TEMP', 'AUDIT_GRADE', 'IND')])

  #Handling of additional audits
  if (nrow(dr_info) > 2) {
    dr_info_sub <- dr_info[2:(nrow(dr_info) - 1),]
    for (j in 1:nrow(dr_info_sub)) {
      audit_ind <- which.min(abs(tmp_data$DATETIME - 
                                   dr_info_sub[j, 'AUDIT_DATETIME']))
      tmp.obs <- tmp_data[audit_ind, c('DATETIME', 'TEMP')]
      diff.a <- abs(dr_info_sub[j , 'AUDIT_RESULT'] - tmp.obs$TEMP)
      tmp.obs$AUDIT_GRADE <- ifelse(is.na(diff.a),"E",
                                    ifelse(diff.a < 1.51,"A",
                                           ifelse(diff.a < 2.01,"B", "C")))
      tmp.obs$IND <- audit_ind
      dr_obs <- rbind(dr_obs, tmp.obs)
    }
  }
  
  dr_obs <- dr_obs[order(dr_obs$DATETIME), ]
  dr_obs <- rename(dr_obs, c('DATETIME' = "OBS_DATETIME",
                             'TEMP' = 'OBS_RESULT'))
  dr_info <- cbind(dr_info, dr_obs)
  
    # apply the grades 
  for (k in 1:nrow(dr_info)) {
    start_ind <- ifelse(k == 1, dr_info$IND[1], dr_info$IND[k - 1] + 1)
    tmp_data[start_ind:dr_info$IND[k], 
             'field_audit_grade'] <- dr_info[k, 'AUDIT_GRADE']
  }

  #Determine what the DQL from the PrePostResult baths will be
  bath_dql <- unique(ppcheck[grep(loggers[i], ppcheck$"LOGGER ID"),'DATA QUALITY LEVEL'])
  if (length(bath_dql) > 1) {
    if ('C' %in% bath_dql) {
      bath_dql <- 'C'
    } else if ('B' %in% bath_dql) {
      bath_dql <- 'B'
    }
  }
  
  #Apply the grade
  tmp_data[,"bath_grade"] <- bath_dql
  
  # Create a vector of daily dates for grouping
  tmp_data$date <- as.Date(tmp_data$DATETIME, format="%m/%d/%Y", tz="America/Los_Angeles")
  
  # Calculate the daily min and maximums
  tmax <- aggregate(TEMP~date, data=tmp_data, FUN=max)
  colnames(tmax)[2] <- "daily_max"
  tmin <- aggregate(TEMP~date, data=tmp_data, FUN=min)
  colnames(tmin)[2] <- "daily_min"
  tmean <- aggregate(TEMP~date, data=tmp_data, FUN=mean)
  colnames(tmean)[2] <- "daily_mean"
  
  day <- merge(x=tmin, y=tmean,by="date",all.x=TRUE, all.y=TRUE)
  day <- merge(x=day, y=tmax, by="date", all.x=TRUE, all.y=TRUE)
  day$daily_diel <- day$daily_max - day$daily_min
  date <- seq(min(tmp_data$date), max(tmp_data$date), by=1)
  date_seq <- as.data.frame(date)
  day <- merge(x=date_seq,y=day, by="date",all.x=TRUE)
  
  # Find anamolies
  day$anomaly  <- (day$daily_diel > 7 | 
                     day$daily_max > 25 | 
                     day$daily_mean < 8 | 
                     day$daily_mean > 20)
  
  tmp_data <- merge(tmp_data,day,by="date",all.x=TRUE)
  
  # flag observations before deployment and after retreival date
  tmp_data[which(tmp_data$DATETIME < obs.dt$DATETIME | 
                   tmp_data$DATETIME > obs.rt$DATETIME),"anomaly"] <- NA
  
  #Set up final grade column to be verified using shiny app and further review
  tmp_data$DQL <- ifelse(tmp_data$field_audit_grade == 'C' | tmp_data$bath_grade == 'C',
                         'C',
                         ifelse(tmp_data$field_audit_grade == 'B' | tmp_data$bath_grade == 'B',
                                'B',
                                ifelse(tmp_data$field_audit_grade == 'A' & tmp_data$bath_grade == 'A',
                                       'A',
                                       ifelse(tmp_data$field_audit_grade == 'E' & tmp_data$bath_grade == 'E',
                                              'E',
                                              'B'))))
  
  #Set pre and post deployment as NA for future clipping
  tmp_data$final_grade <- ifelse(tmp_data$dbf & tmp_data$raf, tmp_data$DQL, NA)
  
  #Just keep the fields we want to persits
  tmp_data <- tmp_data[,c('DATETIME', 'TEMP', 'DQL', 'anomaly', 
                          'field_audit_grade', 'bath_grade', 'daily_min', 
                          'daily_max', 'daily_mean', 'daily_diel')]
  
  yr <- strftime(dr_info[1, 'AUDIT_DATETIME'], "%Y")
  lasar <- smi[which(smi$"Logger ID" == loggers[i]), c('LASAR ID', "Station Description")]
  fname <- paste(yr, loggers[i], lasar$"LASAR ID", lasar$"Station Description", ".Rdata", sep = "_")
  fname <- gsub("/","_",fname)
  
  fname_audit <- paste(yr, loggers[i], lasar$"LASAR ID", 
                       lasar$"Station Description", "AUDIT_INFO.Rdata", sep = "_")
  fname_audit <- gsub("/","_",fname_audit)
  
  print(fname)
  print(fname_audit)
  cat('\n\n')
  
  save(tmp_data, file = paste(save_dir, fname, sep = "/"))
  save(dr_info, file = paste(save_dir, fname_audit, sep = "/"))
  
  #   write.csv(tmp_data, file = paste(save_dir, fname, sep = "/"))
  #   write.csv(dr_info, file = paste(save_dir, fname_audit, sep = "/"))
}