library(readxl)
library(plyr)

#Set the data file path for processing
src_file <- '//deqlead02/Vol_Data/Hylawoods/2015/OriginalCopy_HylawoodsSummer2015.xls'

#Set the output location where the shiny app can use it
save_dir <- '//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data'

#Grab the master info sheet that has the logger ids
smi <- read_excel(src_file, sheet = 'SiteMasterInfo', skip = 5)
smi <- smi[!is.na(smi$'Logger ID'),]

#Grab the PrePostResults for getting teh bath_dql
ppcheck <- read_excel(src_file, sheet = 'PrePostResults')
ppcheck <- ppcheck[!is.na(ppcheck$'LOGGER ID'),]

#Get the audit sheet which has the deploy and retrieval times as well
audits <- read_excel(src_file, sheet = 'FieldAuditResults')
audits <- audits[!is.na(audits$"LOGGER ID"),]
audits$date_char <- strftime(audits$DATE, format = "%Y-%m-%d", tz = 'GMT')
audits$time_char <- strftime(audits$TIME, format = '%H:%M:%S', tz ='GMT')
audits$datetime <- paste(audits$date_char, audits$time_char)
audits$AUDIT_DATETIME <- as.POSIXct(strptime(audits$datetime, format = "%Y-%m-%d %H:%M:%S"))

#Pull out the logger ids for looping through
loggers <- unique(smi$"Logger ID")

for (i in 1:length(loggers)) {
  tmp_data <- read_excel(src_file, sheet = as.character(loggers[i]), skip = 4)
  tmp_data <- tmp_data[!is.na(tmp_data$TEMP),]
  tmp_data$date_char <- strftime(tmp_data$DATE, format = "%Y-%m-%d", tz = 'GMT')
  tmp_data$time_char <- strftime(tmp_data$TIME, format = '%H:%M:%S', tz ='GMT')
  tmp_data$datetime <- paste(tmp_data$date_char, tmp_data$time_char)
  tmp_data$DATETIME <- as.POSIXct(strptime(tmp_data$datetime, format = "%Y-%m-%d %H:%M:%S"))
  
  dr_info <- audits[grep(loggers[i], audits$"LOGGER ID"),c('AUDIT_DATETIME','AUDIT RESULT','COMMENTS')]
  dr_info <- rename(dr_info, c('AUDIT RESULT' = 'AUDIT_RESULT'))
  # Set TRUE FALSE for before and after deployment
  tmp_data$dbf <- ifelse(tmp_data$DATETIME < dr_info[dr_info$COMMENTS == 'deployed',
                                                     'AUDIT_DATETIME'], FALSE, TRUE)
  tmp_data$raf <- ifelse(tmp_data$DATETIME > dr_info[dr_info$COMMENTS == 'retrieved',
                                                     'AUDIT_DATETIME'], FALSE, TRUE)
  
  # get the probe values at the time of the audit  
  obs.dt <- tmp_data[min(which(tmp_data$dbf)), c("DATETIME","TEMP")]
  
  obs.rt <- tmp_data[max(which(tmp_data$raf)) - 1, c("DATETIME","TEMP")]
  
  # Grading
  tmp_data$field_audit_grade <- NA
  
  # calc differences
  diff.d <- abs(dr_info[dr_info$COMMENTS == 'deployed', 
                        'AUDIT_RESULT'] - obs.dt$TEMP)
  diff.r <- abs(dr_info[dr_info$COMMENTS == 'retrieved', 
                        'AUDIT_RESULT'] - obs.rt$TEMP)
  
  # deploy grade
  obs.dt$AUDIT_GRADE <- ifelse(is.na(diff.d),"E",
                               ifelse(diff.d < 1.51,"A",
                                      ifelse(diff.d < 2.01,"B", "C")))
  
  # retrival grade
  obs.rt$AUDIT_GRADE <- ifelse(is.na(diff.r),"E",
                               ifelse(diff.r < 1.51,"A",
                                      ifelse(diff.r < 2.01,"B", "C")))
  
  obs.dt$COMMENTS <- 'deployed'
  obs.rt$COMMENTS <- 'retrieved'
  dr_obs <- rbind(obs.dt[,c('DATETIME','TEMP','COMMENTS', 'AUDIT_GRADE')], 
                  obs.rt[,c('DATETIME','TEMP','COMMENTS', 'AUDIT_GRADE')])
  dr_obs <- rename(dr_obs, c('DATETIME' = "OBS_DATETIME",
                             'TEMP' = 'OBS_RESULT'))
  dr_info <- merge(dr_info, dr_obs, by = 'COMMENTS')
  
  # apply the grades 
  tmp_data[c(which(tmp_data$DATETIME <= dr_info[dr_info$COMMENTS == 'deployed', 
                                                'AUDIT_DATETIME']), 
             min(which(tmp_data$dbf))), "field_audit_grade"] <- obs.dt$AUDIT_GRADE
  #TODO: have field audit grade for second audit end at the audit datetime
  #this will matter for datasets that have audits that do not coincide with the retrieval
  tmp_data[which(tmp_data$DATETIME > dr_info[dr_info$COMMENTS == 'deployed', 
                                             'AUDIT_DATETIME']), 
           "field_audit_grade"] <- obs.rt$AUDIT_GRADE
  
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
  
  yr <- strftime(dr_info[dr_info$COMMENTS == 'deployed', 'AUDIT_DATETIME'], "%Y")
  lasar <- smi[smi$"Logger ID" == loggers[i], c('LASAR ID', "Station Description")]
  fname <- paste(yr, loggers[i], lasar$"LASAR ID", lasar$"Station Description", ".Rdata", sep = "_")
  fname <- gsub("/","_",fname)
  
  fname_audit <- paste(yr, loggers[i], lasar$"LASAR ID", 
                       lasar$"Station Description", "AUDIT_INFO.Rdata", sep = "_")
  fname_audit <- gsub("/","_",fname_audit)
  
  save(tmp_data, file = paste(save_dir, fname, sep = "/"))
  save(dr_info, file = paste(save_dir, fname_audit, sep = "/"))
  
  #   write.csv(tmp_data, file = paste(save_dir, fname, sep = "/"))
  #   write.csv(dr_info, file = paste(save_dir, fname_audit, sep = "/"))
}