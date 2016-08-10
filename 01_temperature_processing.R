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
#    Logger_ID
#    LASAR_ID
#    Station Description
#
#Expected format for PrePostResults:
#No header info. Column names in row 1.
#Columns to include in this exact format:
#    LOGGER_ID 
#    DATA QUALITY LEVEL
#Column LOGGER ID must at least contain the same logger ids as are in SiteMasterInfo
#
#Expected format for FieldAuditResults:
#No head info. Column names in row 1.
#Columns to include in this exact format:
#    LOGGER_ID
#    PARAMETER (DO,TEMP,COND,DOs,adjDO,TURB,Q,PH) 
#         each parameter reported must be listed regardless of the presence of audit info for parameter
#    DATE
#    TIME
#    AUDIT_RESULT
#    COMMENTS
#
#
#Expected format for worksheets with logger ID as their name:
#Header infro on rows 1-4. COlumn names in row 5.  Number of columns depends on the number of continuous parameters
#but there must always be one row with results and a second with data quality levels for each parameter.
#    DATE
#    TIME
#    PARAMETER_r : 'PARAMETER' must exactly match parameter listed in FieldAuditResults 
#    PARAMETER_DQL : 'PARAMETER' must exactly match parameter listed in FieldAuditResults
# repeat 
#Note: the last column name is DQL because TEMP is contained in row 4 which is a skipped row

#Set the data file path and file that you want to process
src_file <- '//deqlead02/Vol_Data/salmon-drift/2015/Fake2SDCWC2015CDO.xls'
SubID <- '0020' # Enter the submission ID from VolWQDB
#src_file <- '//deqlead02/Vol_Data/Hylawoods/2015/Hyla4R.xls'
#SubID <- 'YYYY' # Enter the submission ID from VolWQDB


#Set the output location where the shiny app can use it
save_dir <- '//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data'

#Grab the master info sheet that has the logger ids
capture.output(smi <- read_excel(src_file, sheet = 'SiteMasterInfo', skip = 5), file = "nul")
smi <- smi[!is.na(smi$Logger_ID),]
smi <- rename(smi, replace = c('Deployment_Depth_(meters)' = 'Depth_m'))
smi$Logger_ID <- gsub("\\..*","",smi$Logger_ID) # get rid of extraneous .000000
smi$Depth_m <- gsub("\\..*","",smi$Depth_m)
save(smi, file = paste(save_dir, paste0(SubID,"_SiteMasterInfo.RData"), sep = "/"))


#Grab the PrePostResults for getting teh bath_dql
capture.output(ppcheck <- read_excel(src_file, sheet = 'PrePostResults'), file = "nul")
ppcheck <- ppcheck[!is.na(ppcheck$LOGGER_ID),]
ppcheck$LOGGER_ID <- gsub("\\..*","",ppcheck$LOGGER_ID)

#Get the audit sheet which has the deploy and retrieval times as well
capture.output(audits <- read_excel(src_file, sheet = 'FieldAuditResults'), file = "nul")
audits <- audits[!is.na(audits$LOGGER_ID),]
audits$LOGGER_ID <- gsub("\\..*","",audits$LOGGER_ID)
audits$date_char <- strftime(audits$DATE, format = "%Y-%m-%d", tz = 'GMT')
audits$time_char <- strftime(audits$TIME, format = '%H:%M:%S', tz ='GMT')
audits$datetime <- paste(audits$date_char, audits$time_char)
audits$AUDIT_DATETIME <- as.POSIXct(strptime(audits$datetime, format = "%Y-%m-%d %H:%M:%S"))

logchar<-unique(audits[,c('LOGGER_ID','PARAMETER')])
names(logchar)<-c('log','char')

# Load the QC criteria for continuous data
load('//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/ConQC.RData')
#ConQC <- read.csv('//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ConQC.csv')


#Pull out the logger ids for looping through


for (i in seq_along(logchar$log)) {
  start.time <- Sys.time()
  print(paste("Starting file", i, "of", nrow(logchar), "=", logchar[i,1], logchar[i,2], start.time))
  capture.output(tmp_data <- read_excel(src_file, sheet = logchar[[i,1]], skip = 4), file = "nul")
  tmp_data <- tmp_data[,c(1,2,grep(paste0("^",logchar[i,2],"_"),names(tmp_data), ignore.case = TRUE))]
  tmp_data$charid <- logchar$char[i]
  
  names(tmp_data) <- c("DATE","TIME","r","dql", "charid")
  tmp_data <- tmp_data[!is.na(tmp_data$r),]
  tmp_data <- tmp_data[!is.na(tmp_data$DATE),]
  tmp_data$date_char <- strftime(tmp_data$DATE, format = "%Y-%m-%d", tz = 'GMT')
  tmp_data$time_char <- strftime(tmp_data$TIME, format = '%H:%M:%S', tz ='GMT')
  tmp_data$datetime <- paste(tmp_data$date_char, tmp_data$time_char)
  tmp_data$DATETIME <- as.POSIXct(strptime(tmp_data$datetime, format = "%Y-%m-%d %H:%M:%S"))
  
  
  # pull out audits based on logger id and characteristic/parameter
  dr_info <- audits[which(audits$LOGGER_ID %in% logchar$log[i]),c('AUDIT_DATETIME','PARAMETER', 'AUDIT_RESULT','COMMENTS')]
  dr_info <- dr_info[which(dr_info$PARAMETER %in% logchar$char[i]),]
  dr_info <- as.data.frame(dr_info)
  dr_info <- dr_info[order(dr_info$AUDIT_DATETIME), ]
  # Set TRUE FALSE for before and after deployment
  tmp_data$dbf <- ifelse(tmp_data$DATETIME < dr_info[1, 'AUDIT_DATETIME'], 
                         FALSE, TRUE)
  tmp_data$raf <- ifelse(tmp_data$DATETIME > dr_info[nrow(dr_info), 'AUDIT_DATETIME'], 
                         FALSE, TRUE)
  
  # get the probe values at the time of the audits  
  deploy_ind <- min(which(tmp_data$dbf))
  obs.dt <- tmp_data[deploy_ind, c("DATETIME","r")]
  
  retrieve_ind <- max(which(tmp_data$raf))
  obs.rt <- tmp_data[retrieve_ind, c("DATETIME","r")]
  
  # Grading
  tmp_data$field_audit_grade <- NA
  
  # Quality control calculation type
  qcad <- as.vector(ConQC$charid[which(ConQC$Qccal == 'AbsDiff')])
  qcpd <- as.vector(ConQC$charid[which(ConQC$Qccal == 'PerDiff')])
  
  # calc differences
  diff.d <- NA
  diff.r <- NA
  if (logchar[i,2] %in% qcad){
    diff.d <- abs(dr_info[1, 'AUDIT_RESULT'] - obs.dt$r)
    diff.r <- abs(dr_info[nrow(dr_info), 'AUDIT_RESULT'] - obs.rt$r)
  } else if (logchar[i,2] %in% qcpd){
    diff.d <- abs((dr_info[1, 'AUDIT_RESULT'] - obs.dt$r)/dr_info[1, 'AUDIT_RESULT'])
    diff.r <- abs((dr_info[nrow(dr_info), 'AUDIT_RESULT'] - obs.rt$r)/dr_info[nrow(dr_info), 'AUDIT_RESULT'])
  }
  
  
  # deploy grade
  pA <- ConQC$prec_A[which(ConQC$charid == logchar$char[i])]
  pB <- ConQC$prec_B[which(ConQC$charid == logchar$char[i])]
  obs.dt$AUDIT_GRADE <- ifelse(is.na(diff.d),"E",
                               ifelse(diff.d < pA,"A",
                                      ifelse(diff.d < pB,"B", "C")))
  
  # retrieval grade
  obs.rt$AUDIT_GRADE <- ifelse(is.na(diff.r),"E",
                               ifelse(diff.r < pA,"A",
                                      ifelse(diff.r < pB,"B", "C")))
  
  obs.dt$IND <- deploy_ind
  obs.rt$IND <- retrieve_ind
  dr_obs <- rbind(obs.dt[,c('DATETIME','r', 'AUDIT_GRADE', 'IND')], 
                  obs.rt[,c('DATETIME','r', 'AUDIT_GRADE', 'IND')])
  
  #Handling of additional audits
  if (nrow(dr_info) > 2) {
    dr_info_sub <- dr_info[2:(nrow(dr_info) - 1),]
    for (j in 1:nrow(dr_info_sub)) {
      audit_ind <- which.min(abs(tmp_data$DATETIME - 
                                   dr_info_sub[j, 'AUDIT_DATETIME']))
      tmp.obs <- tmp_data[audit_ind, c('DATETIME', 'r')]
      diff.a <- abs(dr_info_sub[j , 'AUDIT_RESULT'] - tmp.obs$r)
      tmp.obs$AUDIT_GRADE <- ifelse(is.na(diff.a),"E",
                                    ifelse(diff.a < pA,"A",
                                           ifelse(diff.a < pB,"B", "C")))
      tmp.obs$IND <- audit_ind
      dr_obs <- rbind(dr_obs, tmp.obs)
    }
  }
  
  dr_obs <- dr_obs[order(dr_obs$DATETIME), ]
  dr_obs <- rename(dr_obs, c('DATETIME' = "OBS_DATETIME",
                             'r' = 'OBS_RESULT'))
  dr_info <- cbind(dr_info, dr_obs)
  
  # apply the grades 
  for (k in 1:nrow(dr_info)) {
    start_ind <- ifelse(k == 1, dr_info$IND[1], dr_info$IND[k - 1] + 1)
    grade <- max(c(dr_info$AUDIT_GRADE[k-1],dr_info$AUDIT_GRADE[k]))# assign lower grade
    tmp_data[start_ind:dr_info$IND[k]-1, 
             'field_audit_grade'] <- grade
    tmp_data[dr_info$IND[k], 'field_audit_grade'] <- dr_info[k, 'AUDIT_GRADE']
  }
  
  #Determine what the DQL from the PrePostResult baths will be
  bath_dql <- ppcheck[grep(logchar[i,1], ppcheck$LOGGER_ID),c('LOGGER_ID', 'PARAMETER', 'DATA_QUALITY_LEVEL')] # this needs to be charid specific too
  bath_dql <- as.vector(unique(bath_dql[which(bath_dql$PARAMETER == logchar$char[i]),'DATA_QUALITY_LEVEL']))
  
  if (length(bath_dql) == 0) {
    bath_dql <- 'E'
  } else if (length(bath_dql) > 1) {
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
    # Final daily statistics should be calculated after the data has been graded...
      #these are preliminary used for anomaly considerations
  max <- aggregate(r~date, data=tmp_data, FUN=max)
  colnames(max)[2] <- "daily_max"
  min <- aggregate(r~date, data=tmp_data, FUN=min)
  colnames(min)[2] <- "daily_min"
  mean <- aggregate(r~date, data=tmp_data, FUN=mean)
  colnames(mean)[2] <- "daily_mean"
  
  day <- merge(x=min, y=mean,by="date",all.x=TRUE, all.y=TRUE)
  day <- merge(x=day, y=max, by="date", all.x=TRUE, all.y=TRUE)
  day$daily_diel <- day$daily_max - day$daily_min
  date <- seq(min(tmp_data$date), max(tmp_data$date), by=1)
  date_seq <- as.data.frame(date)
  day <- merge(x=date_seq,y=day, by="date",all.x=TRUE)
  
  # Find anamolies
  aDD <- ConQC$AnomDiel[which(ConQC$charid == logchar$char[i])]
  aDMax <- ConQC$AnomMax[which(ConQC$charid == logchar$char[i])]
  aDMin <- ConQC$AnomMin[which(ConQC$charid == logchar$char[i])]
  aDMnL <- ConQC$AnomMnL[which(ConQC$charid == logchar$char[i])]
  aDMnH <- ConQC$AnomMnH[which(ConQC$charid == logchar$char[i])]
  # if any of the anomolies are present report FALSE
  day$anomaly  <- (day$daily_diel > aDD | 
                     day$daily_max > aDMax | 
                     day$daily_min < aDMin | 
                     day$daily_mean < aDMnL | 
                     day$daily_mean > aDMnH)
  
  tmp_data <- merge(tmp_data,day,by="date",all.x=TRUE)
  
  # flag observations before deployment and after retreival date
  tmp_data[which(tmp_data$DATETIME < obs.dt$DATETIME | 
                   tmp_data$DATETIME > obs.rt$DATETIME),"anomaly"] <- NA
  
  #Set up final grade column to be verified using shiny app and further review
  tmp_data$rDQL <- ifelse(tmp_data$field_audit_grade == 'C' | tmp_data$bath_grade == 'C',
                          'C',
                          ifelse(tmp_data$field_audit_grade == 'B' | tmp_data$bath_grade == 'B',
                                 'B',
                                 ifelse(tmp_data$field_audit_grade == 'A' & tmp_data$bath_grade == 'A',
                                        'A',
                                        ifelse(tmp_data$field_audit_grade == 'E' & tmp_data$bath_grade == 'E',
                                               'E',
                                               'B'))))
  
  #Set pre and post deployment as NA for future clipping
  tmp_data$rDQL <- ifelse(tmp_data$dbf & tmp_data$raf, tmp_data$rDQL, NA)
  
  #Update the rDQL when the original file suggests a lower grade is appropriate
  tmp_data[which(tmp_data$dql %in% c('B','C','D') & 
             tmp_data$dql > tmp_data$rDQL), 
           'rDQL'] <- tmp_data[which(tmp_data$dql %in% c('B','C','D') & 
                                        tmp_data$dql > tmp_data$rDQL), 'dql']
  
  # anomaly check for provided dql not matching the calculated rDQL
  tmp_data$anomaly <- ifelse(is.na(tmp_data$dql) | is.na(tmp_data$rDQL), tmp_data$anomaly, 
                             ifelse(tmp_data$dql == tmp_data$rDQL, tmp_data$anomaly, FALSE))
  
  #Just keep the fields we want to persits
  tmp_data <- tmp_data[,c('DATETIME', 'charid','r', 'dql','rDQL', 'anomaly', 
                          'field_audit_grade', 'bath_grade','date', 'daily_min', 
                          'daily_max', 'daily_mean', 'daily_diel')]
  
  dploy <- strftime(dr_info[1, 'AUDIT_DATETIME'], "%Y%m%d")
  lasar <- smi[which(smi$Logger_ID == logchar[i,1]), c('LASAR_ID', 'Station_Description', 'Depth_m' )]
  fname <- paste(SubID, lasar$LASAR_ID, logchar[i,1], logchar[i,2], dploy, lasar$Station_Description, 
                 lasar$Depth_m, ".Rdata", sep = "_")
  fname <- gsub("/","_",fname) # not sure what this is for
  
  fname_audit <- paste(SubID, lasar$"LASAR_ID", logchar[i,1], logchar[i,2], dploy,  
                       lasar$Station_Description, "AUDIT_INFO.Rdata", sep = "_")
  fname_audit <- gsub("/","_",fname_audit)
  
  print(fname)
  print(fname_audit)
  cat('\n\n')
  
  save(tmp_data, file = paste(save_dir, fname, sep = "/"))
  save(dr_info, file = paste(save_dir, fname_audit, sep = "/"))
  
  rm("aDD","aDMax","aDMin","aDMnH", "aDMnL", "audit_ind", "bath_dql", "date", "date_seq", "day", 
     "deploy_ind", "diff.a", "diff.d", "diff.r", "dploy", "dr_info", "dr_info_sub", "dr_obs", "fname", "fname_audit",
     "lasar", "max",  "mean", "min", "obs.dt", "obs.rt",  "pA", "pB", "qcad", "qcpd", "retrieve_ind", "start_ind",
     "tmp.obs", "tmp_data", "grade")
  #   write.csv(tmp_data, file = paste(save_dir, fname, sep = "/"))
  #   write.csv(dr_info, file = paste(save_dir, fname_audit, sep = "/"))
}
