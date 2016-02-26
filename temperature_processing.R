library(readxl)

#Set the data file path for processing
src_file <- '//deqlead02/Vol_Data/Hylawoods/2015/OriginalCopy_HylawoodsSummer2015.xls'

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
audits$DATETIME <- as.POSIXct(strptime(audits$datetime, format = "%Y-%m-%d %H:%M:%S"))

#Pull out the logger ids for looping through
loggers <- unique(smi$"Logger ID")

for (i in 1:length(loggers)) {
  tmp_data <- read_excel(src_file, sheet = as.character(loggers[i]), skip = 4)
  tmp_data <- tmp_data[!is.na(tmp_data$TEMP),]
  tmp_data$date_char <- strftime(tmp_data$DATE, format = "%Y-%m-%d", tz = 'GMT')
  tmp_data$time_char <- strftime(tmp_data$TIME, format = '%H:%M:%S', tz ='GMT')
  tmp_data$datetime <- paste(tmp_data$date_char, tmp_data$time_char)
  tmp_data$DATETIME <- as.POSIXct(strptime(tmp_data$datetime, format = "%Y-%m-%d %H:%M:%S"))
  
  dr_info <- audits[grep(loggers[i], audits$"LOGGER ID"),c('DATETIME','COMMENTS')]
  
  bath_dql <- unique(ppcheck[grep(loggers[i], ppcheck$"LOGGER ID"),'DATA QUALITY LEVEL'])
  if (length(bath_dql) > 1) {
    if ('C' %in% bath_dql) {
      bath_dql <- 'C'
    } else if ('B' %in% bath_dql) {
      bath_dql <- 'B'
    }
  }
  
  
}