#This script is intended to be copied and saved for each logger
#Filename for saving this script should be equivalent to fname_edits.R
# Set the filename - This should be what you see in the select station box in the shiny app
fname <- "2015_9918684_NA_NA_.Rdata"

#This won't need to change
path <- "//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/"

load(paste0(path, fname))

#When making adjustments for more than one range copy the code between 
# HERE and TO HERE and past it below in order to maintain a record of the
# changes you've made to the DQL. It would also be handy if you added a 
# little explanantion of why you are editing the DQL for that range
#HERE
#To adjust DQL for a specific date range
#First set the ranges you want to modify and the new DQL to assign
start_date_time_char <- "2015-06-02 12:07:00"
end_date_time_char <- "2015-10-06 13:15:00"
new_DQL <- 'A'

#Run these to actually update the file
start <- as.POSIXct(strptime(start_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
end <- as.POSIXct(strptime(end_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end, 'DQL'] <- new_DQL
#TO HERE

#When you have made all the edits run this line to save it back to the shiny app data folder
save(tmp_data, file = paste0(path, fname))
