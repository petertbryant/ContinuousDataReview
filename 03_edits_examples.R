#This script is intended to be copied and saved for each logger
#Filename for saving this script should be equivalent to fname_edits.R
# Set the filename - This should be what you see in the select station box in the shiny app
fname <- "0020_99990_10429625_DO_20150420_Salmon R Hatchery_1_.Rdata"

#This won't need to change
path <- "//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/"

# This is the directory of the original Excel files and is where this script is saved at the end
EditPath <- '//deqlead02/Vol_Data/salmon-drift/2015/'

load(paste0(path, fname))




#When making adjustments for more than one range copy the code between 
# HERE and TO HERE and past it below in order to maintain a record of the
# changes you've made to the DQL. It would also be handy if you added a 
# little explanantion of why you are editing the DQL for that range

# 1
#HERE
#To adjust DQL for a specific date range
#First set the ranges you want to modify and the new DQL to assign
start_date_time_char <- "2015-05-01 12:45:00"
end_date_time_char <- "2015-05-01 16:40:00"
new_DQL <- 'C'
# provide a brief justification if necessary.  Comment must be in ''  
cmnt <- 'Can you see me now?'

#Run these to actually update the file
start <- as.POSIXct(strptime(start_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
end <- as.POSIXct(strptime(end_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end, 'rDQL'] <- new_DQL
tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end, 'cmnt'] <- cmnt
#TO HERE


#When you have made all the edits run this line to save it back to the shiny app data folder
save(tmp_data, file = paste0(path, fname))


# Save this script with the original data ....this doesn't work yet so you'll need to use save as
#save(03_edits_examples.R*, file = (paste0(EditPath,unlist(strsplit(fname,split = '.',fixed = TRUE))[1],'EDITS.R')))

