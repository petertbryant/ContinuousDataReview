#To edit a data file
load(fname)

#To adjust DQL for a specific date rane
start_date_time_char <- "Y-m-d H:M:S"
end_date_time_char <- "Y-m-d H:M:S"
new_DQL <- 'A'

start <- as.POSIXct(strptime(start_date_time_char, format = "%Y-%m-%d %H:%M:%S"))
end <- as.POSIXct(strptim(end_date_time_char, format = "%Y-%m-%d %H:%M:%S"))

tmp_data[tmp_data$DATETIME >= start & tmp_data$DATETIME <= end, 'DQL'] <- new_DQL