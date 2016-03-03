#Set the path to where you want to put the graded data
out_path <- "//deqlead02/Vol_Data/Hylawoods/2015/"

#This shouldn't change
shiny_path <- "//deqlab1/wqm/Volunteer Monitoring/datamanagement/R/ContinuousDataReview/Check_shinyapp/data/"

#Gather filenames in the shiny folder
in_fnames <- list.files(shiny_path, full.names = TRUE)

#Sets the filenames based on what you've provided for the out_path
out_fnames <- paste0(out_path, list.files(shiny_path))

#Does the transfer
file.copy(from = in_fnames, to = out_fnames, overwrite = FALSE)

#Deletes the files in the shiny data folder
file.remove(in_fnames)
