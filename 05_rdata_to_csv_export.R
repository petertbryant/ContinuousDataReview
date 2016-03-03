#Designate the folder where you are getting the .Rdata file and saving the .csv
in_out_path <- '//deqlead02/Vol_Data/Hylawoods/2015/'

#Get the names for the .Rdata graded temperature files
fnames <- list.files(path = in_out_path, pattern = ".R[Dd]ata")

#Set working directory
setwd(in_out_path)

#Save csv file for each fnames data frame
for (i in 1:length(fnames)) {
  load(fnames[i])
  if (grepl('AUDIT',fnames[i])) {
    write.csv(dr_info, file = gsub(".Rdata",".csv",paste0(in_out_path, fnames[i])), 
              row.names = FALSE)
  } else {
    write.csv(tmp_data, file = gsub(".Rdata",".csv",paste0(in_out_path, fnames[i])), 
            row.names = FALSE)
  }
}

