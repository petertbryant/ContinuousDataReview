#Designate the folder where you are getting the .Rdata file and saving the .csv
in_out_path <- '//deqlead02/Vol_Data/Hylawoods/2015'

#Get the names for the .Rdata graded temperature files
list.files(path = in_out_path, pattern = ".R[Dd]ata")

