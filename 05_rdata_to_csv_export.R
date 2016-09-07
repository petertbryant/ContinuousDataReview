#Designate the folder where you are getting the .Rdata file and saving the .csv
in_out_path <- '//deqlead02/Vol_Data/salmon-drift/2015/CDO/SDC15cdo4R/'

#Get the names for the .Rdata graded files
fnames <- list.files(path = in_out_path, pattern = ".R[Dd]ata")
datfls <- fnames[grep("[^0-9]+._[^0-9]+_.Rdata",fnames)] # data files
audfls <- fnames[grep(".AUDIT_INFO.Rdata",fnames)] # audit files
svdfls <- fnames[grep(".RData", fnames)] # Saved files for volunteer db.


#Set working directory
setwd(in_out_path)

#  Save the continuous data files as csv files
for (i in 1:length(datfls)) {
  load(datfls[i])
  write.csv(tmp_data, file = gsub(".Rdata",".csv",paste0(in_out_path, datfls[i])), 
            row.names = FALSE)
  rm(tmp_data)
}

# Save the audit data files as csv files
for (i in 1:length(audfls)) {
  load(audfls[i])
  write.csv(dr_info, file = gsub(".Rdata",".csv",paste0(in_out_path, audfls[i])), 
            row.names = FALSE)
  rm(dr_info)
} 

# Save the other saved data files using regex to identify what the file is
for (i in 1:length(svdfls)){
  load(svdfls[i])
  print(svdfls[i])
  if (grepl(".SiteMasterInfo.RData", svdfls[i])){
    write.csv(smi, file = gsub(".RData",".csv",paste0(in_out_path, svdfls[i])), 
              row.names = FALSE)
    rm(smi)
  } else if (grepl("^[0-9]{4}ActGrp2Act.RData", svdfls[i])) {
    write.csv(t_ActGrp2Act, file = gsub(".RData",".csv",paste0(in_out_path, svdfls[i])), 
              row.names = FALSE)
    rm(t_ActGrp2Act)
  } else if (grepl("^[0-9]{4}Activity.RData", svdfls[i])) {
    write.csv(t_ConDatAct, file = gsub(".RData",".csv",paste0(in_out_path, svdfls[i])), 
              row.names = FALSE)
    rm(t_ConDatAct)
  } else if (grepl("^[0-9]{4}ActivityGrps.RData", svdfls[i])) {
    write.csv(t_ActGrp, file = gsub(".RData",".csv",paste0(in_out_path, svdfls[i])), 
              row.names = FALSE)
    rm(t_ActGrp)
  } else if (grepl(".+AuditActGrp2Act.RData", svdfls[i])) {
    write.csv(t_AudActGrp2Act, file = gsub(".RData",".csv",paste0(in_out_path, svdfls[i])), 
              row.names = FALSE)
    rm(t_AudActGrp2Act)
  } else if (grepl(".AuditActivity.RData", svdfls[i])) {
    write.csv(t_CnAudAct, file = gsub(".RData",".csv",paste0(in_out_path, svdfls[i])), 
              row.names = FALSE)
    rm(t_CnAudAct)
  } else if (grepl(".AuditActivityGrps.RData", svdfls[i])) {
    write.csv(t_AudActGrp, file = gsub(".RData",".csv",paste0(in_out_path, svdfls[i])), 
              row.names = FALSE)
    rm(t_AudActGrp)
  } else if (grepl("^[0-9]{4}Results.RData", svdfls[i])) {
    write.csv(t_CnRslt, file = gsub(".RData",".csv",paste0(in_out_path, svdfls[i])), 
              row.names = FALSE)
    rm(t_CnRslt)
  } else if (grepl("^[0-9]{4}.+Results.RData", svdfls[i])) {
    write.csv(t_CnAudRslt, file = gsub(".RData",".csv",paste0(in_out_path, svdfls[i])), 
              row.names = FALSE)
    rm(t_CnAudRslt)
  }
}


