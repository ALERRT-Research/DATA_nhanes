source("../cleaning_packages.R")

#check checksum file
checksum_result <- check_md5_file("mortality_raw.rds")

if(!file.exists("mortality_raw.rds") || !checksum_result){
  
  #download file
  system("curl https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_III_MORT_2019_PUBLIC.dat \\
       > NHANES_III_MORT_2019_PUBLIC.dat")
  
  #read in mortality file using specified widths
  mort_raw <- read_nhanes_mort("NHANES_III_MORT_2019_PUBLIC.dat")
  
  #export
  export(mort_raw, "mortality_raw.rds")
  
  #create checksum
  create_md5_file("mortality_raw.rds")
  
}

# #=====update log file==========================================================
# 
# #write update message
# message="
# Added checksum procedure.
# "
# 
# #update log
# update_log(file="log_mortality_download.txt",
#            author="Peter T. Tanksley",
#            message = message)
