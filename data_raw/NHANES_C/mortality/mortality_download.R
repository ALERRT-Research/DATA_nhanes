source("../cleaning_packages.R")

#check checksum
checksum_result <- check_md5_file("mortality_raw.rds")


if(!file.exists("mortality_raw.rds") || !checksum_result){
  
  # batch download
  start_yr <- seq(1999, 2017, 2)
  map(start_yr, get_nhanes_mort) 
  
  #read in files and bind rows
  mort_files <- list.files(pattern = "NHANES")
  mort_raw <- map(mort_files, read_nhanes_mort) |> 
    list_rbind()
  
  #export data
  export(mort_raw, "mortality_raw.rds")
  
  #create checksum file
  create_md5_file("mortality_raw.rds")
}


# #=====update log file==========================================================
# 
# #write update message
# message="
# Added checksum procedure for NHC mortality data.
# "
# 
# #update log
# update_log(file="log_mortality.txt",
#            author="Peter T. Tanksley",
#            message = message)
