source("../cleaning_packages.R")

# batch download
start_yr <- seq(1999, 2017, 2)
map(start_yr, get_nhanes_mort) 

mort_files <- list.files(pattern = "NHANES")
mort_raw <- map(mort_files, read_nhanes_mort) |> 
  list_rbind()

export(mort_raw, "mortality_raw.rds")



#=====update log file==========================================================

#write update message
message="
Initiate log.
"

#update log
update_log(file="log_mortality.txt",
           author="Peter T. Tanksley",
           message = message)
