source("../cleaning_packages.R")

#download file
system("curl https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_III_MORT_2019_PUBLIC.dat \\
       > NHANES_III_MORT_2019_PUBLIC.dat")

mort_raw <- read_nhanes_mort("NHANES_III_MORT_2019_PUBLIC.dat")

export(mort_raw, "mortality_raw.rds")

#=====update log file==========================================================

#write update message
message="
Added raw mortality data from NHANES3.
"

#update log
update_log(file="log_mortality.txt",
           author="Peter T. Tanksley",
           message = message)
