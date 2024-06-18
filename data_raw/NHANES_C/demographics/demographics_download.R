source("../cleaning_packages.R")

# Check if the file exists
if (!file.exists("demographics_raw.rds")) {
  
  # Get waves (1999-2017)
  names_demo <- c("DEMO", paste0("DEMO_", LETTERS[2:10]))
  years_demo <- seq(1999, 2017, by=2)
  
  demo <- pull_nhanes(names_demo, years_demo, mismatch_regex = "DMD..SIZ|DMDHHSZ.")
  
  export(demo, "demographics_raw.rds")
} 



#=====update log file==========================================================

#write update message
message="
Downloaded raw demographic data for NHANES Continuous.
"

#update log
update_log(file="log_demographic_download.txt",
           author="Peter T. Tanksley",
           message = message)
