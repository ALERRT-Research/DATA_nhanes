source("../cleaning_packages.R")

#get codebook for all questionnaire data files
# quest_codebook <- nhanes_table("https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Questionnaire&Cycle=")

#check checksums
checksum_result <- check_md5_file("medication_raw.rds")

# Check if the file exists
if (!file.exists("medication_raw.rds") || !checksum_result) {
  
  # Get waves (1999-2017)
  names <- c("RXQ_RX", paste0("RXQ_RX_", LETTERS[2:10]))
  years <- seq(1999, 2017, by=2)
  
  meds <- pull_nhanes(names, years#, mismatch_regex = ""
                      )
  
  
  
  #export data
  export(meds, "medication_raw.rds")
  
  #create/update checksum file
  create_md5_file("medication_raw.rds")
} 



#=====update log file==========================================================

#write update message
message="
Initiate log.
"

#update log
update_log(file="log_medication_download.txt",
           author="Peter T. Tanksley",
           message = message)
