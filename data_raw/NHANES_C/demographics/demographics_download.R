source("../cleaning_packages.R")

#get codebook for all questionnaire data files
quest_codebook <- nhanes_table("https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Questionnaire&Cycle=")

#check checksums
checksum_result <- check_md5_file("demographics_raw.rds")

# Check if the file exists
if (!file.exists("demographics_raw.rds") || !checksum_result) {
  
  # Get waves (1999-2017)
  names_demo <- c("SMQ", paste0("SMQ_", LETTERS[2:10]))
  years_demo <- seq(1999, 2017, by=2)
  
  smok <- pull_nhanes(names_demo, years_demo, mismatch_regex = "SMD100LN|SMQ666K|SMQ665D"
                      )
  
  #export data
  export(demo, "demographics_raw.rds")
  
  #create/update checksum file
  create_md5_file("demographics_raw.rds")
} 



# #=====update log file==========================================================
# 
# #write update message
# message="
# Added checksum procedure.
# "
# 
# #update log
# update_log(file="log_demographic_download.txt",
#            author="Peter T. Tanksley",
#            message = message)
