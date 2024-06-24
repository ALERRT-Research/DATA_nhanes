source("../cleaning_packages.R")

#check checksums
checksum_result <- check_md5_file("dexa_raw.rds")

if(!file.exists("dexa_raw.rds") || !checksum_result){
  
  #=====DEXA-Full body=(READY)===================================================
  
  #DEXA from 1999-2006 had high levels of missing data and only come in multiply
  #imputed format (x5 datasets). DEXA was not collected from 2007-2010.
  #Using non-imputed years for now.
  
  # 1999="",
  # 2001="",
  # 2003="",
  # 2005="",
  # 2007="",
  # 2009="",
  # 2011="DXX_G",
  # 2013="DXX_H",
  # 2015="DXX_I",
  # 2017="DXX_J"
  
  #set names
  names_dexa <- c("DXX_G",
                  "DXX_H",
                  "DXX_I",
                  "DXX_J")
  
  #set years
  years_dexa <- seq(2011, 2017, 2)
  
  #BP data
  df_dexa <- pull_nhanes(names_dexa, years_dexa)
  
  #recode 
  df_dexa_recodes <- df_dexa |> 
    select(SEQN, year,
           DXDTOLE,
           DXDTOFAT,
           DXDTOPF,
           DXDTOBMC,
           DXDTOTOT) 
  
  #=====DEXA-android/gynoid/VAT=(READY)========================================
  
  # 1999="",
  # 2001="",
  # 2003="",
  # 2005="",
  # 2007="",
  # 2009="",
  # 2011="DXXAG_G",
  # 2013="DXXAG_H",
  # 2015="DXXAG_I",
  # 2017="DXXAG_J"
  
  #set names
  names_dexa_ag <- c("DXXAG_G",
                     "DXXAG_H",
                     "DXXAG_I",
                     "DXXAG_J")
  
  #set years
  years_dexa_ag <- seq(2011, 2017, 2)
  
  #BP data
  df_dexa_ag <- pull_nhanes(names_dexa_ag, years_dexa_ag)
  
  #recode 
  df_dexa_ag_recodes <- df_dexa_ag |> 
    select(SEQN, 
           year,
           DXXGPFAT,
           DXXAPFAT,
           DXXAGRAT,
           DXXVFATV) 
  
  #=====merge and export=========================================================
  
  df_dexa_all <- df_dexa_recodes |> 
    full_join(df_dexa_ag_recodes, by=c("SEQN", "year"))
  
  export(df_dexa_all, "dexa_raw.rds")
  create_md5_file("dexa_raw.rds")
}

#=====update log file==========================================================

#write update message
message="
Added checksum procedure to NHC DEXA data.
"

#update log
update_log(file="log_DEXA_download.txt",
           author="Peter T. Tanksley",
           message = message)




