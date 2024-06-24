source("../cleaning_packages.R")

#get codebook for all exam years
if(!file.exists("exam_codebook.csv")) {
  exam_codebook <- nhanes_table("https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Examination&Cycle=")
  export(exam_codebook, "exam_codebook.csv")
} else {
  exam_codebook <- import("exam_codebook.csv")
}

#conduct checksum check
checksum_result <- check_md5_file("exam_raw.rds")


if(!file.exists("exam_raw.rds") || !checksum_result) {
  
  #=====blood pressure=(READY)===================================================
  
  # 1999="BPX",
  # 2001="BPX_B",
  # 2003="BPX_C",
  # 2005="BPX_D",
  # 2007="BPX_E",
  # 2009="BPX_F",
  # 2011="BPX_G",
  # 2013="BPX_H",
  # 2015="BPX_I",
  # 2017="BPX_J"
  
  #set names
  names_bp <- c("BPX",
                "BPX_B",
                "BPX_C",
                "BPX_D",
                "BPX_E",
                "BPX_F",
                "BPX_G",
                "BPX_H",
                "BPX_I",
                "BPX_J")
  
  #set years
  years_bp <- seq(1999, 2017, 2)
  
  #BP data
  df_bp <- pull_nhanes(names_bp, years_bp, "BPAEN1")
  
  #=====Grip=(READY)=============================================================
  
  # 2011="MGX_G",
  # 2013="MGX_H",
  
  #set names
  names_grip <- c("MGX_G",
                  "MGX_H")
  
  #set years
  years_grip <- c(2011, 2013)
  
  #grip data
  df_grip <- pull_nhanes(names_grip, years_grip)
  
  #=====BMI=(READY)==============================================================
  
  # 1999="BMX",
  # 2001="BMX_B",
  # 2003="BMX_C",
  # 2005="BMX_D",
  # 2007="BMX_E",
  # 2009="BMX_F",
  # 2011="BMX_G",
  # 2013="BMX_H",
  # 2015="BMX_I",
  # 2017="BMX_J"
  
  #set names
  names_bmi <- c("BMX",
                 "BMX_B",
                 "BMX_C",
                 "BMX_D",
                 "BMX_E",
                 "BMX_F",
                 "BMX_G",
                 "BMX_H",
                 "BMX_I",
                 "BMX_J")
  
  #set years
  years_bmi <- seq(1999, 2017, 2)
  
  #bmi data
  df_bmi <- pull_nhanes(names_bmi, years_bmi, "BMIRECUM")
  
  #=====VO2max=(READY)===========================================================
  
  # 1999="CVX",
  # 2001="CVX_B",
  # 2003="CVX_C",
  
  
  #set names
  names_vo2 <- c("CVX",
                 "CVX_B",
                 "CVX_C")
  
  #set years
  years_vo2 <- c(1999, 2001, 2003)
  
  #vo2 data
  df_vo2 <- pull_nhanes(names_vo2, years_vo2)
  
  #=====Spirometry=()===========================================================
  
  # 2007="SPX_E",
  # 2009="SPX_F",
  # 2011="SPX_G",
  
  #set names
  names_spir <- c("SPX_E",
                  "SPX_F",
                  "SPX_G")
  
  #set years
  years_spir <- seq(2007, 2011, 2)
  
  #bmi data
  df_spir <- pull_nhanes(names_spir, years_spir, "SPQ070B")
  
  
  #=====Combine data=============================================================
  
  df_list <- list(df_bmi,
                  df_bp,
                  df_grip,
                  df_vo2,
                  df_spir
  )
  
  df_all <- reduce(df_list, full_join)
  
  #save clean dataframe
  export(df_all, "exam_raw.rds")
  
  #add checksum file 
  create_md5_file("exam_raw.rds")
    
}






#=====update log file==========================================================

#write update message
message="
Added conditional based on the matching of MD5 checksums.
"

#update log
update_log(file="log_exam_download.txt",
           author="Peter T. Tanksley",
           message = message)




