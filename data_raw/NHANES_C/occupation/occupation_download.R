source("../cleaning_packages.R")

#check checksums
checksum_result <- check_md5_file("occupation_raw.rds")


if(!file.exists("occupation_raw.rds") || !checksum_result) {
  
  
  #--------OCUPATIONAL CODES SWITCH ACROSS YEARS, CONFIRM CODING
  #--------READ IN CODEBOOK
  
  occ_codebook <- import("nhanes_occ_census_crosswalk.xlsx") |> 
    mutate(occ_desc = str_to_lower(occ_desc))
  
  #=====Occupation=(READY)=====================================================
  
  # 1999="OCQ",
  # 2001="OCQ_B",
  # 2003="OCQ_C",
  # 2005="OCQ_D",
  # 2007="OCQ_E",
  # 2009="OCQ_F",
  # 2011="OCQ_G",
  # 2013="OCQ_H",
  # 2015="OCQ_I",
  # 2017="OCQ_J"
  
  #=====BLOCK 1 (1999-2003)
  #set names
  names_ocq <- c("OCQ",
                 "OCQ_B",
                 "OCQ_C")
  
  #set years
  years_ocq <- seq(1999, 2003, 2)
  
  #BP data
  df_ocq_block1 <- pull_nhanes(names_ocq, years_ocq, "OCD231|OCD241|OCD392")
  
  #=====BLOCK 2 (2005)
  #set names
  names_ocq <- "OCQ_D"
  
  #set years
  years_ocq <- 2005
  
  #BP data
  df_ocq_block2 <- pull_nhanes(names_ocq, years_ocq)
  
  #=====BLOCK 3 (2007 onward)
  #set names
  names_ocq <- c("OCQ_E",
                 "OCQ_F",
                 "OCQ_G",
                 "OCQ_H",
                 "OCQ_I",
                 "OCQ_J")
  
  #set years
  years_ocq <- seq(2007, 2017, 2)
  
  #BP data
  df_ocq_block3 <- pull_nhanes(names_ocq, years_ocq, "OCD231|OCD241|OCD392")
  
  
  #=====Combine blocks================================================
  
  #block1
  df_ocq_block1_occ <- df_ocq_block1 |> 
    select(SEQN, 
           year, 
           employed= OCD150,
           unemployed_reason=OCQ380,
           occ_hrs=OCD180,
           occ_tenure_months=OCD270,
           occ_cur_code= OCD240, 
           occ_long_code= OCD390, 
           occ_long_months= OCD395) |> 
    left_join(occ_codebook |> filter(census_yr==1990) |> select(-census_yr), by=c("occ_cur_code"="occ_code")) |> 
    rename(occ_cur_desc = occ_desc) |> 
    left_join(occ_codebook |> filter(census_yr==1990) |> select(-census_yr), by=c("occ_long_code"="occ_code")) |> 
    rename(occ_long_desc = occ_desc) |> 
    mutate(occ_code_census_yr=1990)
  
  #block2
  df_ocq_block2_occ <- df_ocq_block2 |> 
    select(SEQN, 
           year, 
           employed= OCD150,
           unemployed_reason=OCQ380,
           occ_hrs=OCQ180,
           # occ_tenure_months=OCD270, #not available
           occ_cur_code= OCD241, 
           occ_long_code= OCD392, 
           occ_long_months= OCD395) |> 
    left_join(occ_codebook |> filter(census_yr==2000) |> select(-census_yr), by=c("occ_cur_code"="occ_code")) |> 
    rename(occ_cur_desc = occ_desc) |> 
    left_join(occ_codebook |> filter(census_yr==2000) |> select(-census_yr), by=c("occ_long_code"="occ_code")) |> 
    rename(occ_long_desc = occ_desc) |> 
    mutate(occ_code_census_yr=2000)
  
  #block3
  df_ocq_block3_occ <- df_ocq_block3 |> 
    select(SEQN, 
           year, 
           employed= OCD150,
           unemployed_reason=OCQ380,
           occ_hrs=OCQ180,
           occ_tenure_months=OCD270, 
           occ_cur_code= OCD241, 
           occ_long_code= OCD392, 
           occ_long_months= OCD395) |> 
    mutate(across(ends_with("code"), ~str_to_lower(.))) |> 
    left_join(occ_codebook |> filter(census_yr==2000) |> select(-census_yr), by=c("occ_cur_code"="occ_desc")) |> 
    rename(occ_cur_desc = occ_cur_code,
           occ_cur_code = occ_code) |> 
    left_join(occ_codebook |> filter(census_yr==2000) |> select(-census_yr), by=c("occ_long_code"="occ_desc")) |> 
    rename(occ_long_desc = occ_long_code,
           occ_long_code = occ_code) |> 
    mutate(occ_code_census_yr=2000)
  
  #combine
  df_ocq_recodes <- bind_rows(df_ocq_block1_occ, df_ocq_block2_occ, df_ocq_block3_occ) 
  
  
  #=====Export=================================================================
  
  #export
  export(df_ocq_recodes, "occupation_raw.rds")
  
  #create checksum file
  create_md5_file("occupation_raw.rds")
}

#=====update log file==========================================================

#write update message
message="
Added checksum procedure for NHC occupation data.
"

#update log
update_log(file="log_occupation_download.txt",
           author="Peter T. Tanksley",
           message = message)


