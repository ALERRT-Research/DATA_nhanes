source("../cleaning_packages.R")

#check checksums
checksum_result <- check_md5_file("mortality_clean.rds")

if(!file.exists("mortality_clean.rds") || !checksum_result){
  
  #=====NHANES Continuous========================================================
  
  mort_nhanesc_raw <- import("../../data_raw/NHANES_C/mortality/mortality_raw.rds")
  
  mort_nhanesc <- mort_nhanesc_raw |> 
    mutate(study = "NHANES Continuous")
  
  
  #=====NHANES III===============================================================
  
  mort_nhanes3_raw <- import("../../data_raw/NHANES_III/mortality/mortality_raw.rds")
  
  mort_nhanes3 <- mort_nhanes3_raw |> 
    mutate(study = "NHANES 3")
  
  
  #=====Combine, Recode, Label===================================================
  
  mort_all <- bind_rows(mort_nhanesc, mort_nhanes3) |> 
    mutate(study_id = case_when(
      (study=='NHANES 3') ~ paste0('3', str_pad(SEQN, width=6, side='left', pad='0')),
      (study!='NHANES 3') ~ paste0('4', str_pad(SEQN, width=6, side='left', pad='0'))))
  
  #recodes
  mort_all_recode <- mort_all |> 
    select(study_id, SEQN, study,
           elig_stat = eligstat,
           mort_stat = mortstat,
           ucod_leading,
           diabetes,
           hyperten,
           permth_int,
           permth_exm) |> 
    mutate(elig_stat = fct_recode(as.factor(elig_stat), "eligible"="1", "ineligible, minor" = "2", "ineligible"="3")) |> 
    mutate(mort_stat = fct_recode(as.factor(mort_stat), "assumed alive" = "0", "assumed deceased" = "1")) |> 
    mutate(ucod_leading = fct_recode(as.factor(ucod_leading),
                                     "heart disease"                             = "1",
                                     "malignant neoplasms"                       = "2",
                                     "chronic lower respiratory disease"         = "3",
                                     "accidents (unintentional injuries)"        = "4",
                                     "cerebrovascular diseases"                  = "5",
                                     "Alzhaimer's disease"                       = "6",
                                     "diabetes"                                  = "7",
                                     "influenza & pneumonia"                     = "8",
                                     "Nephritis, nephrotic syndrome & nephrosis" = "9",
                                     "All other causes (residual)"               = "10")) |> 
    mutate(diabetes = fct_recode(as.factor(diabetes), "no" = "0", "yes" = "1")) |> 
    mutate(hyperten = fct_recode(as.factor(hyperten), "no" = "0", "yes" = "1")) |> 
    mutate(cod_multi = case_when((diabetes == "yes" & hyperten == "yes") ~ "biabetes & hypertension",
                                 (diabetes == "yes" & hyperten != "yes") ~ "biabetes",
                                 (diabetes != "yes" & hyperten == "yes") ~ "hypertension",
                                 (diabetes == "no"  & hyperten == "no")  ~ "neither",
                                 TRUE ~ NA_character_),
           cod_multi = fct_infreq(cod_multi))
  
  #add labels
  mort_all_labs <- mort_all_recode |> 
    var_labels(elig_stat    = "elibibility status (sufficient identifying info)",
               mort_stat    = "mortality status (end of year, 2019)", 
               ucod_leading = "leading underlying cause of death",
               diabetes     = "diabetes flag from multiple causes of death",
               hyperten     = "hypertension flag from multiple causes of death",
               cod_multi    = "combined diabetes/hypertension flag",
               permth_int   = "person months from interview (until end of year, 2019)",
               permth_exm   = "person months from exam (until end of year, 2019)")
  
  #export
  export(mort_all_labs, "mortality_clean.rds")
  
  #create checksum file
  create_md5_file("mortality_clean.rds")
  
}

# #=====update log file==========================================================
# 
# #write update message
# message='
# Added checksum procedure for combined mortality data.
# '
# 
# #update log
# update_log(file='log_mortality.txt',
#            author='Peter T. Tanksley',
#            message = message)
