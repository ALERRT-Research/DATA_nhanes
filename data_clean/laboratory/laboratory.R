
source("../cleaning_packages.R")

#NOTE: ALL SELECTED BIOMARKERS NEED TO BE CHECKED FOR ANALYTICAL CORRECTIONS WAVE-TO-WAVE
#Convention: ALWAYS PREFER --FORWARD-- DEMING EQUATIONS

#check checksums
checksum_result <- check_md5_file("laboratory_clean.rds")

if(!file.exists("laboratory_clean.rds") || !checksum_result){
  
  #=====NHANES Continuous========================================================
  
  lab_nhanesc <- import("../../data_raw/NHANES_C/laboratory/laboratory_raw.rds")
  
  #=====Standard biochemistry profile
  
  #ANALYTIC NOTE: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BIOPRO_J.htm
  
  lab_nhanesc_recodes <- lab_nhanesc |> 
    mutate(LBXSCR = ifelse(is.na(LBXSCR), LBDSCR, LBXSCR)) |> 
    select(-LBDSCR) |> 
    rename(chol_tot_mgdL = LBXSCH, #adjustment needed
           bun_mgdL      = LBXSBU, #adjustment needed
           creat_mgdL    = LBXSCR,  #adjustment needed
           sod_mmolL     = LBXSNASI) |> 
    #apply forward deming equations to years prior to 2017
    mutate(chol_tot_mgdL = case_when((year==2017) ~ chol_tot_mgdL,
                                     TRUE         ~ 0.9556*chol_tot_mgdL+2.105),
           bun_mgdL      = case_when((year==2017) ~ bun_mgdL,
                                     TRUE         ~ 0.9992*bun_mgdL+0.4484),
           creat_mgdL    = case_when((year==2017) ~ creat_mgdL,
                                     TRUE         ~ 0.9515*creat_mgdL+0.06608)) 
  
  
  #=====Glucose
  
  #ANALYTIC NOTES:
  # https://wwwn.cdc.gov/nchs/nhanes/2003-2004/L10AM_C.htm
  # https://wwwn.cdc.gov/nchs/nhanes/2005-2006/GLU_D.htm
  # https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/GLU_E.htm
  # https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/GLU_I.htm
  
  lab_nhanesc_recodes <- lab_nhanesc_recodes |> 
    rename(gluc_plas_mgdL=LBXGLU) |> 
    #1999-2002 -> 2003 onward
    mutate(gluc_plas_mgdL = case_when((year<2003) ~ (1.0027*gluc_plas_mgdL)-2.2934,
                                      TRUE ~ gluc_plas_mgdL)) |> 
    #1999-2004 -> 2005 onward
    mutate(gluc_plas_mgdL = case_when((year<2005) ~ (0.9815*gluc_plas_mgdL)-3.5707,
                                      TRUE ~ gluc_plas_mgdL)) |> 
    #1999-2006 -> 2007 onward
    mutate(gluc_plas_mgdL = case_when((year<2007) ~ gluc_plas_mgdL+1.148,
                                      TRUE ~ gluc_plas_mgdL)) |> 
    #1999-2014 -> 2015 onward
    mutate(gluc_plas_mgdL = case_when((year<2015) ~ (1.023*gluc_plas_mgdL)-0.5108,
                                      TRUE ~ gluc_plas_mgdL)) |> 
    var_labels(gluc_plas_mgdL = "glucose (plasma) (mg/dL)")
  
  #=====C-reactive protein
  
  #ANALYTICAL NOTE: 
  # Contains normal and high-sensitivity CRP (2015 onward) and need to be combined
  
  #convert CRP to hsCRP
  lab_nhanesc_recodes <- lab_nhanesc_recodes |> 
    rename(crp=LBXCRP, crp_hs=LBXHSCRP) |> 
    mutate(crp_hs_mgL = case_when((!is.na(crp)) ~ crp*10,
                                  (!is.na(crp_hs)) ~ crp_hs,
                                  TRUE ~ NA_real_)) |> 
    select(-c(crp, crp_hs))
  
  #=====Insulin
  
  #ANALYTIC NOTES: 
  # https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L10AM_C.htm
  # https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/GLU_D.htm
  # https://wwwn.cdc.gov/nchs/nhanes/2011-2012/GLU_G.htm
  # https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/INS_H.htm
  
  #make adjustment progressively by year
  lab_nhanesc_recodes <- lab_nhanesc_recodes |> 
    rename(insulin_uUmL=LBXIN) |> 
    #apply adjustments
    #1999-2002 -> 2003 onward
    mutate(insulin_uUmL = case_when((year < 2003) ~ (1.0027*insulin_uUmL)-2.2934,
                                    TRUE ~ insulin_uUmL)) |> 
    #1999-2004 -> 2005 onward
    mutate(insulin_uUmL = case_when((year < 2005) ~ (0.9591*insulin_uUmL)+1.4890,
                                    TRUE ~ insulin_uUmL)) |> 
    #1999-2010 -> 2011 onward
    mutate(insulin_uUmL = case_when((year < 2011) ~ (0.8868*insulin_uUmL)+(0.0011*(insulin_uUmL^2)),
                                    TRUE ~ insulin_uUmL),
           insulin_uUmL = ifelse(insulin_uUmL<0, NA, insulin_uUmL) #removed neg value from adjustment
    ) |> 
    #1999-2012 -> 2013 onward
    mutate(insulin_uUmL2 = case_when((year < 2013) ~ 10^((1.024*log10(insulin_uUmL))-0.0802),
                                     TRUE ~ insulin_uUmL)) 
  
  #=====Testosterone
  
  #ANALYTICAL NOTE: https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/TST_H.htm
  
  #Adjust units, combine, and apply deming regression equation to pre-2013 data
  lab_nhanesc_recodes <- lab_nhanesc_recodes |>
    mutate(SSTESTO_ngdL = convert_units(SSTESTO, "ng/mL", "ng/dL")) |> 
    mutate(testo_tot_ngdL_unadj = ifelse(!is.na(SSTESTO_ngdL), SSTESTO_ngdL, LBXTST)) |> 
    mutate(testo_tot_ngdL = case_when((year %in% c(1999, 2001, 2003, 2011)) ~ (1.021*testo_tot_ngdL_unadj)-0.178,
                                      TRUE ~ testo_tot_ngdL_unadj)) |> 
    select(-c(SSTESTO,
              SSTESTO_ngdL,
              LBXTST,
              testo_tot_ngdL_unadj))
  
  #=====Apolipoprotein B
  
  #no adjustments needed
  lab_nhanesc_recodes <- lab_nhanesc_recodes |> 
    rename(apob_mgdL = LBXAPB)
  
  #=====Cholesterol (HDL)
  
  #select HDL 
  lab_nhanesc_recodes <- lab_nhanesc_recodes |> 
    rename(chol_hdl_mgdL=LBDHDD) 
  
  #=====Cholesterol (LDL) & Triglycerides
  
  lab_nhanesc_recodes <- lab_nhanesc_recodes |> 
    rename(chol_ldl_mgdL = LBDLDL,
           trig_mgdL = LBXTR)
  
  # #=====Complete blood count
  # 
  # # 1999="LAB25",
  # # 2001="L25_B",
  # # 2003="L25_C",
  # # 2005="CBC_D",
  # # 2007="CBC_E",
  # # 2009="CBC_F",
  # # 2011="CBC_G",
  # # 2013="CBC_H",
  # # 2015="CBC_I",
  # # 2017="CBC_J"
  # 
  # #set df names
  # names_cbc <- c("LAB25",
  #                "L25_B",
  #                "L25_C",
  #                "CBC_D",
  #                "CBC_E",
  #                "CBC_F",
  #                "CBC_G",
  #                "CBC_H",
  #                "CBC_I",
  #                "CBC_J")
  # #set years
  # years_cbc <- seq(1999, 2017, 2)
  # 
  # #hdl data
  # df_cbc <- pull_nhanes(names_cbc, years_cbc)
  
  #=====Glycohemoglobin (HbA1c)
  
  lab_nhanesc_recodes <- lab_nhanesc_recodes |> 
    rename(hba1c_pct = LBXGH)
  
  #formatting of NHANES Continuous
  
  lab_nhanesc_recodes <- lab_nhanesc_recodes |> 
    mutate(study = 'NHANES Continuous') |> 
    mutate(year = as_character(year))
  
  #=====NHANES-III===============================================================
  
  lab_nhanes3 <- import("../../data_raw/NHANES_III/laboratory/laboratory_raw.rds")
  
  lab_nhanes3_recodes <- lab_nhanes3 |> 
    select(SEQN,
           apob_mgdL      = ABP,
           crp_mgdL       = CRP, #convert to HS
           hba1c_pct      = GHP,
           chol_hdl_mgdL  = HDP,
           insulin_uUmL   = I1P,
           chol_ldl_mgdL  = LCP,
           trig_mgdL      = TGP,
           chol_tot_mgdL  = TCP,
           bun_mgdL       = BUP,
           creat_mgdL     = CEP,
           sod_mmolL      = NAPSI) |> 
    mutate(year = "1988-1994") |> 
    mutate(study='NHANES 3') |> 
    mutate(crp_hs_mgL = crp_mgdL*10) |> 
    select(-crp_mgdL)
  
  
  #=====Combine, export data=====================================================
  
  lab_all <- bind_rows(lab_nhanesc_recodes,
                       lab_nhanes3_recodes)
  
  #add unique study ID
  lab_all_id <- lab_all |> 
    mutate(study_id = case_when(
      (study=='NHANES 3') ~ paste0('3', str_pad(SEQN, width=6, side='left', pad='0')),
      (study!='NHANES 3') ~ paste0('4', str_pad(SEQN, width=6, side='left', pad='0')))) |> 
    select(study_id, SEQN, study, year, everything())
  
  #export
  export(lab_all_id, "laboratory_clean.rds")
  
  #create checksum file
  create_md5_file("laboratory_clean.rds")
  
}

# #=====update log file==========================================================
# 
# #write update message
# message="
# Added checksum procedure for combined laboratory data.
# "
# 
# #update log
# update_log(file="log_laboratory.txt",
#            author="Peter T. Tanklsey",
#            message = message)
# 
