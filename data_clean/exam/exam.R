source("../cleaning_packages.R")

#check checksum 
checksum_result <- check_md5_file("exam_clean.rds")

if(!file.exists("exam_clean.rds") || !checksum_result){
  
  #=====NHANES C=================================================================
  
  #get raw data
  exam_nhanesc_raw <- import("../../data_raw/NHANES_C/exam/exam_raw.rds")
  
  #select vars
  exam_nhanesc <- exam_nhanesc_raw |>  
    select(SEQN, year,
           #blood pressure
           pulse_60sec         = BPXPLS,
           bp_sys_1            = BPXSY1,
           bp_sys_2            = BPXSY2,
           bp_sys_3            = BPXSY3,
           bp_sys_4            = BPXSY4,
           bp_dia_1            = BPXDI1,
           bp_dia_2            = BPXDI2,
           bp_dia_3            = BPXDI3,
           bp_dia_4            = BPXDI4,
           #grip
           grip_combo_kg       = MGDCGSZ,
           #BMI
           height_cm           = BMXHT,
           weight_kg           = BMXWT,
           bmi_kgm2            = BMXBMI, 
           waist_cm            = BMXWAIST,
           hip_cm              = BMXHIP,
           #VO2
           exam_min            = CVDEXLEN,
           hr_peak_bpm         = CVDPMHR,
           par_code            = CVXPARC,
           vo2max_pred_mlkgmin = CVDVOMAX,
           vo2max_est_mlkgmin  = CVDESVO2,
           fit_lvl             = CVDFITLV,
           #Spirometry
           fev_ml05            = SPXNFEV5,
           fev_ml1             = SPXNFEV1,
           fev_ml3             = SPXNFEV3,
           fev_ml6             = SPXNFEV6,
           fev_ml_peak         = SPXNPEF,
           fvc_ml_max          = SPXNFVC)
  
  #recode
  exam_nhanesc_recodes <- exam_nhanesc |> 
    rowwise() |>
    mutate(bp_sys_avg_mmHg = ifelse(if_all(starts_with("bp_sys_"), is.na), NA,
                                    mean(c(bp_sys_1,
                                           bp_sys_2,
                                           bp_sys_3,
                                           bp_sys_4), na.rm = TRUE))) |>
    mutate(bp_dia_avg_mmHg = ifelse(if_all(starts_with("bp_dia_"), is.na), NA,
                                    mean(c(bp_dia_1,
                                           bp_dia_2,
                                           bp_dia_3,
                                           bp_dia_4), na.rm = TRUE))) |>
    ungroup() |>
    select(-matches(".*_[1-4]$")) |> 
    mutate(whr = waist_cm/hip_cm) |> 
    mutate(study = "NHANES Continuous") |> 
    mutate(year = as.character(year))
  
  #=====NHANES 3=================================================================
  
  exam_nhanes3_raw <- import("../../data_raw/NHANES_III/exam/exam_raw.rds")
  # codebook <- enframe(get_label(exam_nhanes3_raw))
  
  exam_nhanes3 <- exam_nhanes3_raw |> 
    select(SEQN,
           #Blood pressure
           pulse_60sec     = PEP6DR,
           bp_dia_avg_mmHg = PEPMNK5R,
           bp_sys_avg_mmHg = PEPMNK1R,
           #Grip
           #BMI
           height_cm       = BMPHT,
           weight_kg       = BMPWT,
           bmi_kgm2        = BMPBMI,
           waist_cm        = BMPWAIST,
           hip_cm          = BMPBUTTO,
           whr             = BMPWHR,
           #VO2
           #Spirometry
           fev_ml05        = SPPFEV05,
           fev_ml1         = SPPFEV1,
           fev_ml3         = SPPFEV3,
           fev_ml6         = SPPFEV6,
           fev_ml_peak     = SPPPEAK,
           fvc_ml_max      = SPPFVC
    )
  
  exam_nhanes3_recodes <- exam_nhanes3 |> 
    mutate(study = "NHANES 3") |> 
    mutate(year = "1988-1994")
  
  
  #=====Combine=================================================================
  
  exam_all <- bind_rows(exam_nhanesc_recodes, exam_nhanes3_recodes)
  
  #add unique IDs and label
  exam_all_id <- exam_all |> 
    mutate(study_id = case_when(
      (study=='NHANES 3') ~ paste0('3', str_pad(SEQN, width=6, side='left', pad='0')),
      (study!='NHANES 3') ~ paste0('4', str_pad(SEQN, width=6, side='left', pad='0')))) |> 
    select(study_id, SEQN, year, everything()) |> 
    var_labels(study_id            = "unique ID for combined NH3 and NHC studies",
               pulse_60sec         = "60 sec pulse",
               grip_combo_kg       = "grip strength combined R/L (kg)", 
               height_cm           = "height (cm)", 
               weight_kg           = "weight (kg)", 
               bmi_kgm2            = "body mass index (kg/m^2)",
               waist_cm            = "waist circumference (cm)", 
               exam_min            = "cardiovascular fitness exam time (min)", 
               hr_peak_bpm         = "peak heart rate (bpm)", 
               par_code            = "physical activity readiness code",
               vo2max_pred_mlkgmin = "vo2max predicted (ml/kg/min) (PAR, age, BMI, sex)", 
               vo2max_est_mlkgmin  = "vo2max estimated (ml/kg/min)", 
               fit_lvl             = "cardiovascular fitness level (sex-specific)", 
               fev_ml05            = "forced expiratory volume (ml/.05sec)",
               fev_ml1             = "forced expiratory volume (ml/1sec)", 
               fev_ml3             = "forced expiratory volume (ml/3sec)", 
               fev_ml6             = "forced expiratory volume (ml/6sec)", 
               fev_ml_peak         = "forced expiratory volume (ml)",
               fvc_ml_max          = "forced vital capacity (max)", 
               bp_sys_avg_mmHg     = "systolic blood pressure (avg.) (mmHg)", 
               bp_dia_avg_mmHg     = "diastolic blood pressure (avg.) (mmHg)", 
               study               = "NHANES study (III or continuous)",
               whr                 = "waist-hip ratio")
  
  
  
  
  
  
  #save clean dataframe
  export(exam_all_id, "exam_clean.rds")
  
  #create checksum file
  create_md5_file("exam_clean.rds")
  
}

# #=====update log file==========================================================
# 
# #write update message
# message="
# Added checksum procedure for combined exam data.
# "
# 
# #update log
# update_log(file="log_exam.txt",
#            author="Peter T. Tanksley",
#            message = message)
# 
# 
# 
# 
