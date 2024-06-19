source("../cleaning_packages.R")

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
         #VO2
         exam_min            = CVDEXLEN,
         hr_peak_bpm         = CVDPMHR,
         par_code            = CVXPARC,
         vo2max_pred_mlkgmin = CVDVOMAX,
         vo2max_est_mlkgmin  = CVDESVO2,
         fit_lvl             = CVDFITLV)

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
  mutate(study = "NHANES Continuous")

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
         whr             = BMPWHR,
         #VO2
         #FEV
         fev_ml5sec      = SPPFEV05,
         fvc_ml          = SPPFVC
         )

exam_nhanes3_recodes <- exam_nhanes3 |> 
  mutate(study = "NHANES 3")


#=====Combine=================================================================

exam_all <- bind_rows(exam_nhanesc_recodes, exam_nhanes3_recodes)

#add unite
exam_all_id <- exam_all |> 
  mutate(study_id = case_when(
    (study=='NHANES 3') ~ paste0('3', str_pad(SEQN, width=6, side='left', pad='0')),
    (study!='NHANES 3') ~ paste0('4', str_pad(SEQN, width=6, side='left', pad='0')))) |> 
  select(study_id, SEQN, year, everything())







#save clean dataframe
export(exam_all_id, "exam_clean.rds")

#=====update log file==========================================================

#write update message
message="
Added NHANES 3 data. Need to find FEV and WHR for NHANESC. 
"

#update log
update_log(file="log_exam.txt",
           author="Peter T. Tanksley",
           message = message)




