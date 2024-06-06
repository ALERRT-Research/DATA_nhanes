source("../cleaning_packages.R")

#=====import codebook==========================================================
# url <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Examination&Cycle="
# webpage <- read_html(url)
# table_data <- html_table(html_nodes(webpage, "table"), fill = TRUE)[[1]]
# 
# #export
# export(table_data, "nhanes_exam_codebook.csv")

#import
codebook <- import("nhanes_exam_codebook.csv")

#start list to keep when cleaning env
keep_items <- c(ls(), "keep_items")

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

#recode 
df_bp_recodes <- df_bp |> 
  select(SEQN, year,
         "pulse_60sec"= BPXPLS,
         "bp_sys_1"   = BPXSY1,
         "bp_sys_2"   = BPXSY2,
         "bp_sys_3"   = BPXSY3,
         "bp_sys_4"   = BPXSY4,
         "bp_dia_1"   = BPXDI1,
         "bp_dia_2"   = BPXDI2,
         "bp_dia_3"   = BPXDI3,
         "bp_dia_4"   = BPXDI4) |> 
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
  var_labels(bp_sys_avg_mmHg="Blood pressure-systolic (avg.) (mm/Hg)",
             bp_dia_avg_mmHg="Blood pressure-diastolic (avg.) (mm/Hg)") |> 
  select(-matches(".*_[1-4]$"))

#check distributions by year
ridge_years("SEQN", "year", df_bp_recodes)

#clean up environment
keep_items <- c(keep_items, "df_bp_recodes")
rm(list = setdiff(ls(), keep_items))

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

df_grip_recodes <- df_grip |> 
  select(SEQN, year, grip_combo_kg=MGDCGSZ)

#check distributions (looks good)
ridge_years("SEQN", "year", df_grip_recodes)

#clean up environment
keep_items <- c(keep_items, "df_grip_recodes")
rm(list = setdiff(ls(), keep_items))

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

#recodes (hip measurements only available in 2017, not included for now)
df_bmi_recodes <- df_bmi |> 
  select(SEQN, year, 
         height_cm=BMXHT,
         weight_kg=BMXWT,
         bmi_kgm2=BMXBMI, 
         waist_cm=BMXWAIST)  


#check distributions (looks good)
ridge_years("SEQN", "year", df_bmi_recodes)

#clean up environment
keep_items <- c(keep_items, "df_bmi_recodes")
rm(list = setdiff(ls(), keep_items))

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

#recodes (hip measurements only available in 2017, not included for now)
df_vo2_recodes <- df_vo2 |> 
  filter(CVDEXSTS=="VO2max estimated") |> #removed 6,093 rows (42%), 8,324 rows remaining
  select(SEQN, year, 
         exam_min=CVDEXLEN,
         hr_peak_bpm=CVDPMHR,
         par_code=CVXPARC,
         vo2max_pred_mlkgmin=CVDVOMAX,
         vo2max_est_mlkgmin=CVDESVO2,
         fit_lvl=CVDFITLV)  


#check distributions (looks good)
ridge_years("SEQN", "year", df_vo2_recodes)

#clean up environment
keep_items <- c(keep_items, "df_vo2_recodes")
rm(list = setdiff(ls(), keep_items))

#=====Combine data=============================================================

df_list <- list(df_bmi_recodes,
                df_bp_recodes,
                df_grip_recodes,
                df_vo2_recodes)

df_all <- reduce(df_list, full_join)

#clear up env
rm(list = setdiff(ls(), c("df_all", "update_log")))

#save clean dataframe
export(df_all, "exam_clean.rds")

#=====update log file==========================================================

#write update message
message="
Added grip, BMI, and vo2max sections. Explored clean dataframe. Also added 
code for cleaning env in each section. 
"

#update log
update_log(file="log_exam.txt",
           author="Peter T. Tanksley",
           message = message)




