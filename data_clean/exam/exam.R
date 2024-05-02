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

#=====blood pressure (resting)=================================================

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

#check distributions by year
# ridge_years("SEQN", "year", df_bp)

#recode 
df_bp_recodes <- df_bp |> 
  mutate(BPXPLS = ifelse(is.na(BPXPLS), BPXCHR, BPXPLS)) |> 
  select(SEQN, year,
         "pulse_60s" = BPXPLS,
         "bp_sys_1"   = BPXSY1,
         "bp_sys_2"   = BPXSY2,
         "bp_sys_3"   = BPXSY3,
         "bp_sys_4"   = BPXSY4,
         "bp_dia_1"   = BPXDI1,
         "bp_dia_2"   = BPXDI2,
         "bp_dia_3"   = BPXDI3,
         "bp_dia_4"   = BPXDI4) |> 
  rowwise() |> 
  mutate(bp_sys_avg = ifelse(if_all(starts_with("bp_sys_"), is.na), NA, 
                             mean(c(bp_sys_1,
                                    bp_sys_2,
                                    bp_sys_3,
                                    bp_sys_4), na.rm = TRUE))) |> 
  mutate(bp_dia_avg = ifelse(if_all(starts_with("bp_dia_"), is.na), NA, 
                             mean(c(bp_dia_1,
                                    bp_dia_2,
                                    bp_dia_3,
                                    bp_dia_4), na.rm = TRUE))) |> 
  ungroup() |> 
  select(-matches(".*_[1-4]$"))


#=====Grip=====================================================================

# 2011="MGX_G",
# 2013="MGX_H",


#set names
names_grip <- c("MGX_G",
              "MGX_H")

#set years
years_grip <- c(2011, 2013)

#grip data
df_grip <- pull_nhanes(names_grip, years_grip)

#check distributions by year
# ridge_years("SEQN", "year", df_bp)




#=====update log file==========================================================

#write update message
message="
Updated script to use common functions. Cleaned up BP data. Need to check on
which variables of interest are included in exam (grip, DEXA, cardiovascular 
fitness, etc.).
"

#update log
update_log(file="log_exam.txt",
           author="Peter T. Tanksley",
           message = message)




