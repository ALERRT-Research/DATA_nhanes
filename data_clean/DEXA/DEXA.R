source("../cleaning_packages.R")

#=====import codebook==========================================================
# url <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Examination&Cycle="
# webpage <- read_html(url)
# table_data <- html_table(html_nodes(webpage, "table"), fill = TRUE)[[1]]
# 
# #export
# export(table_data, "nhanes_exam_codebook.csv")

#import
codebook <- import("../exam/nhanes_exam_codebook.csv")

#start list to keep when cleaning env
keep_items <- c(ls(), "keep_items")

#=====DEXA=()==========================================================

#DEXA from 1999-2006 had high levels of missing data and only come in multiply
#imputed format (x5).

# 1999="DXX",
# 2001="DXX_B",
# 2003="DXX_C",
# 2005="DXX_D",
# 2007="DXX_E",
# 2009="DXX_F",
# 2011="DXX_G",
# 2013="DXX_H",
# 2015="DXX_I",
# 2017="DXX_J"

#set names
names_dexa <- c(#"DXX",
                #"DXX_B",
                # "DXX_C",
                # "DXX_D",
                # "DXX_E",
                # "DXX_F",
                "DXX_G",
                "DXX_H",
                "DXX_I",
                "DXX_J")

#set years
years_dexa <- seq(2011, 2017, 2)

#BP data
df_dexa <- pull_nhanes(names_dexa, years_dexa)

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


#=====update log file==========================================================

#write update message
message="
Started work on DEXA files. Found that 1999-2006 only come as multiple
imputation files (x5). Consider the utility of including/excluding them.
"

#update log
update_log(file="log_DEXA.txt",
           author="Peter T. Tanksley",
           message = message)




