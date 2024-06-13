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

dexa_codebook <- enframe(get_label(df_dexa))

#recode 
df_dexa_recodes <- df_dexa |> 
  select(SEQN, year,
         mass_lean_g  = DXDTOLE,
         mass_fat_g   = DXDTOFAT,
         mass_fat_pct = DXDTOPF,
         mass_bmc_g   = DXDTOBMC,
         mass_total_g = DXDTOTOT) |>  #r=1 with (lean+fat+bmc)
  #convert grams to pounds
  mutate(across(ends_with("_g"), ~ convert_units(., "g", "lb"),
                .names = "{str_replace_all(.col, '_g', '_lb')}" )) |> 
  select(-ends_with("_g")) |> 
  var_labels(mass_lean_lb  = "total lean mass (excl BMC) (lb)",
             mass_fat_lb   = "total fat mass (lb)",
             mass_bmc_lb   = "total bone mineral content (lb)",
             mass_total_lb = "total mass (lb)",
             mass_fat_pct  = "total fat percent (%)"
  )

#check distributions by year
ridge_years("SEQN", "year", df_dexa_recodes)



#clean up environment
keep_items <- c(keep_items, "df_dexa_recodes")
rm(list = setdiff(ls(), keep_items))

#=====DEXA-android/gynoid/VAT=()===============================================

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

dexa_ag_codebook <- enframe(get_label(df_dexa_ag))

#recode 
df_dexa_ag_recodes <- df_dexa_ag |> 
  select(SEQN, 
         year,
         fat_gyn_pct      = DXXGPFAT,
         fat_and_pct      = DXXAPFAT,
         fat_andgyn_ratio = DXXAGRAT,
         fat_vat_cm2      = DXXVFATV) |> 
  var_labels(fat_gyn_pct      = "gynoid fat percent (%)",
             fat_and_pct      = "android fat percent (%)",
             fat_andgyn_ratio = "android-to-gynoid fat ratio",
             fat_vat_cm2      = "visceral adipose tissue volumn (cm^2)")

#check distributions by year
ridge_years("SEQN", "year", df_dexa_ag_recodes)

#clean up environment
keep_items <- c(keep_items, "df_dexa_ag_recodes")
rm(list = setdiff(ls(), keep_items))

#=====merge and export=========================================================

df_dexa_all <- df_dexa_recodes |> 
  full_join(df_dexa_ag_recodes, by=c("SEQN", "year"))

export(df_dexa_all, "dexa_clean.rds")

#=====update log file==========================================================

#write update message
message="
Added total body and region-specific DEXA data from 2011-2017 (n=22K). I also
updated the convert_units() function to be used within mutate() calls. First
draft complete and exported.
"

#update log
update_log(file="log_DEXA.txt",
           author="Peter T. Tanksley",
           message = message)




