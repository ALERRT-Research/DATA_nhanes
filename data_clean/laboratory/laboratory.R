
source("../cleaning_packages.R")

#NOTE: ALL SELECTED BIOMARKERS NEED TO BE CHECKED FOR ANALYTICAL CORRECTIONS WAVE-TO-WAVE
#Convention: ALWAYS PREFER --FORWARD-- DEMING EQUATIONS

#=====Lab codebook=============================================================
# url <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Laboratory"
# webpage <- read_html(url)
# table_data <- html_table(html_nodes(webpage, "table"), fill = TRUE)[[1]]
# 
# #export
# export(table_data, "nhanes_lab_codebook.csv")

#import
codebook <- import("nhanes_lab_codebook.csv")

#=====Define function for unit conversions=====================================

convert_units <- function(df, var_name_old, var_name_new, input_unit, output_unit, drop_after = FALSE) {
  require(units)
  converted <- set_units(df[[var_name_old]], input_unit, mode = "standard") |> 
    set_units(output_unit, mode = "standard")
  
  if (drop_after) {
    converted <- drop_units(converted)
  }
  
  df[[var_name_new]] <- converted
  return(df)
}

keep_items <- c(ls(), "keep_items")

#=====Standard biochemistry profile============================================

#Analytic note: adjustments needed, see: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BIOPRO_J.htm

# 1999="LAB18",
# 2001="LAB40_B",
# 2003="LAB40_C",
# 2005="BIOPRO_D",
# 2007="BIOPRO_E",
# 2009="BIOPRO_F",
# 2011="BIOPRO_G",
# 2013="BIOPRO_H",
# 2015="BIOPRO_I",
# 2017="BIOPRO_J"

#set df names
names_sbp <- c("LAB18",
               "L40_B",
               "L40_C",
               "BIOPRO_D",
               "BIOPRO_E",
               "BIOPRO_F",
               "BIOPRO_G",
               "BIOPRO_H",
               "BIOPRO_I",
               "BIOPRO_J")
#set years
years_sbp <- seq(1999, 2017, 2)

#SBP data
df_sbp <- pull_nhanes(names_sbp, years_sbp)

#contents: cholesterol total, triglyceride, glucose, blood urea nitrogen, creatinine, sodium
# lab_names <- enframe(get_label(df_sbp))

ridge_years("SEQN", "year", df_sbp)

df_sbp_recodes <- df_sbp |> 
  select(SEQN, year,
         chol_tot_mgdL = LBXSCH, #adjustment needed
         trig_mgdL     = LBXSTR, #adjustment needed
         gluc_mgdL     = LBXSGL,
         bun_mgdL      = LBXSBU, #adjustment needed
         creat_mgdL    = LBXSCR,  #adjustment needed
         sod_mmolL     = LBXSNASI) |> 
  #apply forward deming equations to years prior to 2017
  mutate(chol_tot_mgdL = case_when((year==2017) ~ chol_tot_mgdL,
                                   TRUE         ~ 0.9556*chol_tot_mgdL+2.105),
         trig_mgdL     = case_when((year==2017) ~ trig_mgdL,
                                   TRUE         ~ 1.036*trig_mgdL+7.271),
         bun_mgdL      = case_when((year==2017) ~ bun_mgdL,
                                   TRUE         ~ 0.9992*bun_mgdL+0.4484),
         creat_mgdL    = case_when((year==2017) ~ creat_mgdL,
                                   TRUE         ~ 0.9515*creat_mgdL+0.06608))

ridge_years("SEQN", "year", df_sbp_recodes)

#clean up environment
keep_items <- c(keep_items, "df_sbp_recodes")
rm(list = setdiff(ls(), keep_items))

#=====C-reactive protein=(READY)===============================================

#ANALYTICAL NOTE: contains normal and high-sensitivity CRP (to be combined)

# 1999="LAB11",
# 2001="L11_B",
# 2003="L11_C",
# 2005="CRP_D",
# 2007="CRP_E",
# 2009="CRP_F",
# 2015="HSCRP_I",
# 2017="HSCRP_J"

#set names
names_crp <- c("LAB11",
               "L11_B",
               "L11_C",
               "CRP_D",
               "CRP_E",
               "CRP_F",
               "HSCRP_I", #these are high-sensitivity CRP
               "HSCRP_J"
)

#set years
years_crp <- c(1999,
               2001,
               2003,
               2005,
               2007,
               2009,
               2015,
               2017
)


#CRP data
df_crp <- pull_nhanes(names_crp, years_crp)


#restrict to CRP only (convert CRP to hsCRP?)
df_crp_recodes <- df_crp |> 
  select(SEQN, year, crp=LBXCRP, crp_hs=LBXHSCRP) |> 
  mutate(crp_combined = case_when((!is.na(crp)) ~ crp*10,
                                  (!is.na(crp_hs)) ~ crp_hs,
                                  TRUE ~ NA_real_))

#check distributions by year (looks good)
ridge_years("SEQN", "year", df_crp_recodes |> mutate(across(-c(SEQN, year), ~ log(. +1))))

#clean up environment
keep_items <- c(keep_items, "df_crp_recodes")
rm(list = setdiff(ls(), keep_items))

#=====Insulin=(READY)==========================================================

# 1999="LAB10AM",
# 2001="L10AM_B",
# 2003="L10AM_C",
# 2005="GLU_D",
# 2007="GLU_E",
# 2009="GLU_F",
# 2011="GLU_G",
# 2013="INS_H",
# 2015="INS_I",
# 2017="INS_J"

#set names
names_insul <- c("LAB10AM",
                 "L10AM_B",
                 "L10AM_C",
                 "GLU_D",
                 "GLU_E",
                 "GLU_F",
                 "GLU_G",
                 "INS_H",
                 "INS_I",
                 "INS_J")

#set years
years_insul <- c(1999,
                 2001,
                 2003,
                 2005,
                 2007,
                 2009,
                 2011,
                 2013,
                 2015,
                 2017)

#pull data
df_insul <- pull_nhanes(names_insul, years_insul)

#final
df_insul_recodes <- df_insul |> 
  select(SEQN, year, insulin_uUmL="LBXIN")

#check distributions (looks good)
ridge_years("SEQN", "year", df_insul_recodes |> mutate(insulin_uUmL = log(insulin_uUmL)))

#clean up environment
keep_items <- c(keep_items, "df_insul_recodes")
rm(list = setdiff(ls(), keep_items))

#=====Testosterone=(READY)=====================================================

#ANALYTICAL NOTE. SEE: https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/TST_H.htm

# 1999="SSCHL_A",
# 2001="SSCHL_B",
# 2003="SSCHL_C",
# 2011="TST_G",
# 2013="TST_H",
# 2015="TST_I"

#set names
names_testo <- c("SSCHL_A",
                 "SSCHL_B",
                 "SSCHL_C",
                 "TST_G",
                 "TST_H",
                 "TST_I")

#set years
years_testo <- c(1999,
                 2001,
                 2003,
                 2011,
                 2013,
                 2015)

# testo data
df_testo <- pull_nhanes(names_testo, years_testo)

#convert early testo from ng/mL to ng/dL
df_testo_adj <- convert_units(df_testo, "SSTESTO", "SSTESTO_ngdL", "ng/mL", "ng/dL", drop_after = TRUE)

#combine and apply deming regression equation to pre-2013 data
df_testo_recodes <- df_testo_adj |> 
  mutate(testo_tot_unadj = ifelse(!is.na(SSTESTO_ngdL), SSTESTO_ngdL, LBXTST)) |> 
  mutate(testo_tot = case_when((year %in% c(1999, 2001, 2003, 2011)) ~ 1.021*testo_tot_unadj-0.178,
                               TRUE ~ testo_tot_unadj)) |> 
  select(SEQN, year, testo_tot)

#check distributions (2011 and later waves have a distinct peak at low levels)
#check for subgroups (females, minors, etc)
ridge_years(id="SEQN", year="year", df=df_testo_recodes)

# #pull in demo data; select adult males
# demo_male_18 <- import("../demographics/demo_99to17.rds") |> 
#   filter(gender=="Male" & age_screen_yr >=18) |>
#   pull(SEQN)
# 
# #check distributions looking only at adult males (looks good)
# ridge_years(id="SEQN", year="year", df=df_testo_adj |>
#               select(SEQN, year, testo_tot) |> 
#               filter(SEQN %in% demo_male_18))

#clean up environment
keep_items <- c(keep_items, "df_testo_recodes")
rm(list = setdiff(ls(), keep_items))

#=====Apolipoprotein B=(READY)=================================================

# 2005="TRIGLY_D",
# 2007="ApoB_E",
# 2009="ApoB_F",
# 2011="ApoB_G",
# 2013="ApoB_H",
# 2015="ApoB_I"

#set df names
names_apob <- c("TRIGLY_D",
                "ApoB_E",
                "ApoB_F",
                "ApoB_G",
                "ApoB_H",
                "ApoB_I")
#set years
years_apob <- seq(2005, 2015, 2)

#apob data
df_apob <- pull_nhanes(names_apob, years_apob)

#select apob
df_apob_recodes <- df_apob |> 
  select(SEQN, year, "apob_mgdL"=LBXAPB)

ridge_years("SEQN", "year", df_apob_recodes)

#clean up environment
keep_items <- c(keep_items, "df_apob_recodes")
rm(list = setdiff(ls(), keep_items))

#=====Cholesterol=HDL=(READY)==================================================

# 1999="Lab13",
# 2001="l13_b",
# 2003="l13_c",
# 2005="HDL_D",
# 2007="HDL_E",
# 2009="HDL_F",
# 2011="HDL_G",
# 2013="HDL_H",
# 2015="HDL_I",
# 2017="HDL_J"

#set df names
names_hdl <- c("Lab13",
               "l13_b",
               "l13_c",
               "HDL_D",
               "HDL_E",
               "HDL_F",
               "HDL_G",
               "HDL_H",
               "HDL_I",
               "HDL_J")
#set years
years_hdl <- seq(1999, 2017, 2)

#hdl data
df_hdl <- pull_nhanes(names_hdl, years_hdl)

#select HDL (Total cholesterol not present in all years)
df_hdl_recodes <- df_hdl |> 
  select(SEQN, year, chol_hdl_mmolL=LBDHDDSI)

#check distributions (looks good)
ridge_years("SEQN", "year", df_hdl_recodes)

#clean up environment
keep_items <- c(keep_items, "df_hdl_recodes")
rm(list = setdiff(ls(), keep_items))

#=====Cholesterol=LDL=(READY)==================================================

# 1999="Lab13",
# 2001="l13_b",
# 2003="l13_c",
# 2005="TRIGLY_D",
# 2007="TRIGLY_E",
# 2009="TRIGLY_F",
# 2011="TRIGLY_G",
# 2013="TRIGLY_H",
# 2015="TRIGLY_I",
# 2017="TRIGLY_J"

#set df names
names_ldl <- c("LAB13AM",
               "L13AM_B",
               "L13AM_C",
               "TRIGLY_D",
               "TRIGLY_E",
               "TRIGLY_F",
               "TRIGLY_G",
               "TRIGLY_H",
               "TRIGLY_I",
               "TRIGLY_J")
#set years
years_ldl <- seq(1999, 2017, 2)

#hdl data
df_ldl <- pull_nhanes(names_ldl, years_ldl)

ridge_years("SEQN", "year", df_ldl)

#select LDL
df_ldl_recodes <- df_ldl |> 
  select(SEQN, year, chol_ldl_mgdL=LBDLDL)

#check distributions (looks good)
ridge_years("SEQN", "year", df_ldl_recodes)

#clean up environment
keep_items <- c(keep_items, "df_ldl_recodes")
rm(list = setdiff(ls(), keep_items))

# #=====Complete blood count ====================================================
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

#=====Glycohemoglobin (HbA1c)=(READY)==========================================

# 1999="LAB10",
# 2001="L10_B",
# 2003="L10_C",
# 2005="GHB_D",
# 2007="GHB_E",
# 2009="GHB_F",
# 2011="GHB_G",
# 2013="GHB_H",
# 2015="GHB_I",
# 2017="GHB_J"

#set df names
names_ghb <- c("LAB10",
               "L10_B",
               "L10_C",
               "GHB_D",
               "GHB_E",
               "GHB_F",
               "GHB_G",
               "GHB_H",
               "GHB_I",
               "GHB_J")
#set years
years_ghb <- seq(1999, 2017, 2)

#hdl data
df_ghb <- pull_nhanes(names_ghb, years_ghb)

df_ghb_recodes <- df_ghb |> 
  select(SEQN, year, hba1c_pct=LBXGH)

#clean up environment
keep_items <- c(keep_items, "df_ghb_recodes")
rm(list = setdiff(ls(), keep_items))

#=====update log file==========================================================

#write update message
message="
Several updates. Added LDL. Completed adjustments of all vars. Added env-cleaning
script to each section. Next up: combine data.
"

#update log
update_log(file="log_laboratory.txt",
           author="Peter T. Tanklsey",
           message = message)

