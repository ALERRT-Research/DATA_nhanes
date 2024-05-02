
source("../cleaning_packages.R")

#NOTE: ALL SELECTED BIOMARKERS NEED TO BE CHECKED FOR ANALYTICAL CORRECTIONS WAVE-TO-WAVE

#=====Lab codebook=============================================================
# url <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Laboratory"
# webpage <- read_html(url)
# table_data <- html_table(html_nodes(webpage, "table"), fill = TRUE)[[1]]
# 
# #export
# export(table_data, "nhanes_lab_codebook.csv")

#import
codebook <- import("nhanes_lab_codebook.csv")

#=====Standard biochemistry profile============================================

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
df_sbp <- process_data(names_sbp, years_sbp)

#=====C-reactive protein=======================================================

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
df_crp <- process_data(names_crp, years_crp)


#restrict to CRP only (convert CRP to hsCRP?)
df_crp_only <- df_crp |> 
  select(SEQN, year, crp=LBXCRP, crp_hs=LBXHSCRP) |> 
  mutate(crp_combined = case_when((!is.na(crp)) ~ crp*10,
                                  (!is.na(crp_hs)) ~ crp_hs,
                                  TRUE ~ NA_real_)) |> 
  mutate(across(-c(SEQN, year), ~log(.+1)))

#check distributions by year
ridge_years("SEQN", "year", df_crp_only)

#=====Insulin==================================================================

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

df_insul <- process_data(names_insul, years_insul)


#=====Testosterone=============================================================

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
df_testo <- process_data(names_testo, years_testo)


#=====Apolipoprotein B=========================================================

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
df_apob <- process_data(names_apob, years_apob)

#=====Cholesterol - HDL========================================================

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
df_hdl <- process_data(names_hdl, years_hdl)

#=====Complete blood count ====================================================

# 1999="LAB25",
# 2001="L25_B",
# 2003="L25_C",
# 2005="CBC_D",
# 2007="CBC_E",
# 2009="CBC_F",
# 2011="CBC_G",
# 2013="CBC_H",
# 2015="CBC_I",
# 2017="CBC_J"

#set df names
names_cbc <- c("LAB25",
               "L25_B",
               "L25_C",
               "CBC_D",
               "CBC_E",
               "CBC_F",
               "CBC_G",
               "CBC_H",
               "CBC_I",
               "CBC_J")
#set years
years_cbc <- seq(1999, 2017, 2)

#hdl data
df_cbc <- process_data(names_cbc, years_cbc)

#=====Glycohemoglobin (HbA1c)==================================================

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
df_ghb <- process_data(names_ghb, years_ghb)



#=====update log file==========================================================

#write update message
message="
Data matching TAMU vars is pulled. Need to make year to year adjustments before use.
"

#update log
update_log(file="log_laboratory.txt",
           author="Peter T. Tanklsey",
           message = message)
