
source("../cleaning_packages.R")

#get codebook for all exam years
if(!file.exists("lab_codebook.csv")) {
  lab_codebook <- nhanes_table("https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Laboratory&Cycle=")
  export(exam_codebook, "lab_codebook.csv")
} else {
  lab_codebook <- import("lab_codebook.csv")
}

if(!file.exists("laboratory_raw.rds")) {
  
  #=====Standard biochemistry profile= (READY)=================================
  
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
                 "L40_B", #different name for creatinine: LBDSCR
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
  
  df_sbp_select <- df_sbp |> 
    select(SEQN,
           year,
           LBXSCH, 
           LBXSBU, 
           LBXSCR, 
           LBDSCR,
           LBXSNASI)
  
  #=====Glucose=(READY)==========================================================
  
  # 1999="LAB10AM",
  # 2001="L10AM_B",
  # 2003="L10AM_C",
  # 2005="GLU_D",
  # 2007="GLU_E",
  # 2009="GLU_F",
  # 2011="GLU_G",
  # 2013="GLU_H",
  # 2015="GLU_I",
  # 2017="GLU_J"
  
  #set df names
  names_glu <- c("LAB10AM",
                 "L10AM_B",
                 "L10AM_C",
                 "GLU_D",
                 "GLU_E",
                 "GLU_F",
                 "GLU_G",
                 "GLU_H",
                 "GLU_I",
                 "GLU_J")
  #set years
  years_glu <- seq(1999, 2017, 2)
  
  #SBP data
  df_glu <- pull_nhanes(names_glu, years_glu)
  
  df_glu_select <- df_glu |> 
    select(SEQN, year, LBXGLU)
  
  #=====C-reactive protein=(READY)===============================================
  
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
  
  df_crp_select <- df_crp |> 
    select(SEQN,
           year,
           LBXCRP,
           LBXHSCRP)
  
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
  
  df_insul_select <- df_insul |>
    select(SEQN, year, LBXIN)
  
  #=====Testosterone=(READY)=====================================================
  
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
  
  df_testo_select <- df_testo |> 
    select(SEQN, 
           year,
           SSTESTO,
           LBXTST)
  
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
  
  df_apob_select <- df_apob |> 
    select(SEQN,
           year,
           LBXAPB)
  
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
  df_hdl_select <- df_hdl |> 
    select(SEQN,
           year,
           LBDHDD)
  
  #=====Cholesterol=LDL=(READY)=Triglyceride=(READY)=============================
  
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
  
  df_ldl_select <- df_ldl |> 
    select(SEQN,
           year,
           LBDLDL,
           LBXTR)
  
  # #=====Complete blood count=()====================================================
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
  # #cbc data
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
  
  #ghb data
  df_ghb <- pull_nhanes(names_ghb, years_ghb) 
  df_ghb_select <- df_ghb |> 
    select(SEQN,
           year,
           LBXGH)
  
  
  #=====Combine data=============================================================
  
  df_list <- list(df_sbp_select,
                  df_glu_select,
                  df_crp_select,
                  df_insul_select,
                  df_testo_select,
                  df_apob_select,
                  df_hdl_select,
                  df_ldl_select,
                  # df_cbc_select,
                  df_ghb_select
  )
  
  df_all <- reduce(df_list, full_join)
  
  #save clean dataframe
  export(df_all, "laboratory_raw.rds")
  
}

#=====update log file==========================================================

#write update message
message="
Swapped out the SI version of HDL cholesterol for metric.
"

#update log
update_log(file="log_laboratory_download.txt",
           author="Peter T. Tanklsey",
           message = message)

