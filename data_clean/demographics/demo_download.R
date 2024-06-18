source("../cleaning_packages.R")

#=====NHANES-Continuous========================================================

# Define file path
file_path <- glue("{onedrive_dir_nhanes}/NHANES_Continuous/demographics/demographics_raw.rds")

# Check if the file exists
if (!file.exists(file_path)) {
  # Get waves (1999-2017)
  names_demo <- c("DEMO", paste0("DEMO_", LETTERS[2:10]))
  years_demo <- seq(1999, 2017, by=2)
  
  demo <- pull_nhanes(names_demo, years_demo, mismatch_regex = "DMD.SIZ$|DMDHHSZ.")
  
  save_to_onedrive(demo, file_name = "demographics")
} else {
  demo_nhanesc <- import(file_path)
}


#=====NHANES-III===============================================================

if (!file.exists(glue("{onedrive_dir_nhanes}/NHANES_III/hh_adult/hh_adult_raw.rds"))) {
  
  #get SAS code
  if (!file.exists("nhanes3_hh_adult_sas_setup.sas")) {
    system("curl https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.sas > nhanes3_hh_adult_sas_setup.sas")
  }
  
  #get ASCII data file
  if (!file.exists("nhanes3_hh_adult_ascii.dat")) {
    system("curl https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.dat > nhanes3_hh_adult_ascii.dat")
  }
  
  #=====Import Data==============================================================
  
  # hh_adult_data <- read.SAScii("hh_adult_ascii.dat",
  #                              "hh_adult_sas_setup.sas",
  #                              zipped = F)
  
  #initial run of read.SAScii spotted some issues with the setup file, fixing
  sas_file <- read_lines("nhanes3_hh_adult_sas_setup.sas")
  
  #patterns to replace and their replacements
  patterns <- c('HAZA1CC  $30',
                'HAZA1CC  3235-3264',
                'HAZA1CC  = "Med reason BP not taken - other(yrs 5-6)"')
  
  replacements <- c('HAZA1CC $  $30',
                    'HAZA1CC $  3235-3264',
                    'HAZA1CC $  = "Med reason BP not taken - other(yrs 5-6)"')
  
  #apply replacements
  for (i in seq_along(patterns)) {
    sas_file <- gsub(patterns[i], replacements[i], sas_file)
  }
  
  #write the fixed file
  write_lines(sas_file, "nhanes3_hh_adult_sas_setup_fixed.sas")
  
  hh_adult_data <- read.SAScii("nhanes3_hh_adult_ascii.dat",
                               "nhanes3_hh_adult_sas_setup_fixed.sas",
                               zipped = F)
  
  #=====Get labels===============================================================
  
  #get data dictionary from SAS files
  var_labels <- read_lines_to_df("nhanes3_hh_adult_sas_setup_fixed.sas",
                                 start_line = 2632,
                                 end_line = 3869)
  
  #add labels
  hh_adult_data_labs <- hh_adult_data |> 
    set_label(label = var_labels$description)
  
  #=====Export===================================================================
  
  
  save_to_onedrive(hh_adult_data_labs, file_name = "hh_adult", file_path=glue("{onedrive_dir_nhanes}/NHANES_III"))
  save_to_onedrive(var_labels, "hh_adult")
  rm(nhanes3_hh_adult_ascii.dat,
     nhanes3_hh_adult_sas_setup.sas,
     nhanes3_hh_adult_sas_setup_fixed.sas)
}

# df_nhanes3_codebook <- import(glue("{onedrive_dir_nhanes}/NHANES_III/hh_adult/hh_adult_codebook.rds"))
df_nhanes3 <- import("../../data_raw/nhanes_3/hh_adult/hh_adult_clean.rds")









