source("../cleaning_packages.R")

if(!file.exists("exam_raw.rds")){
  
  #=====Download Data============================================================
  
  #get SAS code
  if (!file.exists("exam_sas_setup.sas")) {
    system("curl https://wwwn.cdc.gov/nchs/data/nhanes3/1a/exam.sas > exam_sas_setup.sas")
  }
  
  #get ASCII data file
  if (!file.exists("exam_ascii.dat")) {
    system("curl https://wwwn.cdc.gov/nchs/data/nhanes3/1a/exam.dat > exam_ascii.dat")
  }
  
  #=====Import Data==============================================================
  
  # exam_data <- read.SAScii("exam_ascii.dat",
  #                          "exam_sas_setup.sas",
  #                          zipped = F)
  
  #initial run of read.SAScii spotted some issues with the setup file, fixing
  sas_file <- read_lines("exam_sas_setup.sas")
  
  #patterns to replace and their replacements
  patterns <- c('DEPSTLC1 $6',
                'DEPSTLC1 2640-2645',
                'DEPSTLC1 = "Soft tissue lesion 1: location"',
                'DEPSTLC2 $6',
                'DEPSTLC2 2674-2679',
                'DEPSTLC2 = "Soft tissue lesion 2: location"',
                'DEPSTLC3 $6',
                'DEPSTLC3 2702-2707',
                'DEPSTLC3 = "Soft tissue lesion 3: location"',
                'DEPSTLC4 $6',
                'DEPSTLC4 2730-2732',
                'DEPSTLC4 = "Soft tissue lesion 4: location"',
                'DEPSTLC5 $6',
                'DEPSTLC5 2750-2751',
                'DEPSTLC5 = "Soft tissue lesion 5: location"',
                'DEPSTLC6 $6',
                'DEPSTLC6 2770-2771',
                'DEPSTLC6 = "Soft tissue lesion 6: locations"',
                'SPPTIME  $5',
                'SPPTIME  4498-4502',
                'SPPTIME  = "Time of day test was conducted (hh:mm)"'
  )
  
  replacements <- c('DEPSTLC1 $ $6',
                    'DEPSTLC1 $ 2640-2645',
                    'DEPSTLC1 $ = "Soft tissue lesion 1: location"',
                    'DEPSTLC2 $ $6',
                    'DEPSTLC2 $ 2674-2679',
                    'DEPSTLC2 $ = "Soft tissue lesion 2: location"',
                    'DEPSTLC3 $ $6',
                    'DEPSTLC3 $ 2702-2707',
                    'DEPSTLC3 $ = "Soft tissue lesion 3: location"',
                    'DEPSTLC4 $ $6',
                    'DEPSTLC4 $ 2730-2732',
                    'DEPSTLC4 $ = "Soft tissue lesion 4: location"',
                    'DEPSTLC5 $ $6',
                    'DEPSTLC5 $ 2750-2751',
                    'DEPSTLC5 $ = "Soft tissue lesion 5: location"',
                    'DEPSTLC6 $ $6',
                    'DEPSTLC6 $ 2770-2771',
                    'DEPSTLC6 $ = "Soft tissue lesion 6: locations"',
                    'SPPTIME $  $5',
                    'SPPTIME $  4498-4502',
                    'SPPTIME $  = "Time of day test was conducted (hh:mm)"')
  
  #apply replacements
  for (i in seq_along(patterns)) {
    sas_file <- gsub(patterns[i], replacements[i], sas_file)
  }
  
  #write the fixed file
  write_lines(sas_file, "exam_sas_setup_fixed.sas")
  
  exam_data <- read.SAScii("exam_ascii.dat",
                           "exam_sas_setup_fixed.sas",
                           zipped = F)
  
  #=====Get labels===============================================================
  
  #get data dictionary from SAS files
  var_labels <- read_lines_to_df("exam_sas_setup_fixed.sas",
                                 start_line = 5267,
                                 end_line = 7634)
  
  #add labels
  exam_data_labs <- exam_data |> 
    set_label(label = var_labels$description)
  
  #=====Export===================================================================
  
  export(exam_data_labs, "exam_raw.rds")
  
}


#=====update log file==========================================================

#write update message
message="
Downloaded raw NHANES3 data.
"

#update log
update_log(file="log_exam_download.txt",
           author="Peter T. Tanksley",
           message = message)
