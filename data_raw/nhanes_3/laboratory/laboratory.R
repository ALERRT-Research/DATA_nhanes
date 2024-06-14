source("../cleaning_packages.R")

#=====Download Data============================================================

#get SAS code
if (!file.exists("lab_sas_setup.sas")) {
  system("curl https://wwwn.cdc.gov/nchs/data/nhanes3/1a/lab.sas > lab_sas_setup.sas")
}

#get ASCII data file
if (!file.exists("lab_ascii.dat")) {
  system("curl https://wwwn.cdc.gov/nchs/data/nhanes3/1a/lab.dat > lab_ascii.dat")
}

#=====Import Data==============================================================

#initial run of read.SAScii spotted some issues with the setup file, fixing
sas_file <- read_lines("lab_sas_setup.sas")

#patterns to replace and their replacements
patterns <- c("PHPSNTI  $5", 
              "PHPSNTI  1250-1254", 
              'PHPSNTI  = "Time participant last ate"', 
              "PHPDRTI  1257-1261", 
              'PHPDRTI  = "Time participant last drank"', 
              'PHPBEST  $5', 
              'PHPBEST  1268-1272', 
              'PHPBEST  = "Time of venipuncture"')

replacements <- c("PHPSNTI $ $5", 
                  "PHPSNTI $  1250-1254", 
                  'PHPSNTI $  = "Time participant last ate"', 
                  "PHPDRTI $  1257-1261", 
                  'PHPDRTI $  = "Time participant last drank"', 
                  'PHPBEST $  $5', 
                  'PHPBEST $  1268-1272', 
                  'PHPBEST $  = "Time of venipuncture"')

#apply replacements
for (i in seq_along(patterns)) {
  sas_file <- gsub(patterns[i], replacements[i], sas_file)
}

#write the fixed file
write_lines(sas_file, "lab_sas_setup_fixed.sas")

#read in data using SAS setup file
lab_data <- read.SAScii("lab_ascii.dat",
                        "lab_sas_setup_fixed.sas",
                        zipped = F)

#=====Get labels===============================================================

#get data dictionary from SAS files
var_labels <- read_lines_to_df("lab_sas_setup_fixed.sas",
                               start_line = 1003,
                               end_line = 1358)

#add labels
lab_data_labs <- lab_data |> 
  set_label(label = var_labels$description)

#=====Export===================================================================

export(lab_data_labs, "lab_clean.rds")


