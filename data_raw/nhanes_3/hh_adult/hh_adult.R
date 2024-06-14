source("../cleaning_packages.R")

#=====Download Data============================================================

#get SAS code
if (!file.exists("hh_adult_sas_setup.sas")) {
  system("curl https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.sas > hh_adult_sas_setup.sas")
}

#get ASCII data file
if (!file.exists("hh_adult_ascii.dat")) {
  system("curl https://wwwn.cdc.gov/nchs/data/nhanes3/1a/adult.dat > hh_adult_ascii.dat")
}

#=====Import Data==============================================================

# hh_adult_data <- read.SAScii("hh_adult_ascii.dat",
#                              "hh_adult_sas_setup.sas",
#                              zipped = F)

#initial run of read.SAScii spotted some issues with the setup file, fixing
sas_file <- read_lines("hh_adult_sas_setup.sas")

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
write_lines(sas_file, "hh_adult_sas_setup_fixed.sas")

hh_adult_data <- read.SAScii("hh_adult_ascii.dat",
                             "hh_adult_sas_setup_fixed.sas",
                             zipped = F)

#=====Get labels===============================================================

#get data dictionary from SAS files
var_labels <- read_lines_to_df("hh_adult_sas_setup_fixed.sas",
                               start_line = 2632,
                               end_line = 3869)

#add labels
hh_adult_data_labs <- hh_adult_data |> 
  set_label(label = var_labels$description)

#=====Export===================================================================

export(hh_adult_data_labs, "hh_adult_clean.rds")

















