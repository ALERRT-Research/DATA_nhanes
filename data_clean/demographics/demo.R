
source("../cleaning_packages.R")

#=====import data==============================================================

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
  demo <- import(file_path)
}


#=====recode data==============================================================
demo_recodes <- demo |> 
  select(SEQN,
         year,
         "data_release"  = SDDSRVYR,
         "gender"        = RIAGENDR,
         "age_screen_yr" = RIDAGEYR,
         "age_screen_mo" = RIDAGEMN,
         "age_exm_mo"    = RIDAGEEX,
         "race_ethn"     = RIDRETH1,
         "military_vet1" = DMQMILIT,
         "military_vet2" = DMQMILIZ,
         "citizen"       = DMDCITZN,
         "edu_child"     = DMDEDUC3,
         "edu_adult"     = DMDEDUC2,
         "hh_num"        = DMDHHSIZ,
         "hh_income"     = INDHHINC,
         "fam_income"    = INDFMINC,
         "fam_pir"       = INDFMPIR,
         "preg_stat"     = RIDEXPRG) |> 
  mutate(race_ethn = fct_infreq(race_ethn)) |> 
  mutate(military_vet = case_when((military_vet1=="Yes") ~ "yes",
                                  (military_vet2=="Yes") ~ "yes",
                                  (military_vet1=="No") ~ "no",
                                  (military_vet2=="No") ~ "no",
                                  TRUE ~ NA_character_)) |> 
  select(-c(military_vet1, military_vet2)) |> 
  mutate(citizen = case_when((citizen == "Citizen by birth or naturalization") ~ "yes",
                             (citizen == "Not a citizen of the US") ~ "no",
                             TRUE ~ NA_character_),
         citizen = fct_infreq(citizen)) |> 
  mutate(edu_child_full = case_when((edu_child %in% c("Never Attended / Kindergarten Only", "Never attended / kindergarten only")) ~ "never/kindergarden only", 
                                    (edu_child %in% c("1st Grade",  "1st grade"))  ~ "1st grade", 
                                    (edu_child %in% c("2nd Grade",  "2nd grade"))  ~ "2nd grade", 
                                    (edu_child %in% c("3rd Grade",  "3rd grade"))  ~ "3rd grade", 
                                    (edu_child %in% c("4th Grade",  "4th grade"))  ~ "4th grade", 
                                    (edu_child %in% c("5th Grade",  "5th grade"))  ~ "5th grade", 
                                    (edu_child %in% c("6th Grade",  "6th grade"))  ~ "6th grade", 
                                    (edu_child %in% c("7th Grade",  "7th grade"))  ~ "7th grade", 
                                    (edu_child %in% c("8th Grade",  "8th grade"))  ~ "8th grade", 
                                    (edu_child %in% c("9th Grade",  "9th grade"))  ~ "9th grade", 
                                    (edu_child %in% c("10th Grade", "10th grade")) ~ "10th grade", 
                                    (edu_child %in% c("11th Grade", "11th grade")) ~ "11th grade", 
                                    (edu_child %in% c("12th Grade", "12th grade")) ~ "12th grade", 
                                    (edu_child %in% c("12th Grade, No Diploma", "12th grade, no diploma")) ~ "12th grade, no diploma", 
                                    (edu_child %in% c("High School Graduate",   "High school graduate"))   ~ "High school graduate", 
                                    (edu_child %in% c("GED or Equivalent",      "GED or equivalent"))      ~ "GED or equivalent", 
                                    (edu_child == "More than high school")                                 ~ "More than high school", 
                                    (edu_child %in% c("Less Than 5th Grade",    "Less than 5th grade"))    ~ "Less than 5th grade", 
                                    (edu_child %in% c("Less Than 9th Grade",    "Less than 9th grade"))    ~ "Less than 9th grade", 
                                    TRUE ~ NA_character_)) |> 
  mutate(edu_child = case_when((edu_child_full ==  "never/kindergarden only") ~ "none", 
                               (edu_child_full %in% c("1st grade",
                                                      "2nd grade",
                                                      "3rd grade",
                                                      "4th grade",
                                                      "5th grade",
                                                      "Less than 5th grade")) ~ "5th grade or less", 
                               (edu_child_full %in% c("6th grade",
                                                      "7th grade",
                                                      "8th grade",
                                                      "9th grade",
                                                      "Less than 9th grade")) ~ "6-9th grade", 
                               (edu_child_full %in% c("10th grade",
                                                      "11th grade",
                                                      "12th grade",
                                                      "12th grade, no diploma")) ~ "10-12th grade", 
                               (edu_child_full %in% c("High school graduate","GED or equivalent")) ~ "completed high school/GED", 
                               (edu_child_full == "More than high school") ~ "more than high school", 
                               TRUE ~ NA_character_),
         edu_child = fct_relevel(as_factor(edu_child), "none", "5th grade or less","6-9th grade","10-12th grade","completed high school/GED", "more than high school")) |> 
  mutate(edu_adult = case_when((edu_adult %in% c("Less Than 9th Grade", 
                                                 "Less than 9th grade",
                                                 "9-11th Grade (Includes 12th grade with no diploma)", 
                                                 "9-11th grade (Includes 12th grade with no diploma)")) ~ "did not complete HS",
                               (edu_adult %in% c("High School Grad/GED or Equivalent", 
                                                 "High school graduate/GED or equivalent")) ~ "HS graduate/GED or equivalent",
                               (edu_adult %in% c("College Graduate or above", 
                                                 "College graduate or above")) ~ "College graduate or above",
                               TRUE ~ NA_character_),
         edu_adult = fct_relevel(as.factor(edu_adult), "did not complete HS", "HS graduate/GED or equivalent", "College graduate or above")) |> 
  select(-edu_child_full) |> 
  mutate(hh_num = case_when((hh_num %in% c("7", "7 or more people in the Household")) ~ "7 or more people in the Household",
                            TRUE ~ hh_num),
         hh_num = fct_relevel(as.factor(hh_num), 
                              "1",
                              "2",
                              "3",
                              "4",
                              "5",
                              "6",
                              "7 or more people in the Household")) |> 
  mutate(across(c(hh_income, fam_income), ~case_when((. %in% c("Over $20,000","Under $20,000","Refused","Don't know")) ~ NA, #dropped bad categories
                                                     TRUE ~ .))) |> 
  mutate(preg_stat = case_when((preg_stat == "Yes, positive lab pregnancy test or self-reported pregnant at exam") ~ "yes",
                               (preg_stat %in% c("SP not pregnant at exam", "The participant was not pregnant at exam")) ~ "no",
                               (preg_stat %in% c("Cannot ascertain if SP is pregnant at exam", "Cannot ascertain if the participant is pregnant at exam")) ~ "cannot determine",
                               TRUE ~ NA_character_),
         preg_stat = fct_infreq(preg_stat)) 

demo_labs <- demo_recodes |> 
  var_labels(citizen = "US citizen",
             edu_child = "Highest grade (minors only) (harmonized)",
             edu_adult = "Highest education level (adults)",
             hh_num = "Number of individuals living in household",
             preg_stat = "Prenancy status",
             military_vet = "Military veteran?")

#=====NHANES-III===============================================================

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
export(var_labels, "hh_adult_codebook.rds")


df_nhanes3_codebook <- import(glue("{onedrive_dir_nhanes}/NHANES_III/hh_adult/hh_adult_codebook.rds"))
df_nhanes3 <- import("../../data_raw/nhanes_3/hh_adult/hh_adult_clean.rds")

df_nhanes3_recodes <- df_nhanes3 |> 
  select(SEQN,
         gender        = HSSEX,
         race_ethn     = DMARETHN,
         age_screen_yr = HSAGEIR,
         age_screen_mo = HSAITMOR,
         edu_adult     = HFA8R,
         hh_num        = HSHSIZER,
         fam_income    = HFF19R,
         fam_pir       = DMPPIR,
         # preg
         military_vet  = HFA13
         )





#=====Export===================================================================

#check number of adults
demo_labs |> 
  count(adults = age_screen_yr>=18)
# adults     n
# FALSE 42112
#  TRUE 59204

#export data
export(demo_labs, "demo_clean.rds")



#=====update log file==========================================================

#write update message
message="
Started section for NHANES 3 data. Cannot complete bc CDC website is down.
"

#update log
update_log(file="log_demographics.txt",
           author="Peter T. Tanksley",
           message = message)

