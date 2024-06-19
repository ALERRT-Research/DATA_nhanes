
source("../cleaning_packages.R")

#=====import data==============================================================

demo_nhanesc_raw <- import("../../data_raw/NHANES_C/demographics/demographics_raw.rds")

#=====recode data==============================================================

demo_nhanesc <- demo_nhanesc_raw |> 
  select(SEQN,
         year,
         gender        = RIAGENDR,
         age_screen_yr = RIDAGEYR,
         age_screen_mo = RIDAGEMN,
         race_ethn     = RIDRETH1,
         marital_stat  = DMDMARTL,
         military_vet1 = DMQMILIT,
         military_vet2 = DMQMILIZ,
         edu_adult     = DMDEDUC2,
         hh_num        = DMDHHSIZ,
         fam_income    = INDFMINC,
         fam_pir       = INDFMPIR) |> 
  mutate(study = "NHANES Continuous")

demo_nhanesc_recodes <- demo_nhanesc |> 
  mutate(race_ethn = case_when((race_ethn == "Non-Hispanic White")                       ~ "White",
                               (race_ethn == "Non-Hispanic Black")                       ~ "Black",
                               (race_ethn %in% c("Mexican American", "Other Hispanic"))  ~ "Hispanic",
                               (race_ethn == "Other Race - Including Multi-Racial")      ~ "Other",
                               TRUE ~ NA_character_),
         race_ethn = fct_infreq(race_ethn)) |> 
  mutate(marital_stat = fct_recode(marital_stat,
                                   NULL = "Refused", 
                                   NULL = "Don't know", 
                                   NULL = "Don't Know")) |> 
  mutate(military_vet = case_when((military_vet1=="Yes") ~ "Yes",
                                  (military_vet2=="Yes") ~ "Yes",
                                  (military_vet1=="No") ~ "No",
                                  (military_vet2=="No") ~ "No",
                                  TRUE ~ NA_character_)) |> 
  select(-c(military_vet1, military_vet2)) |> 
  mutate(edu_adult = case_when((edu_adult %in% c("Less Than 9th Grade", 
                                                 "Less than 9th grade",
                                                 "9-11th Grade (Includes 12th grade with no diploma)", 
                                                 "9-11th grade (Includes 12th grade with no diploma)")) ~ "did not complete HS",
                               (edu_adult %in% c("High School Grad/GED or Equivalent", 
                                                 "High school graduate/GED or equivalent")) ~ "HS graduate/GED or equivalent",
                               (edu_adult %in% c("Some College or AA degree", 
                                                 "Some college or AA degree")) ~ "Some college or AA degree",
                               (edu_adult %in% c("College Graduate or above", 
                                                 "College graduate or above")) ~ "College graduate or above",
                               TRUE ~ NA_character_),
         edu_adult = fct_relevel(as.factor(edu_adult), "did not complete HS", "HS graduate/GED or equivalent", "Some college or AA degree", "College graduate or above")) |> 
  mutate(hh_num = case_when((hh_num %in% c("7", "7 or more people in the Household")) ~ "7 or more",
                            TRUE ~ hh_num),
         hh_num = fct_relevel(as.factor(hh_num), 
                              "1",
                              "2",
                              "3",
                              "4",
                              "5",
                              "6",
                              "7 or more")) |> 
  mutate(fam_income = fct_recode(fam_income,
                                 NULL = "Over $20,000",
                                 NULL = "Under $20,000",
                                 NULL = "Refused",
                                 NULL = "Don't know"))

  

#=====NHANES-III===============================================================

demo_nhanes3_raw <- import("../../data_raw/NHANES_III/hh_adult/hh_adult_raw.rds")

demo_nhanes3  <- demo_nhanes3_raw  |> 
  select(SEQN,
         gender        = HSSEX,
         race_ethn     = DMARETHN,
         age_screen_yr = HSAGEIR,
         age_screen_mo = HSAITMOR,
         edu_adult     = HFA8R,
         marital_stat  = HFA12,
         hh_num        = HSHSIZER,
         fam_income    = HFF19R,
         fam_pir       = DMPPIR,
         military_vet  = HFA13) |> 
  mutate(study = "NHANES 3")

demo_nhanes3_recodes <- demo_nhanes3 |> 
  mutate(gender = ifelse(gender==1, "Male", "Female"),
         gender = fct_infreq(gender)) |> 
  mutate(race_ethn = case_when((race_ethn==1) ~ "White",
                               (race_ethn==2) ~ "Black",
                               (race_ethn==3) ~ "Hispanic",
                               (race_ethn==4) ~ "Other",
                               TRUE ~ NA_character_),
         race_ethn = fct_infreq(race_ethn)) |> 
  mutate(age_screen_mo = ifelse(age_screen_mo==9999, NA, age_screen_mo)) |> 
  mutate(edu_adult = case_when((edu_adult<12) ~ "did not complete HS",
                               (edu_adult==12) ~ "HS graduate/GED or equivalent",
                               (edu_adult>12 & edu_adult<16) ~ "Some college or AA degree",
                               (edu_adult>=16) ~ "College graduate or above",
                               TRUE ~ NA_character_),
         edu_adult = fct_relevel(as.factor(edu_adult), 
                                 "did not complete HS", 
                                 "HS graduate/GED or equivalent", 
                                 "Some college or AA degree", 
                                 "College graduate or above")) |> 
  mutate(marital_stat = case_when((marital_stat==1) ~ "Married",
                                  (marital_stat==2) ~ "Married (spouse not in home)",
                                  (marital_stat==3) ~ "Living with partner",
                                  (marital_stat==4) ~ "Widowed",
                                  (marital_stat==5) ~ "Divorced",
                                  (marital_stat==6) ~ "Separated",
                                  (marital_stat==7) ~ "Never married",
                                  TRUE ~ NA_character_)) |> 
  mutate(fam_income = ifelse(fam_income %in% c(88:99), NA, fam_income),
         fam_income = case_when((fam_income %in% c(0:5))   ~ "$0-4,999",
                                (fam_income %in% c(6:10))  ~ "$5k-9,999",
                                (fam_income %in% c(11:15)) ~ "10k-14,999",
                                (fam_income %in% c(16:20)) ~ "15k-19,999",
                                (fam_income==21)           ~ "20k-24,999",
                                (fam_income==22)           ~ "25k-29,999",
                                (fam_income==23)           ~ "30k-34,999",
                                (fam_income==24)           ~ "35k-39,999",
                                (fam_income==25)           ~ "40k-44,999",
                                (fam_income==26)           ~ "45k-49,999",
                                (fam_income==27)           ~ "50k and over",
                                TRUE ~ NA_character_),
         fam_income = fct_relevel(fam_income, "$0-4,999",
                                  "$5k-9,999",
                                  "10k-14,999",
                                  "15k-19,999",
                                  "20k-24,999",
                                  "25k-29,999",
                                  "30k-34,999",
                                  "35k-39,999",
                                  "40k-44,999",
                                  "45k-49,999",
                                  "50k and over")) |> 
  mutate(fam_pir = ifelse(fam_pir==888888, NA, fam_pir)) |> 
  mutate(military_vet = case_when((military_vet==1) ~ "Yes",
                                  (military_vet==2) ~ "No",
                                  TRUE ~ NA_character_)) |> 
  mutate(hh_num = ifelse(hh_num>=7, "7 or more", hh_num),
         hh_num = fct_relevel(as.factor(hh_num), 
                              "1",
                              "2",
                              "3",
                              "4",
                              "5",
                              "6",
                              "7 or more"))


#=====Combine, label, export===================================================

demo_all <- bind_rows(demo_nhanesc_recodes, demo_nhanes3_recodes)

demo_all_labs <- demo_all |> 
  var_labels(gender        = "Gender",
             age_screen_yr = "Age in years (screener)",
             age_screen_mo = "Age in months (screener)",
             race_ethn     = "Race/ethnicity",
             marital_stat  = "Marital status",
             edu_adult     = "Educational level",
             hh_num        = "Number in household",
             fam_income    = "Family income (not harmonized across studies)",
             fam_pir       = "Family poverty-income ratio",
             study         = "NHANES study",
             military_vet  = "Military veteran")

#create unique, seven digit IDs that incorporate study of origin 
#NHANES3 start with "3"
#NHANESC starts with "4" 

demo_all_id <- demo_all_labs |> 
  mutate(study_id = case_when((study=="NHANES 3") ~ paste0("3", str_pad(SEQN, width=6, side="left", pad="0")),
                              (study!="NHANES 3") ~ paste0("4", str_pad(SEQN, width=6, side="left", pad="0")))) |> 
  select(study_id, SEQN, year, everything())

#check number of adults
demo_all_id |> 
  count(adults = age_screen_yr>=18)
#   adults     n
# 1  FALSE 42544
# 2   TRUE 78822

#export data
export(demo_all_id, "demo_clean.rds")



#=====update log file==========================================================

#write update message
message="
Added new study_id variable (SEQN is NOT unique across studies). 
"

#update log
update_log(file="log_demographics.txt",
           author="Peter T. Tanksley",
           message = message)

