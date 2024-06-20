source("../cleaning_packages.R")

#=====!!!WARNING!!!============================================================
# Occupational codes DO NOT MATCH UP ACROSS YEARS! Exercise EXTREME CAUTION 
# when attempting to merge variables across years (OR STUDIES)!!!
# 
# Need a method of harmonizing occupational codes (consider Harmonized 
# NHANES project)
#==============================================================================

occ_nhanesc <- import("../../data_raw/NHANES_C/occupation/occupation_raw.rds")

occ_crosswalk <- import("nhanes_occ_census_crosswalk.xlsx") |> 
  mutate(occ_desc = str_to_lower(occ_desc))

occ_nhanesc_recodes <- occ_nhanesc |> 
  mutate(across(ends_with("code"), ~ifelse(. %in% c(98, 99), NA, .))) |> 
  mutate(occ_cur_desc = ifelse(is.na(occ_cur_code), NA, occ_cur_desc),
         occ_long_desc = ifelse(is.na(occ_long_code), NA, occ_long_desc)) |> 
  mutate(across(c(occ_hrs, occ_long_months), ~ if_else(. %in% c(77777, 99999), NA, .))) |> 
  mutate(across(c(employed, unemployed_reason), ~ if_else(. %in% c("Refused", "Don't know"), NA_character_, .))) |> 
  mutate(employed = str_remove_all(employed, "[?,].*")) |>
  mutate(labor_market_stat = case_when((is.na(employed) & is.na(unemployed_reason)) ~ NA_character_,
                                       (is.na(unemployed_reason)) ~ employed,
                                       TRUE ~ unemployed_reason)) |> 
  mutate(employ_stat = case_when((is.na(labor_market_stat)) ~ NA_character_,
                                 (labor_market_stat=="Working at a job or business") ~ "employed",
                                 TRUE ~ "unemployed")) |> 
  #first responder variables
  mutate(first_resp_cur = case_when((is.na(occ_cur_desc)) ~ NA_character_,
                                    (occ_cur_desc=="protective service occupations") ~ "Yes",
                                    TRUE ~ "No")) |> 
  mutate(first_resp_long = case_when((is.na(occ_long_desc)) ~ NA_character_,
                                    (occ_long_desc=="protective service occupations") ~ "Yes",
                                    TRUE ~ "No")) |> 
  mutate(first_resp_ever = case_when((is.na(first_resp_cur) & is.na(first_resp_long)) ~ NA_character_,
                                     (first_resp_cur=="Yes" | first_resp_long=="Yes") ~ "Yes",
                                     TRUE ~ "No")) |> 
  select(-c(employed, unemployed_reason))

table(occ_nhanesc_recodes$first_resp_ever) #nearly 1k first responders!
#    No   Yes 
# 45602   978 


rm(list = setdiff(ls(), c("df_ocq_recodes", "update_log")))

export(df_ocq_recodes, "occ_clean.rds")


#=====update log file==========================================================

#write update message
message="
Started work on reformatting NHC data. Will add NH3 data afterward.
"

#update log
update_log(file="log_occupation.txt",
           author="Peter T. Tanksley",
           message = message)


