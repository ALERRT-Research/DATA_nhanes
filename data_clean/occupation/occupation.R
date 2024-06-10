source("../cleaning_packages.R")

#--------OCUPATIONAL CODES SWITCH ACROSS YEARS, CONFIRM CODING
#--------CODES FOR FIRST RESPONDERS ARE "18" FROM 1999-2004 AND "12" ONWARD
#--------READ IN CODEBOOK

occ_codebook <- import("nhanes_occ_census_crosswalk.xlsx") |> 
  mutate(occ_desc = str_to_lower(occ_desc))

#=====Occupation=()===================================================

# 1999="OCQ",
# 2001="OCQ_B",
# 2003="OCQ_C",
# 2005="OCQ_D",
# 2007="OCQ_E",
# 2009="OCQ_F",
# 2011="OCQ_G",
# 2013="OCQ_H",
# 2015="OCQ_I",
# 2017="OCQ_J"

#=====BLOCK 1 (1999-2003)
#set names
names_ocq <- c("OCQ",
               "OCQ_B",
               "OCQ_C")

#set years
years_ocq <- seq(1999, 2003, 2)

#BP data
df_ocq_block1 <- pull_nhanes(names_ocq, years_ocq, "OCD231|OCD241|OCD392")

#=====BLOCK 2 (2005)
#set names
names_ocq <- "OCQ_D"

#set years
years_ocq <- 2005

#BP data
df_ocq_block2 <- pull_nhanes(names_ocq, years_ocq)

#=====BLOCK 3 (2007 onward)
#set names
names_ocq <- c("OCQ_E",
               "OCQ_F",
               "OCQ_G",
               "OCQ_H",
               "OCQ_I",
               "OCQ_J")

#set years
years_ocq <- seq(2007, 2017, 2)

#BP data
df_ocq_block3 <- pull_nhanes(names_ocq, years_ocq, "OCD231|OCD241|OCD392")


#=====Recode and combine blocks================================================

#block1
df_ocq_block1_occ <- df_ocq_block1 |> 
  select(SEQN, 
         year, 
         employed= OCD150,
         unemployed_reason=OCQ380,
         occ_hrs=OCD180,
         occ_tenure_months=OCD270,
         occ_cur_code= OCD240, 
         occ_long_code= OCD390, 
         occ_long_months= OCD395) |> 
  left_join(occ_codebook |> filter(census_yr==1990) |> select(-census_yr), by=c("occ_cur_code"="occ_code")) |> 
  rename(occ_cur_desc = occ_desc) |> 
  left_join(occ_codebook |> filter(census_yr==1990) |> select(-census_yr), by=c("occ_long_code"="occ_code")) |> 
  rename(occ_long_desc = occ_desc) |> 
  mutate(occ_code_census_yr=1990)

#block2
df_ocq_block2_occ <- df_ocq_block2 |> 
  select(SEQN, 
         year, 
         employed= OCD150,
         unemployed_reason=OCQ380,
         occ_hrs=OCQ180,
         # occ_tenure_months=OCD270, #not available
         occ_cur_code= OCD241, 
         occ_long_code= OCD392, 
         occ_long_months= OCD395) |> 
  left_join(occ_codebook |> filter(census_yr==2000) |> select(-census_yr), by=c("occ_cur_code"="occ_code")) |> 
  rename(occ_cur_desc = occ_desc) |> 
  left_join(occ_codebook |> filter(census_yr==2000) |> select(-census_yr), by=c("occ_long_code"="occ_code")) |> 
  rename(occ_long_desc = occ_desc) |> 
  mutate(occ_code_census_yr=2000)

#block3
df_ocq_block3_occ <- df_ocq_block3 |> 
  select(SEQN, 
         year, 
         employed= OCD150,
         unemployed_reason=OCQ380,
         occ_hrs=OCQ180,
         occ_tenure_months=OCD270, #not available
         occ_cur_code= OCD241, 
         occ_long_code= OCD392, 
         occ_long_months= OCD395) |> 
  mutate(across(ends_with("code"), ~str_to_lower(.))) |> 
  left_join(occ_codebook |> filter(census_yr==2000) |> select(-census_yr), by=c("occ_cur_code"="occ_desc")) |> 
  rename(occ_cur_desc = occ_cur_code,
         occ_cur_code = occ_code) |> 
  left_join(occ_codebook |> filter(census_yr==2000) |> select(-census_yr), by=c("occ_long_code"="occ_desc")) |> 
  rename(occ_long_desc = occ_long_code,
         occ_long_code = occ_code) |> 
  mutate(occ_code_census_yr=2000)

#combine
df_ocq_recodes <- bind_rows(df_ocq_block1_occ, df_ocq_block2_occ, df_ocq_block3_occ) |> 
  mutate(across(ends_with("code"), ~ifelse(. %in% c(98, 99), NA, .))) |> 
  mutate(occ_cur_desc = ifelse(is.na(occ_cur_code), NA, occ_cur_desc),
         occ_long_desc = ifelse(is.na(occ_long_code), NA, occ_long_desc)) |> 
  mutate(across(c(occ_hrs, occ_long_months), ~ if_else(. %in% c(77777, 99999), NA, .))) |> 
  mutate(across(c(employed, unemployed_reason), ~ if_else(. %in% c("Refused", "Don't know"), NA_character_, .))) |> 
  #first responder variables
  mutate(first_resp_cur = case_when((is.na(occ_cur_desc)) ~ NA_character_,
                                    (occ_cur_desc=="protective service occupations") ~ "Yes",
                                    TRUE ~ "No")) |> 
  mutate(first_resp_long = case_when((is.na(occ_long_desc)) ~ NA_character_,
                                    (occ_long_desc=="protective service occupations") ~ "Yes",
                                    TRUE ~ "No")) |> 
  mutate(first_resp_ever = case_when((is.na(first_resp_cur) & is.na(first_resp_long)) ~ NA_character_,
                                     (first_resp_cur=="Yes" | first_resp_long=="Yes") ~ "Yes",
                                     TRUE ~ "No"))

table(df_ocq_recodes$first_resp_ever) #nearly 1k first responders!
#    No   Yes 
# 45602   978 


rm(list = setdiff(ls(), c("df_ocq_recodes", "update_log")))

export(df_ocq_recodes, "occ_clean.rds")


#=====update log file==========================================================

#write update message
message="
Added employment indicator. I also added a variable for why someone was not 
employed (including retirement). Finally, I added a variable for number of 
hours worked (past week) and dropped categories/values for DK/refused.
"

#update log
update_log(file="log_occupation.txt",
           author="Peter T. Tanksley",
           message = message)


