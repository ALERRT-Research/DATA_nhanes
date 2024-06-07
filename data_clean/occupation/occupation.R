source("../cleaning_packages.R")

#--------OCUPATIONAL CODES SWITCH ACROSS YEARS, CONFIRM CODING
#--------CODES FOR FIRST RESPONDERS ARE "18" FROM 1999-2004 AND "12" ONWARD
#--------READ IN CODEBOOK

occ_codebook <- import("nhanes_occ_census_crosswalk.xlsx") |> 
  mutate(occ_desc = str_to_lower(occ_desc))

#=====Occupation=()===================================================

# 1999="OCP",
# 2001="OCP_B",
# 2003="OCP_C",
# 2005="OCP_D",
# 2007="OCP_E",
# 2009="OCP_F",
# 2011="OCP_G",
# 2013="OCP_H",
# 2015="OCP_I",
# 2017="OCP_J"

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
  mutate(first_resp_cur = case_when((is.na(occ_cur_desc)) ~ NA_character_,
                                    (occ_cur_desc=="protective service occupations") ~ "Yes",
                                    TRUE ~ "No")) |> 
  mutate(first_resp_long = case_when((is.na(occ_long_desc)) ~ NA_character_,
                                    (occ_long_desc=="protective service occupations") ~ "Yes",
                                    TRUE ~ "No")) |> 
  mutate(first_resp_ever = case_when((is.na(first_resp_cur) & is.na(first_resp_long)) ~ NA_character_,
                                     (first_resp_cur=="Yes" | first_resp_long=="Yes") ~ "Yes",
                                     TRUE ~ "No"))

rm(list = setdiff(ls(), c("df_ocq_recodes", "update_log")))

export(df_ocq_recodes, "occ_clean.rds")


#=====update log file==========================================================

#write update message
message="
Saved first draft of occupation data. Consider adding retirement indicator in
v2"

#update log
update_log(file="log_occupation.txt",
           author="Peter T. Tanksley",
           message = message)


