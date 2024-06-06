source("../cleaning_packages.R")

#--------OCUPATIONAL CODES SWITCH ACROSS YEARS, CONFIRM CODING

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

#set names
names_ocq <- c("OCQ",
               "OCQ_B",
               "OCQ_C",
               "OCQ_D",
               "OCQ_E",
               "OCQ_F",
               "OCQ_G",
               "OCQ_H",
               "OCQ_I",
               "OCQ_J")

#set years
years_ocq <- seq(1999, 2017, 2)

#BP data
df_ocq <- pull_nhanes(names_ocq, years_ocq, "OCD231|OCD241|OCD392")

#recode (NOTE: occupational code variables merged poorly)
df_ocq_recodes <- df_ocq |> 
  mutate(occ_current = case_when((is.na(OCD241)) ~ as.character(OCD240),
                                 TRUE ~ as.character(OCD241)),
         occ_longest = case_when((is.na(OCD392)) ~ as.character(OCD390),
                                 TRUE ~ as.character(OCD392))) |> 
  mutate(across(starts_with("occ"), ~ case_when((.=="1" ) ~ 'Management Occupations',
                                                (.=="2" ) ~ 'Business, Financial Operations Occupations',
                                                (.=="3" ) ~ 'Computer, Mathematical Occupations',
                                                (.=="4" ) ~ 'Architecture, Engineering Occupations',
                                                (.=="5" ) ~ 'Life, Physical, Social Science Occupations',
                                                (.=="6" ) ~ 'Community, Social Services Occupations',
                                                (.=="7" ) ~ 'Legal Occupations',
                                                (.=="8" ) ~ 'Education, Training, Library Occupations',
                                                (.=="9" ) ~ 'Arts, Design, Entertainment, Sports, Media Occupations',
                                                (.=="10") ~ 'Healthcare Practitioner, Technical Occupations',
                                                (.=="11") ~ 'Healthcare Support Occupations',
                                                (.=="12") ~ 'Protective Service Occupations',
                                                (.=="13") ~ 'Food Preparation, Serving Occupations',
                                                (.=="14") ~ 'Building & Grounds Cleaning, Maintenance Occupations',
                                                (.=="15") ~ 'Personal Care, Service Occupations',
                                                (.=="16") ~ 'Sales & Related Occupations',
                                                (.=="17") ~ 'Office, Administrative Support Occupations',
                                                (.=="18") ~ 'Farming, Fishing, Forestry Occupations',
                                                (.=="19") ~ 'Construction, Extraction Occupations',
                                                (.=="20") ~ 'Installation, Maintenance, Repair Occupations',
                                                (.=="21") ~ 'Production Occupations',
                                                (.=="22") ~ 'Transportation, Material Moving Occupations',
                                                (.=="23") ~ 'Armed Forces',
                                                (.=="98") ~ 'Blank but applicable',
                                                TRUE ~ .)))
# select(SEQN, year,
#        job_cur_occ  = OCD240,
#        job_long_occ = OCD392,
#        no_work      = OCQ380) |> 
# mutate(across(starts_with("job"), ~ifelse(.==98, NA, .))) |> 
# mutate(across(c(job_cur_occ, job_long_occ), ~case_when((is.na(.)) ~ NA_character_,
#                                                       (.==12) ~ "first responder",
#                                                       TRUE ~ "other"),
#               .names = "{col}_fr"))
#        var_labels(job_long_occ = "Occupation group code: longest job")

#check distributions by year
ridge_years("SEQN", "year", df_ocq_recodes)

#clean up environment
keep_items <- c(keep_items, "df_bp_recodes")
rm(list = setdiff(ls(), keep_items))










#=====update log file==========================================================

#write update message
message="
Set up initial codes but found major differences in occupational codes across
years (1999-2004 vs 2005 onward). Need to resolve before moving forward.
"

#update log
update_log(file="log_occupation.txt",
           author="Peter T. Tanksley",
           message = message)


