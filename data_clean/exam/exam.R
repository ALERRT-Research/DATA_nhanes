source("../cleaning_packages.R")

#=====import codebook==========================================================
# url <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Examination&Cycle="
# webpage <- read_html(url)
# table_data <- html_table(html_nodes(webpage, "table"), fill = TRUE)[[1]]
# 
# #export
# export(table_data, "nhanes_exam_codebook.csv")

#import
codebook <- import("nhanes_exam_codebook.csv")

#=====define function to pull/process data=====================================

process_data <- function(dataframes, years, mismatch_regex = NULL) {
  # Retrieve data
  data_list <- map2(dataframes, years, ~{
    df <- nhanes(.x, includelabels = TRUE) |>
      mutate(year = .y)
    if (!is.null(mismatch_regex) && mismatch_regex != "" && 
        any(grepl(mismatch_regex, names(df)))) {
      df <- df |> mutate(across(matches(mismatch_regex), ~as.character(.)))
    }
    return(df)
  })
  
  # Bind dataframes
  data_combined <- bind_rows(data_list)
  
  # Define function to get labels
  get_labs <- function(df) {
    enframe(get_label(df))
  }
  
  # Extract labels (remove weight variables FOR NOW)
  labs_list <- map(data_list, get_labs) %>%
    bind_rows() %>%
    distinct(name, .keep_all=TRUE) 
  
  # Add labels to dataframe
  data_labelled <- data_combined |>
    set_label(label = labs_list$value) |> 
    select(SEQN, year, everything())
  
  return(data_labelled)
}


#check for outliers - Density by year
ridge_years <- function(id, year, df) {
  df |> 
    select(id, year, where(is.numeric)) |> 
    pivot_longer(-c(id, year),
                 names_to="vars",
                 values_to="vals") |>
    ggplot(aes(x=vals, y=as.factor(year))) + 
    geom_density_ridges() +
    facet_wrap(~vars, scales = "free_x") #year-to-year looks good
}

#=====blood pressure (resting)=================================================

# 1999="BPX",
# 2001="BPX_B",
# 2003="BPX_C",
# 2005="BPX_D",
# 2007="BPX_E",
# 2009="BPX_F",
# 2011="BPX_G",
# 2013="BPX_H",
# 2015="BPX_I",
# 2017="BPX_J"

#set names
names_bp <- c("BPX",
              "BPX_B",
              "BPX_C",
              "BPX_D",
              "BPX_E",
              "BPX_F",
              "BPX_G",
              "BPX_H",
              "BPX_I",
              "BPX_J")

#set years
years_bp <- seq(1999, 2017, 2)

#BP data
df_bp <- process_data(names_bp, years_bp, "BPAEN1")

#check distributions by year
# ridge_years("SEQN", "year", df_bp)


#=====Grip=====================================================================

# 2011="MGX_G",
# 2013="MGX_H",


#set names
names_grip <- c("MGX_G",
              "MGX_H")

#set years
years_grip <- c(2011, 2013)

#grip data
df_grip <- process_data(names_grip, years_grip)

#check distributions by year
# ridge_years("SEQN", "year", df_bp)







