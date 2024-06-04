library(pacman)
p_load(rio,
       tidyverse,
       tidylog,
       ###
       ggridges,
       janitor,
       naniar,
       nhanesA,
       rvest,
       sjlabelled,
       units)


#=====define function to pull/process data=====================================

pull_nhanes <- function(dataframes, years, mismatch_regex = NULL) {
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

#=====define function to initialize/update logs

#function for creating/updating a log file for each directory ()
update_log <- function(file, author, message) {
  # Get the current date
  sysdate <- format(Sys.Date(), "%m-%d-%Y")
  # Wrap the message text
  wrapped_message <- paste(strwrap(message, width = 80), collapse = "\n")
  
  # Check if the file exists
  if (file.exists(file)) {
    # Read existing contents of the file
    existing_content <- readLines(file)
    
    # Append new content to the top
    new_content <- paste(sysdate, author, "",  "Message:", wrapped_message, "","", sep = "\n")
    all_content <- c(new_content, existing_content)
    
    # Write all content back to the file
    writeLines(all_content, file)
  } else {
    # If file doesn't exist, create a new one
    write(paste(sysdate, author, "",  "Message:", wrapped_message, "","", sep = "\n"), file)
  }
}


#=====define function to check for outliers across years=======================

#check for outliers - Density by year
ridge_years <- function(id=id, year=year, df=df) {
  df |> 
    select(id, year, where(is.numeric)) |> 
    pivot_longer(-c(id, year),
                 names_to="vars",
                 values_to="vals") |>
    ggplot(aes(x=vals, y=as.factor(year))) + 
    geom_density_ridges() +
    facet_wrap(~vars, scales = "free_x") #year-to-year looks good
}


#check for outliers - boxplots by year
boxplot_years <- function(id=id, year=year, df=df) {
  df |> 
    select(id, year, where(is.numeric)) |> 
    pivot_longer(-c(id, year),
                 names_to="vars",
                 values_to="vals") |>
    ggplot(aes(x=vals, y=as.factor(year))) + 
    geom_boxplot() +
    facet_wrap(~vars, scales = "free_x") #year-to-year looks good
}


#check for outliers - barplot by year
barplot_years <- function(id=id, year=year, df=df) {
  df |> 
    select(id, year, where(is.numeric)) |> 
    pivot_longer(-c(id, year),
                 names_to="vars",
                 values_to="vals") |>
    ggplot(aes(x=vals)) + 
    geom_bar() +
    facet_wrap(year~vars, scales = "free_x") #year-to-year looks good
}


#=====Exit message=============================================================
cat("Make sure to update the log file using {update_log()} after you're done! \n \n")
cat('EXAMPLE:

#=====update log file==========================================================

#write update message
message="
(list of changes made)
"

#update log
update_log(file="log_[filename].txt",
           author="[name]",
           message = message)')



