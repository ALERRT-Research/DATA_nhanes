library(pacman)
p_load(rio,
       tidyverse,
       tidylog,
       ###
       nhanesA,
       rvest,
       SAScii,
       sjlabelled)


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



