
#=====PACKAGES=================================================================
library(pacman)
p_load(rio,
       tidyverse,
       tidylog,
       ###
       haven,
       janitor,
       SAScii,
       sjlabelled)

#=====FUNCTIONS================================================================

# Function to read lines from a file and convert to dataframe
read_lines_to_df <- function(file_path, start_line, end_line) {
  # Read lines from the file
  lines <- readLines(file_path)[start_line:end_line]
  
  # Trim leading whitespace from each line
  trimmed_lines <- trimws(lines, which = "left")
  
  # Define a regular expression pattern to capture the variables and descriptions
  pattern <- "^([A-Za-z0-9]+)\\s+=\\s\"([^\"]+)\"$"
  
  # Use str_match to extract the components
  matches <- str_match(trimmed_lines, pattern)
  
  # Create a dataframe from the matches
  df <- data.frame(
    variable = matches[, 2],
    description = matches[, 3],
    stringsAsFactors = FALSE
  )
  
  return(df)
}

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






