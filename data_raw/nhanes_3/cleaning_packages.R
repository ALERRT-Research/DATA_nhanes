
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







