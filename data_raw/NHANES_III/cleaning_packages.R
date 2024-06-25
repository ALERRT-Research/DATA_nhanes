
#=====PACKAGES=================================================================
library(pacman)
p_load(rio,
       tidyverse,
       tidylog,
       ###
       glue,
       haven,
       httr,
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

# #download NHANES (or NHIS) mortality data from ftp website
# get_nhanes_mort <- function(start_yr){
#   stem <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality"
#   dest <- glue("NHANES_{start_yr}_{start_yr+1}_MORT_2019_PUBLIC.dat")
#   system(glue("curl {stem}/NHANES_{start_yr}_{start_yr+1}_MORT_2019_PUBLIC.dat > {dest}"))
# }

#process NHANES ASCII files (from widths from NHANES website)
read_nhanes_mort <- function(data_file) {
  read_fwf(
    file = data_file,
    col_types = "iiiiiiii",
    fwf_cols(
      SEQN = c(1, 6),
      eligstat = c(15, 15),
      mortstat = c(16, 16),
      ucod_leading = c(17, 19),
      diabetes = c(20, 20),
      hyperten = c(21, 21),
      permth_int = c(43, 45),
      permth_exm = c(46, 48)
    ),
    na = c("", ".")
  )
}

#add MD5 checksums to a local file 
create_md5_file <- function(file_in, file_out="md5.txt"){
  require(glue)
  system(glue("md5 -q {file_in} | awk '{{print $0, \"{file_in}\"}}' > {file_out}"))
}


#check if checksums match up
check_md5_file <- function(file_in, md5_file="md5.txt") {
  require(glue)
  
  # Read the stored MD5 hash
  stored_hash <- readLines(md5_file)
  
  # Extract the hash and filename from the stored hash
  stored_md5 <- sub(" .*", "", stored_hash)
  stored_filename <- sub(".* ", "", stored_hash)
  
  # Calculate the MD5 hash of the new file
  new_md5 <- system(glue("md5 -q {file_in}"), intern = TRUE)
  
  # Check if the filenames match and the hashes match
  if (file_in == stored_filename && new_md5 == stored_md5) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#=====Exit message=============================================================
cat('
Make sure to update the log file using {update_log()} after you are done! \n \n
EXAMPLE:

#=====update log file==========================================================

#write update message
message="
(list of changes made)
"

#update log
update_log(file="log_[filename].txt",
           author="[name]",
           message = message)')






