# DMON Log Processing
# Goal: Rename audio files to include timestamps, extract duty cycle log files, and separate wav files by duty cycle. Adapted from Joshua Soll's DMON Python scripts


#################### FUNCTIONS ####################
# Required Libraries
library(dplyr)

# Function: Convert hexadecimal timestamp and milliseconds to UTC
hex_to_datetime <- function(hex_time, hex_ms) {
  seconds <- as.numeric(strtoi(hex_time, base = 16))
  ms <- as.numeric(strtoi(hex_ms, base = 16))
  as.POSIXct(seconds + ms / 1000, origin = "1970-01-01", tz = "UTC")
}

# Function: Parse lines to only include data we want
parse_duty_lines <- function(line, log_num) {
  parts <- unlist(strsplit(line, ","))
  duty_data <- sub("\\*.*", "", parts[4])  # Remove anything after '*'
  
  # Extract hex substrings
  start_hex     <- substr(duty_data, 1, 8) # first 8 characters are seconds
  start_ms_hex  <- substr(duty_data, 9, 12) # next 4 are milliseconds
  end_hex       <- substr(duty_data, 13, 20) # next 8 are end seconds
  end_ms_hex    <- substr(duty_data, 21, 24) # next 4 are end milliseconds
  hydrophone    <- substr(duty_data, 25, 25) # LF or HF hydrophone number
  session_hex   <- substr(duty_data, 26, 33) # 
  samples_hex   <- substr(duty_data, 34, 41)
  
  # Convert hex to values
  start_time <- hex_to_datetime(start_hex, start_ms_hex)
  end_time   <- hex_to_datetime(end_hex, end_ms_hex)
  hydrophone_type <- as.integer(hydrophone)
  session_number <- as.integer(strtoi(session_hex, base = 16))
  num_samples <- as.integer(strtoi(samples_hex, base = 16))
  
  # Convert hydrophone type (2 -> LF, 3 -> HF)
  hydrophone_label <- ifelse(hydrophone_type == 3, "HF", "LF")
  
  return(data.frame(
    log_num         = log_num,
    start_time_unix = as.numeric(start_time),
    start_time_str  = format(start_time, "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
    end_time_unix   = as.numeric(end_time),
    end_time_str    = format(end_time, "%Y-%m-%d %H:%M:%OS3", tz = "UTC"),
    hydrophone_type = hydrophone_label,  # Renamed to "HF" or "LF"
    session_number  = session_number,
    num_samples     = num_samples,
    stringsAsFactors = FALSE
  ))
}


# Function: process each log file
process_log_file <- function(log_path) {
  # Extract the 3-digit log number from the filename
  log_filename <- basename(log_path)
  log_num_match <- regmatches(log_filename, regexpr("\\d{3}", log_filename))
  log_num <- log_num_match[1]  # Extract the number as a character string
  
  # Read in the log file
  log_lines <- readLines(log_path)
  
  # Filter lines to only include $DUTY
  duty_lines <- grep("\\$DUTY", log_lines, value = TRUE)
  
  # Initialize an empty list to store parsed data
  parsed_data_list <- list()
  
  # Loop through each line and parse it
  for (i in seq_along(duty_lines)) {
    line <- duty_lines[i]
    parsed_row <- parse_duty_lines(line, log_num)
    parsed_data_list[[i]] <- parsed_row
  }
  
  # Combine all rows into a single data frame
  duty_df <- do.call(rbind, parsed_data_list)
  
  return(duty_df)
}
