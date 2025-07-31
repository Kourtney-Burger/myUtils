# ----Set Up----
## ----Load Packages----
library(dplyr)
library(tuneR)
library(lubridate)

## ----Global Variables----
deployment <- "risso-04142025"
raw_dir <- "C:/Users/kourtney.burger/Documents/Gliders/dmon/risso_April_deployment/raw/"
processed_dir <- "C:/Users/kourtney.burger/Documents/Gliders/dmon/risso_April_deployment/processed/"

## ----Create Processed Directory----
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir)
}

# ----Process Logs----
# list all log files in raw_dir
log_files <- list.files(path = raw_dir, pattern = "\\.log$", full.names = TRUE)

# create empty list to store log data
log_list <- list()

# read each log file, split by comma, and filter out lines with duty information
for (file in log_files) {
  # read all lines from log
  lines <- readLines(file)
  # separate lines by comma
  split_lines <- strsplit(lines, ",")
  # filter out lines with more than 4 elements (which are not duty cycle logs)
  valid_lines <- split_lines[sapply(split_lines, length) <= 4]
  # if there are valid duty cycle lines, separate them into a data frame
  if (length(valid_lines) > 0) {
    # create comma-separated string
    text <- vapply(valid_lines, function(x) paste(x, collapse = ","),
                   character(1))
    # read the string
    temp_conn <- textConnection(text)
    log_data <- read.csv(temp_conn, header = FALSE, stringsAsFactors = FALSE)
    close(temp_conn)
    # set column names
    colnames(log_data) <- c("Col1", "Col2", "Message",
                            "DataPayload")[seq_len(ncol(log_data))]
    # add source file name column
    log_data$source_file <- basename(file)
    # add log row to data list
    log_list[[length(log_list) + 1]] <- log_data
  }
}

# Combine all lists/rows into one dataframe
logs <- dplyr::bind_rows(log_list)

# split logs to get duty cylce metadata
logs_filtered <- logs[, 3:5]
logs_filtered <- logs_filtered %>%
  filter(if_any(everything(), ~grepl("\\$DUTY", .)))

# first 8 characters are start seconds
logs_filtered$start_s <- substr(logs_filtered$DataPayload, 1, 8)
# next 4 are start milliseconds
logs_filtered$start_ms <- substr(logs_filtered$DataPayload, 9, 12)
# next 8 are end seconds
logs_filtered$end_s <- substr(logs_filtered$DataPayload, 13, 20)
# next 4 are end milliseconds
logs_filtered$end_ms <- substr(logs_filtered$DataPayload, 21, 24)
# LF or HF hydrophone number
logs_filtered$hydrophone <- substr(logs_filtered$DataPayload, 25, 25)
# recording session
logs_filtered$session <- substr(logs_filtered$DataPayload, 26, 33)
# number of samples recorded
logs_filtered$samples <- substr(logs_filtered$DataPayload, 34, 41)

# helper function to convert hexadecimal timestamp and milliseconds to UTC
hex_to_datetime <- function(hex_time, hex_ms) {
  seconds <- as.numeric(strtoi(hex_time, base = 16))
  ms <- as.numeric(strtoi(hex_ms, base = 16))
  datetime <- as.POSIXct(seconds + ms / 1000, origin = "1970-01-01", tz = "UTC")
  format(datetime, format = "%Y-%m-%d %H:%M:%OS3")
}

# Convert hex to values
logs_filtered$start_time <- hex_to_datetime(logs_filtered$start_s,
                                            logs_filtered$start_ms)
logs_filtered$end_time <- hex_to_datetime(logs_filtered$end_s,
                                          logs_filtered$end_ms)
logs_filtered$hydrophone_type <- as.integer(logs_filtered$hydrophone)
logs_filtered$session_number <- as.integer(strtoi(logs_filtered$session,
                                                  base = 16))
logs_filtered$num_samples <- as.integer(strtoi(logs_filtered$samples,
                                               base = 16))

# Convert hydrophone type (2 -> LF, 3 -> HF)
logs_filtered$hydrophone_type <- ifelse(logs_filtered$hydrophone_type == 3,
                                        "HF", "LF")

# Add name of source wav file
logs_filtered <- logs_filtered %>%
  mutate(
    source_wav = ifelse(
      hydrophone_type == "LF",
      sub("\\.log$", "_LF.wav", source_file),
      sub("\\.log$", "_HF.wav", source_file)
    )
  )

# keep and rename cleaned columns
final_df <- logs_filtered %>%
  select(source_file, source_wav, session_number, start_time, end_time,
         hydrophone_type, num_samples)

# save final dataframe to CSV
write.csv(final_df, file = paste0(processed_dir, "/", deployment, "_logs.csv"),
          row.names = FALSE)

# ----Process wavs----
## ----copy and rename .wav2 and .wav3 files----
# set output directory for renamed wav files
renamed_wav_dir <- file.path(processed_dir, "renamed_wav")

# create the directory if it doesn't exist
if (!dir.exists(renamed_wav_dir)) {
  dir.create(renamed_wav_dir, recursive = TRUE)
}

# List all .wav2 and .wav3 files in raw_dir
wav_list <- list.files(raw_dir, "\\.wav(2|3)$", full.names = TRUE)

# Loop through files and copy + rename them
for (file in wav_list) {
  if (grepl("\\.wav2$", file, ignore.case = TRUE)) {
    new_name <- sub("\\.wav2$", "_LF.wav", basename(file), ignore.case = TRUE)
  } else if (grepl("\\.wav3$", file, ignore.case = TRUE)) {
    new_name <- sub("\\.wav3$", "_HF.wav", basename(file), ignore.case = TRUE)
  } else {
    next  # Skip if neither
  }
  dest_file <- file.path(renamed_wav_dir, new_name)
  file.copy(from = file, to = dest_file, overwrite = TRUE)
}

## ----Low Frequency Continuous Data----
### ----list all LF data from wavs and logs----
LF_wavs <- list.files(renamed_wav_dir, "\\_LF.wav$", full.names = TRUE)
LF_logs <- final_df %>%
  filter(hydrophone_type == "LF")

# create LF directory if it doesn't exist
LF_dir <- file.path(processed_dir, "LF")
if (!dir.exists(LF_dir)) {
  dir.create(LF_dir, recursive = TRUE)
}

### ----segment data based on start times and samples from logs----
# read in first wav file 
wav_data <- tuneR::readWave(LF_wavs[1])

# set counters 
counter <- 1
wav_counter <- 1

for (i in 1:nrow(LF_logs)) {
  # create name string and path for segmented wav
  start_time <- as.POSIXct(LF_logs$start_time[i], format = "%Y-%m-%d %H:%M:%OS")
  main_time <- format(start_time, "%Y%m%d_%H%M%S")
  ms <- sub("^[0-9]+\\.", "", format(start_time, "%OS6"))
  start_time_str <- paste0(main_time, "_", ms)
  
  # name based on wav
  # base_name <- sub("\\.wav$", "", basename(LF_wavs[wav_counter]))
  
  # name based on logs
  # base_name <- sub("\\.wav$", "", LF_logs$source_wav[i])
  
  # name without base wav ###, based on deployment id 
  base_name <- deployment
  filename <- paste0(base_name, "_", start_time_str, ".wav")
  
  out_path <- file.path(LF_dir, filename)
  
  # create start and end of segment
  start_sample <- counter # first segmented wav starts at sample #1
  end_sample <- start_sample + LF_logs$num_samples[i] - 1 # first segmented wav ends at sample length plus counter - 1
  
  # check end_sample exist in wav sample length and start an if statement
  if (end_sample <= length(wav_data@left)) {
    # if end sample is not greater then total samples, write full hour wav
    segment <- extractWave(wav_data, from = start_sample, to = end_sample,
                           xunit = "samples")
    writeWave(segment, filename = out_path)
    
    # update counter for next log entry 
    counter <- end_sample + 1
  } else if (end_sample > length(wav_data@left)){
    # if end sample is greater then total samples, save leftover and concatenate
    # with a piece of the next wav file
    # save leftover samples (start_sample to end of that wav_data@left)
    segment_leftover <- extractWave(wav_data, from = start_sample, to =
                                      length(wav_data@left), xunit = "samples")
    
    # check if we're at last wav file 
   if (wav_counter == length(LF_wavs)) {
     # save last segment from last wav file 
     writeWave(segment_leftover, filename = out_path)
     
     # end loop
     break
   } 
    
    # make object for number of samples already segmented and to be segmented
    already_segmented <- length(segment_leftover@left)
    to_be_segmented <- LF_logs$num_samples[i] - already_segmented
    
    # look at next wav
    wav_counter <- wav_counter + 1
    
    #read and extract segment from next wav
    wav_data <- tuneR::readWave(LF_wavs[wav_counter])
    next_wav_segment <- extractWave(wav_data, from = 1, to = to_be_segmented)
    
    # combine two segments and write wav
    combined_wav <- tuneR::bind(segment_leftover, next_wav_segment)
    writeWave(combined_wav, filename = out_path)
    
    counter <- to_be_segmented + 1 
  } else {
    stop("Error: End sample is not valid")
  }
}

## ----High Frequency Duty Cycled Data----
### ----list all HF data from wavs and logs----
HF_wavs <- list.files(renamed_wav_dir, "\\_HF.wav$", full.names = TRUE)
HF_logs <- final_df %>%
  filter(hydrophone_type == "HF")

# create HF directory if it doesn't exist
HF_dir <- file.path(processed_dir, "HF")
if (!dir.exists(HF_dir)) {
  dir.create(HF_dir, recursive = TRUE)
}

### ----segment data based on start times and samples from logs----
# read in first wav file 
wav_data <- tuneR::readWave(HF_wavs[1])

# set counters 
counter <- 1
wav_counter <- 1

for (i in 1:nrow(HF_logs)) {
  # create name string and path for segmented wav
  start_time <- as.POSIXct(HF_logs$start_time[i], format = "%Y-%m-%d %H:%M:%OS")
  main_time <- format(start_time, "%Y%m%d_%H%M%S")
  ms <- sub("^[0-9]+\\.", "", format(start_time, "%OS6"))
  start_time_str <- paste0(main_time, "_", ms)
  
  # name based on wav
  # base_name <- sub("\\.wav$", "", basename(HF_wavs[wav_counter]))
  
  # name based on logs
  # base_name <- sub("\\.wav$", "", HF_logs$source_wav[i])
  
  # name without base wav ###, based on deployment id 
  base_name <- deployment
  filename <- paste0(base_name, "_", start_time_str, ".wav")
  
  out_path <- file.path(HF_dir, filename)
  
  # create start and end of segment
  start_sample <- counter # first segmented wav starts at sample #1
  end_sample <- start_sample + HF_logs$num_samples[i] - 1 # first segmented wav ends at sample length plus counter - 1
  
  # check end_sample exist in wav sample length and start an if statement
  if (end_sample <= length(wav_data@left)) {
    # if end sample is not greater then total samples, write full hour wav
    segment <- extractWave(wav_data, from = start_sample, to = end_sample,
                           xunit = "samples")
    writeWave(segment, filename = out_path)
    
    # update counter for next log entry 
    counter <- end_sample + 1
  } else if (end_sample > length(wav_data@left)){
    # if end sample is greater then total samples, save leftover and concatenate
    # with a piece of the next wav file
    # save leftover samples (start_sample to end of that wav_data@left)
    segment_leftover <- extractWave(wav_data, from = start_sample, to =
                                      length(wav_data@left), xunit = "samples")
    
    # check if we're at last wav file 
    if (wav_counter == length(HF_wavs)) {
      # save last segment from last wav file 
      writeWave(segment_leftover, filename = out_path)
      
      # end loop
      break
    } 
    
    # make object for number of samples already segmented and to be segmented
    already_segmented <- length(segment_leftover@left)
    to_be_segmented <- HF_logs$num_samples[i] - already_segmented
    
    # look at next wav
    wav_counter <- wav_counter + 1
    
    #read and extract segment from next wav
    wav_data <- tuneR::readWave(HF_wavs[wav_counter])
    next_wav_segment <- extractWave(wav_data, from = 1, to = to_be_segmented)
    
    # combine two segments and write wav
    combined_wav <- tuneR::bind(segment_leftover, next_wav_segment)
    writeWave(combined_wav, filename = out_path)
    
    counter <- to_be_segmented + 1 
  } else {
    stop("Error: End sample is not valid")
  }
}




# ----OLD CODE----
# # ROUGH OUTLINE OF CHANGES FROM SELENE
# for (i in 1:nrow(LF_logs)) {
# #for testing
# #for (i in 1:29) {
#   # create name string and path for segmented wav
#   start_time_str <- format(as.POSIXct(LF_logs$start_time[i], format =
#                                         "%Y-%m-%d %H:%M:%OS"), "%Y%m%d_%H%M%S")
#   base_name <- sub("\\.wav$", "", LF_logs$source_wav[i])
#   filename <- paste0(base_name, "_", start_time_str, ".wav")
#   
#   out_path <- file.path(LF_dir, filename)
#   
#   # create start and end of segment
#   start_sample <- counter # first segmented wav starts at sample #1
#   end_sample <- start_sample + LF_logs$num_samples[i] - 1 # first segmented wav ends at sample length plus counter - 1
#   
#   # check end_sample exist in wav sample length and start an if statement
#   if (end_sample > length(wav_data@left)) {
#     # if end sample is greater then total samples, save leftover and concatenate
#     # with a piece of the next wav file
#     # save leftover samples (start_sample to end of that wav_data@left)
#     segment_leftover <- extractWave(wav_data, from = start_sample, to =
#                                       length(wav_data@left), xunit = "samples")
#     
#     # make object for number of samples already segmented and to be segmented
#     already_segmented <- length(segment_leftover@left)
#     to_be_segmented <- LF_logs$num_samples[i] - already_segmented
#     
#     # check if this is the last log entry for wav file
#     # THIS IS NOT WORKING AS EXPECTED - for transition from risso2 to risso3, this soruce is listed as risso003 because we have already jumped to i = 31, so the next if statement wil be false
#     source <- LF_logs$source_wav[i]
#     
#     if (LF_logs$source_wav[i+1] != source) { # this is not working because sometimes i is more than 1 ahead of the previous wav file (line 32 in excel example)
#       # last log row for source wav so read in next wav file and save as segment
#       wav_file <- LF_logs$source_wav[i+1]
#       wav_data <- tuneR::readWave(paste0(renamed_wav_dir, "/", wav_file))
#       next_wav_segment <- extractWave(wav_data, from = 1, to = to_be_segmented)
#       
#       # combine two segments and write wav
#       combined_wav <- tuneR::bind(segment_leftover, next_wav_segment)
#       writeWave(combined_wav, filename = out_path)
#       
#       counter <- to_be_segmented + 1
#     } else {
#       stop("Error: End sample is greater than the total wav samples but this is not the last row of a log source")  
#     } 
#   } else {
#     # if end sample is not greater then total samples, write full hour wav
#     segment <- extractWave(wav_data, from = start_sample, to = end_sample,
#                            xunit = "samples")
#     writeWave(segment, filename = out_path)
#     
#     # update counter for next wav
#     counter <- end_sample + 1
#   }
# }

# ROUGH OUTLINE OF CHANGES FROM SELENE
# for (i in 1:nrow(LF_logs)) {
# #for testing
# #for (i in 1:29) {
#   # create name string and path for segmented wav
#   start_time_str <- format(as.POSIXct(LF_logs$start_time[i], format =
#                                         "%Y-%m-%d %H:%M:%OS"), "%Y%m%d_%H%M%S")
#   base_name <- sub("\\.wav$", "", LF_logs$source_wav[i])
#   filename <- paste0(base_name, "_", start_time_str, ".wav")
#   
#   out_path <- file.path(LF_dir, filename)
#   
#   # create start and end of segment
#   start_sample <- counter # first segmented wav starts at sample #1
#   end_sample <- start_sample + LF_logs$num_samples[i] - 1 # first segmented wav ends at sample length plus counter - 1
#   
#   # check end_sample exist in wav sample length and start an if statement
#   if (end_sample > length(wav_data@left)) {
#     # if end sample is greater then total samples, save leftover and concatenate
#     # with a piece of the next wav file
#     # save leftover samples (start_sample to end of that wav_data@left)
#     segment_leftover <- extractWave(wav_data, from = start_sample, to =
#                                       length(wav_data@left), xunit = "samples")
#     
#     # make object for number of samples already segmented and to be segmented
#     already_segmented <- length(segment_leftover@left)
#     to_be_segmented <- LF_logs$num_samples[i] - already_segmented
#     
#     # check if this is the last log entry for wav file
#     # THIS IS NOT WORKING AS EXPECTED - for transition from risso2 to risso3, this soruce is listed as risso003 because we have already jumped to i = 31, so the next if statement wil be false
#     source <- LF_logs$source_wav[i]
#     
#     if (LF_logs$source_wav[i+1] != source) { # this is not working because sometimes i is more than 1 ahead of the previous wav file (line 32 in excel example)
#       # last log row for source wav so read in next wav file and save as segment
#       wav_file <- LF_logs$source_wav[i+1]
#       wav_data <- tuneR::readWave(paste0(renamed_wav_dir, "/", wav_file))
#       next_wav_segment <- extractWave(wav_data, from = 1, to = to_be_segmented)
#       
#       # combine two segments and write wav
#       combined_wav <- tuneR::bind(segment_leftover, next_wav_segment)
#       writeWave(combined_wav, filename = out_path)
#       
#       counter <- to_be_segmented + 1
#     } else {
#       stop("Error: End sample is greater than the total wav samples but this is not the last row of a log source")  
#     } 
#   } else {
#     # if end sample is not greater then total samples, write full hour wav
#     segment <- extractWave(wav_data, from = start_sample, to = end_sample,
#                            xunit = "samples")
#     writeWave(segment, filename = out_path)
#     
#     # update counter for next wav
#     counter <- end_sample + 1
#   }
# }



# # ROUGH OUTLINE OF CHANGES FROM SELENE
# for (i in 1:nrow(LF_logs)) {
# #for testing
# #for (i in 1:29) {
#   # create name string and path for segmented wav
#   start_time_str <- format(as.POSIXct(LF_logs$start_time[i], format =
#                                         "%Y-%m-%d %H:%M:%OS"), "%Y%m%d_%H%M%S")
#   base_name <- sub("\\.wav$", "", LF_logs$source_wav[i])
#   filename <- paste0(base_name, "_", start_time_str, ".wav")
#   
#   out_path <- file.path(LF_dir, filename)
#   
#   # create start and end of segment
#   start_sample <- counter # first segmented wav starts at sample #1
#   end_sample <- start_sample + LF_logs$num_samples[i] - 1 # first segmented wav ends at sample length plus counter - 1
#   
#   # check end_sample exist in wav sample length and start an if statement
#   if (end_sample > length(wav_data@left)) {
#     # if end sample is greater then total samples, save leftover and concatenate
#     # with a piece of the next wav file
#     # save leftover samples (start_sample to end of that wav_data@left)
#     segment_leftover <- extractWave(wav_data, from = start_sample, to =
#                                       length(wav_data@left), xunit = "samples")
#     
#     # make object for number of samples already segmented and to be segmented
#     already_segmented <- length(segment_leftover@left)
#     to_be_segmented <- LF_logs$num_samples[i] - already_segmented
#     
#     # check if this is the last log entry for wav file
#     # THIS IS NOT WORKING AS EXPECTED - for transition from risso2 to risso3, this soruce is listed as risso003 because we have already jumped to i = 31, so the next if statement wil be false
#     source <- LF_logs$source_wav[i]
#     
#     if (LF_logs$source_wav[i+1] != source) { # this is not working because sometimes i is more than 1 ahead of the previous wav file (line 32 in excel example)
#       # last log row for source wav so read in next wav file and save as segment
#       wav_file <- LF_logs$source_wav[i+1]
#       wav_data <- tuneR::readWave(paste0(renamed_wav_dir, "/", wav_file))
#       next_wav_segment <- extractWave(wav_data, from = 1, to = to_be_segmented)
#       
#       # combine two segments and write wav
#       combined_wav <- tuneR::bind(segment_leftover, next_wav_segment)
#       writeWave(combined_wav, filename = out_path)
#       
#       counter <- to_be_segmented + 1
#     } else {
#       stop("Error: End sample is greater than the total wav samples but this is not the last row of a log source")  
#     } 
#   } else {
#     # if end sample is not greater then total samples, write full hour wav
#     segment <- extractWave(wav_data, from = start_sample, to = end_sample,
#                            xunit = "samples")
#     writeWave(segment, filename = out_path)
#     
#     # update counter for next wav
#     counter <- end_sample + 1
#   }
# }

# TRIED DIFFERENT IF STATEMENTS FOR SCENARIOS
# for (i in 1:nrow(LF_logs)) {
# # for testing
# # for (i in 1:28) {
#   # create name string and path for segmented wav
#   start_time_str <- format(as.POSIXct(LF_logs$start_time[i], format =
#                                         "%Y-%m-%d %H:%M:%OS"), "%Y%m%d_%H%M%S")
#   base_name <- sub("\\.wav$", "", LF_logs$source_wav[i])
#   filename <- paste0(base_name, "_", start_time_str, ".wav")
# 
#   out_path <- file.path(LF_dir, filename)
# 
#   # create start and end of segment
#   start_sample <- counter # first segmented wav starts at sample #1
#   end_sample <- start_sample + LF_logs$num_samples[i] - 1 # first segmented wav ends at sample length plus counter - 1
# 
#   # set object for wav we are working with
#   source <- LF_logs$source_wav[i]
#   
#   # check if end_sample exist in wav sample length and start an if statement
#   if (end_sample > length(wav_data@left) || LF_logs$source_wav[i+1] != source) {
#     # if end sample is greater then total samples or this is the last log for wav, 
#     # save leftover and concatenate with a piece of the next wav file
#     # save leftover samples (start_sample to end of that wav_data@left)
#     segment_leftover <- extractWave(wav_data, from = start_sample, to =
#                                        length(wav_data@left), xunit = "samples")
# 
#     # make object for number of samples already segmented and to be segmented
#     already_segmented <- length(segment_leftover@left)
#     to_be_segmented <- LF_logs$num_samples[i] - already_segmented
#     
#     
#     # this is the last row, read next wav and concatenate
#     wav_file <- LF_logs$source_wav[i+1]
#     wav_data <- tuneR::readWave(paste0(renamed_wav_dir, "/", wav_file))
#     next_wav_segment <- extractWave(wav_data, from = 1, to = to_be_segmented)
#     
#     # combine two segments and write wav
#     combined_wav <- tuneR::bind(segment_leftover, next_wav_segment)
#     writeWave(combined_wav, filename = out_path)
#     
#     counter <- to_be_segmented + 1
#   } else {
#     # if end sample is not greater then total samples or this is not the last 
#     # log for a wav, write full hour wav
#     segment <- extractWave(wav_data, from = start_sample, to = end_sample,
#                            xunit = "samples")
#     writeWave(segment, filename = out_path)
#     
#     # update counter for next wav
#     counter <- end_sample + 1
#   }
# }


# SET UP IF STATEMENTS INCORRECTLY - TRYING TO APPLY CHANGES BUT NOT WORKING FOR ALL SCENARIOS 
# for (i in 1:nrow(LF_logs)) {
# # for testing
# # for (i in 1:28) {
#     # create name string and path for segmented wav
#   start_time_str <- format(as.POSIXct(LF_logs$start_time[i], format =
#                                         "%Y-%m-%d %H:%M:%OS"), "%Y%m%d_%H%M%S")
#   base_name <- sub("\\.wav$", "", LF_logs$source_wav[i])
#   filename <- paste0(base_name, "_", start_time_str, ".wav")
#   
#   out_path <- file.path(LF_dir, filename)
#   
#   # create start and end of segment
#   start_sample <- counter # first segmented wav starts at sample #1
#   end_sample <- start_sample + LF_logs$num_samples[i] - 1 # first segmented wav ends at sample length plus counter - 1
#   
#   # check end_sample exist in wav sample length and start an if statement
#   if (end_sample > length(wav_data@left)) {
#     # if end sample is greater then total samples, save leftover and concatenate
#     # with a piece of the next wav file
#     # save leftover samples (start_sample to end of that wav_data@left)
#     segment_leftover <- extractWave(wav_data, from = start_sample, to =
#                                        length(wav_data@left), xunit = "samples")
#     
#     # make object for number of samples already segmented and to be segmented
#     # already_segmented <- length(segment_leftover@left)
#     to_be_segmented <- LF_logs$num_samples[i] - already_segmented
#     
#     # check if this is the last log entry for wav file
#     source <- LF_logs$source_wav[i]
#     
#     if (i < nrow(LF_logs) && LF_logs$source_wav[i+1] == source) {
#       # this is not the last row, raise error
#       stop(paste("Segment overflows source_wav", source, "but it's not the last log entry for it. Check LF_logs."))
#     } else if (i < nrow(LF_logs)) {
#       # this is the last row, read next wav and concatenate
#       wav_file <- LF_logs$source_wav[i+1]
#       wav_data <- tuneR::readWave(paste0(renamed_wav_dir, "/", wav_file))
#       next_wav_segment <- extractWave(wav_data, from = 1, to = to_be_segmented)
#       
#       # combine two segments and write wav
#       combined_wav <- tuneR::bind(segment_leftover, next_wav_segment)
#       writeWave(combined_wav, filename = out_path)
#       
#       counter <- to_be_segmented + 1 
#     } else {
#       # this is the last row in LF_logs
#       stop("Reached last log row and cannot segment across files.")
#     }
#   } else {
#     # if end sample is not greater then total samples, write full hour wav
#     segment <- extractWave(wav_data, from = start_sample, to = end_sample,
#                            xunit = "samples")
#     writeWave(segment, filename = out_path)
#     
#     # update counter for next wav
#     counter <- end_sample + 1
#   }
# }