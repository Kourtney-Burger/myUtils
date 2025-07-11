# DMON WAV Processing
# Goal: Use parsed DMON logs (csv created in DMON_LogProcess.R) to rename WAV files and separate duty cycled recordings 


#################### FUNCTIONS ####################
# Required Libraries
library(dplyr)
library(tuneR)   
library(seewave) 

# FUNCTION: Simple renamer function to switch from .wav2 and .wav3 to _LF.wav and _HF.wav
DMON_audioRename <- function(wav_path, processed_dir) {
  # List .wav2 and .wav3 files in wav_path
  wav_list <- list.files(path = wav_path, pattern = '\\.wav(2|3)$', full.names = TRUE)
  
  # Rename and move each file
  for (file in wav_list) {
    if (grepl("\\.wav2$", file, ignore.case = TRUE)) {
      new_name <- sub("\\.wav2$", "_LF.wav", basename(file), ignore.case = TRUE)
    } else if (grepl("\\.wav3$", file, ignore.case = TRUE)) {
      new_name <- sub("\\.wav3$", "_HF.wav", basename(file), ignore.case = TRUE)
    } else {
      next
    }
    
    dest_file <- file.path(processed_dir, new_name)
    file.rename(file, dest_file)
  }
}


# Helper FUNCTION: Segment a wav file based on start and end times from the CSV
segment_wav_file <- function(wav_file, start_time, end_time, output_dir) {
  # Read the wav file
  wav_data <- readWave(wav_file)
  
  # Convert start_time and end_time from UNIX timestamps to seconds
  start_sample <- as.integer(start_time_unix)  # Convert to sample number
  end_sample <- as.integer(end_time_unix)     # Convert to sample number
  
  # Extract the chunk from the wav file
  wav_chunk <- extractWave(wav_data, from = start_sample, to = end_sample)
  
  # Create the output filename
  new_filename <- paste0("segment_", basename(wav_file), "_", start_time, "_", end_time, ".wav")
  
  # Write the chunk to the output directory
  output_path <- file.path(output_dir, new_filename)
  writeWave(wav_chunk, output_path)
}


# FUNCTION: Process WAV files and segment them based on CSV duty cycle data
process_wav_files <- function(csv_file, processed_dir, output_dir, dep_ID) {
  # Read the CSV data
  duty_data <- read.csv(paste0(processed_dir, "/", dep_ID, "_DutyCycles.csv"), stringsAsFactors = FALSE)
  
  # Convert times to POSIXct
  duty_data$start_time_str <- as.POSIXct(duty_data$start_time_str, tz = "UTC")
  duty_data$end_time_str   <- as.POSIXct(duty_data$end_time_str, tz = "UTC")
  
  # Loop through each row in the duty_data CSV
  for (i in 1:nrow(duty_data)) {
    row <- duty_data[i, ]
    log_num <- row$log_num
    hydrophone_type <- row$hydrophone_type
    start_time <- row$start_time_str
    start_time_unix <- row$start_time_unix
    end_time_unix <- row$end_time_unix
    
    # Identify the corresponding wav file (LF or HF)
    wav_filename <- paste0("risso", sprintf("%03d", log_num), "_", hydrophone_type, ".wav")
    wav_file <- file.path(processed_dir, wav_filename)
    
    # Check if the wav file exists
    if (!file.exists(wav_file)) {
      warning("WAV file not found: ", wav_file)
      next
    }
    
    # Build the output filename and path
    time_str <- format(start_time, "%Y%m%d%H%M%S")
    new_filename <- paste0("risso_", hydrophone_type, "_", time_str, ".wav")
    output_file <- file.path(output_dir, new_filename)
    
    # Segment and write the file
    segment_wav_file(wav_file, start_time_unix, end_time_unix, output_file)
    
    cat("Segment saved:", output_file, "\n")
  }
  
  cat("Finished processing all WAV files.\n")
}



########### FUNCTIONS COMBINED ##############
# Helper FUNCTION: Segment a wav file based on start and end times from the CSV
segment_wav_file <- function(wav_file, start_time_unix, end_time_unix, start_wav_time_unix, samp_rate, output_dir) {
  # Read the wav file
  wav_data <- readWave(wav_file)
  
  # Calculate the sample indices based on the recording start time (first log_num)
  start_sample <- as.integer((start_time_unix - start_wav_time_unix) * samp_rate)
  end_sample <- as.integer((end_time_unix - start_wav_time_unix) * samp_rate)
  
  # Ensure that start_sample is not greater than end_sample
  if (start_sample >= end_sample) {
    warning("Start sample is greater than or equal to end sample. Skipping.")
    return(NULL)
  }
  
  # Extract the chunk from the wav file
  wav_chunk <- extractWave(wav_data, from = start_sample, to = end_sample)
  
  # Create the output filename based on the format risso_HF_yyyymmddhhmmss.wav
  time_str <- format(as.POSIXct(start_time_unix, origin = "1970-01-01", tz = "UTC"), "%Y%m%d%H%M%S")
  hydrophone_type <- basename(wav_file)
  hydrophone_type <- sub("^risso[0-9]+_", "", hydrophone_type)
  hydrophone_type <- sub(".wav$", "", hydrophone_type)
  
  new_filename <- paste0("risso_", hydrophone_type, "_", time_str, ".wav")
  
  # Write the chunk to the output directory
  output_path <- file.path(output_dir, new_filename)
  writeWave(wav_chunk, output_path)
  
  cat("Segment saved:", output_path, "\n")
}




# FUNCTION: Process WAV files and segment them based on CSV duty cycle data
process_wav_files <- function(csv_file, processed_dir, output_dir, dep_ID) {
  # Read the CSV data
  duty_data <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  # Loop through the duty data and process each log_num
  for (i in 1:nrow(duty_data)) {
    row <- duty_data[i, ]
    
    log_num <- row$log_num
    hydrophone_type <- row$hydrophone_type
    start_time_unix <- row$start_time_unix
    end_time_unix <- row$end_time_unix
    
    # Identify the corresponding wav file (LF or HF)
    wav_filename <- paste0("risso", sprintf("%03d", log_num), "_", hydrophone_type, ".wav")
    wav_file <- file.path(processed_dir, wav_filename)
    
    # Check if the wav file exists
    if (!file.exists(wav_file)) {
      warning("WAV file not found: ", wav_file)
      next
    }
    
    # Get the recording start time from the first row of the current log_num
    if (i == 1 || duty_data$log_num[i] != duty_data$log_num[i - 1]) {
      start_wav_time_unix <- row$start_time_unix  # Recording start time for this log_num
    }
    
    # Read header to get sample rate of the WAV file
    header <- readWave(wav_file, header = TRUE)
    samp_rate <- header$sample.rate  # Sample rate from WAV file
    
    # Segment and write the file using the segment_wav_file function
    segment_wav_file(wav_file, start_time_unix, end_time_unix, start_wav_time_unix, samp_rate, output_dir)
  }
  
  cat("Finished processing all WAV files.\n")
}
