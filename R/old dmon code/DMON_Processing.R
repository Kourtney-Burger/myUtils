# DMON Processing
# Goal: Rename audio files to include timestamps, extract duty cycle log files, and separate wav files by duty cycle. Adapted from Joshua Soll's DMON Python scripts

# Required Libraries
library(stringr)
library(fs)
library(readr)
library(dplyr)
library(tuneR)
library(seewave)



# Function to split HF wav files based on duty cycle timestamps
# Function to process WAV files and split them into segments based on duty cycle timestamps
process_wav_files <- function(log_file, wav_dir, processed_dir, segment_duration = 30) {
  
  # === Load timestamp log ===
  log_df <- read_csv(paste0(processed_dir, "/", high_freq_output_csv))
  
  # Filter out rows where duty_cycle is not "$DUTY"
  log_df <- log_df[log_df$duty_cycle == "$DUTY", ]
  
  # Create full timestamp_UTC by combining year, month, day, hours, minutes, seconds
  log_df$timestamp_UTC <- ymd_hms(
    paste(log_df$year, log_df$month, log_df$day, 
          log_df$hours, log_df$minutes, log_df$seconds, sep = "-")
  )
  
  # === Create root output directory ===
  if (!dir.exists(processed_dir)) dir.create(processed_dir)
  
  # === Get list of unique log_num values ===
  log_nums <- sort(unique(log_df$log_num))
  
  # === Process each WAV file ===
  for (log_num in log_nums) {
    # Assuming WAV files have .wav3 extension and name format risso###.wav3
    wav_filename <- sprintf("risso%03d.wav3", log_num)
    wav_path <- file.path(wav_dir, wav_filename)
    
    if (!file.exists(wav_path)) {
      warning(paste("Missing WAV file:", wav_filename))
      next
    }
    
    message("Processing: ", wav_filename)
    
    # Load audio file
    wave <- readWave(wav_path)
    sr <- wave@samp.rate
    
    # Filter timestamps for this WAV file
    file_log <- log_df[log_df$log_num == log_num, ]
    file_log <- file_log[order(file_log$timestamp_UTC), ]
    
    # Use the processed_dir as the destination for all segments (no subfolders)
    output_dir <- processed_dir
    
    # Extract and save each 30s segment
    for (i in seq_len(nrow(file_log))) {
      ts <- file_log$timestamp_UTC[i]
      ts_str <- format(ts, "%Y%m%dT%H%M%S")
      
      # Calculate start and end sample indices for 30s segment
      start_sample <- ((i - 1) * segment_duration + (i - 1) * (450 - segment_duration)) * sr + 1
      end_sample <- start_sample + segment_duration * sr - 1
      
      if (end_sample > length(wave@left)) {
        warning(paste("Skipping segment", i, "- not enough data left in", wav_filename))
        break
      }
      
      # Extract segment
      segment <- extractWave(wave, from = start_sample, to = end_sample, xunit = "samples")
      
      # Create output filename with _HF suffix
      output_file <- file.path(output_dir, paste0("segment_", ts_str, "_HF.wav"))
      
      # Save segment
      writeWave(segment, output_file)
      message("Saved: ", output_file)
    }
  }
}


# REPEATED FOR LF 1 hour segments
