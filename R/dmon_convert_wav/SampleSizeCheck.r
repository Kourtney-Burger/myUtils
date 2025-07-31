#LF
# Summarize total num_samples and log count for each LF wav
lf_samples_summary <- LF_logs %>%
  group_by(source_wav) %>%
  summarise(
    total_log_samples = sum(num_samples, na.rm = TRUE),
    num_log_lines = n()
  )

# Initialize results
lf_results <- list()

for (wav in LF_wavs) {
  wav_name <- basename(wav)
  wav_data <- tuneR::readWave(wav)
  
  actual_samples <- length(wav_data@left)
  sample_rate <- wav_data@samp.rate
  duration_sec <- actual_samples / sample_rate
  expected_log_lines <- duration_sec / 3600
  
  log_row <- lf_samples_summary[lf_samples_summary$source_wav == wav_name, ]
  log_samples <- if (nrow(log_row) == 0) NA else log_row$total_log_samples
  num_log_lines <- if (nrow(log_row) == 0) 0 else log_row$num_log_lines
  
  lf_results[[length(lf_results) + 1]] <- data.frame(
    file = wav_name,
    log_total_samples = log_samples,
    actual_samples = actual_samples,
    sample_rate_hz = sample_rate,
    duration_sec = round(duration_sec, 2),
    num_log_lines = num_log_lines,
    expected_log_lines = expected_log_lines
  )
}

lf_df <- do.call(rbind, lf_results)


# HF
# Summarize total num_samples for each HF wav from logs
hf_samples_summary <- HF_logs %>%
  group_by(source_wav) %>%
  summarise(total_log_samples = sum(num_samples, na.rm = TRUE))

# Initialize empty list to store results
hf_results <- list()

# Compare to actual samples in each wav file
for (wav in HF_wavs) {
  wav_name <- basename(wav)
  wav_data <- tuneR::readWave(wav)
  
  actual_samples <- length(wav_data@left)
  sample_rate <- wav_data@samp.rate
  duration_sec <- actual_samples / sample_rate
  
  log_samples <- hf_samples_summary$total_log_samples[hf_samples_summary$source_wav == wav_name]
  
  hf_results[[length(hf_results) + 1]] <- data.frame(
    file = wav_name,
    log_total_samples = ifelse(length(log_samples) == 0, NA, log_samples),
    actual_samples = actual_samples,
    sample_rate_hz = sample_rate,
    duration_sec = round(duration_sec, 2)
  )
}

# Combine list into a single data.frame
hf_df <- do.call(rbind, hf_results)
