#LF
# Summarize total num_samples for each LF wav from logs
lf_samples_summary <- LF_logs %>%
  group_by(source_wav) %>%
  summarise(total_log_samples = sum(num_samples, na.rm = TRUE))

# Compare to actual samples in each wav file
for (wav in LF_wavs) {
  wav_name <- basename(wav)
  wav_data <- tuneR::readWave(wav)
  actual_samples <- length(wav_data@left)
  log_samples <- lf_samples_summary$total_log_samples[lf_samples_summary$source_wav == wav_name]
  cat("File:", wav_name, 
      "| Log total samples:", log_samples, 
      "| Actual wav samples:", actual_samples, "\n")
}


#HF
# Summarize total num_samples for each HF wav from logs
hf_samples_summary <- HF_logs %>%   
  group_by(source_wav) %>%
  summarise(total_log_samples = sum(num_samples, na.rm = TRUE))

# Compare to actual samples in each wav file
for (wav in HF_wavs) {
  wav_name <- basename(wav)
  wav_data <- tuneR::readWave(wav)
  actual_samples <- length(wav_data@left)
  log_samples <- hf_samples_summary$total_log_samples[hf_samples_summary$source_wav == wav_name]
  cat("File:", wav_name, 
      "| Log total samples:", log_samples, 
      "| Actual wav samples:", actual_samples, "\n")
}
