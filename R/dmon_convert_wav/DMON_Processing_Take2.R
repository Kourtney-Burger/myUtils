# ----Load Packages----
library(dplyr)

# ----Global Variables----
# Set variables
deployment <- "risso-04142025"
raw_dir <- "R/dmon_convert_wav/dmon_test_data/raw"
processed_dir <- "R/dmon_convert_wav/dmon_test_data/processed/"

# Create a subfolder for processed data if not done so already
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir)
}

# ----Process Logs----
log_files <- list.files(path = raw_dir, pattern = "\\.log$", full.names = TRUE)
log_list <- list()

for (file in log_files) {
  lines <- readLines(file)
  split_lines <- strsplit(lines, ",")
  
  valid_lines <- split_lines[sapply(split_lines, length) <= 4]
  
  if (length(valid_lines) > 0) {
    text <- vapply(valid_lines, function(x) paste(x, collapse = ","), character(1))
    
    temp_conn <- textConnection(text)
    log_data <- read.csv(temp_conn, header = FALSE, stringsAsFactors = FALSE)
    close(temp_conn)
    
    colnames(log_data) <- c("Col1", "Col2", "Col3", "Col4")[1:ncol(log_data)]
    log_data$source_file <- basename(file)
    
    log_list[[length(log_list) + 1]] <- log_data
  }
}

logs <- dplyr::bind_rows(log_list) # Combine all data frames

# split logs to get duty cylce metadata
logs_filtered <- logs[, 3:5]
logs_filtered <- logs_filtered %>%
  filter(if_any(everything(), ~grepl("\\$DUTY", .)))

logs_filtered$start_s <- substr(logs_filtered$Col4, 1, 8) # first 8 characters are start seconds
logs_filtered$start_ms <- substr(logs_filtered$Col4, 9, 12) # next 4 are start milliseconds
logs_filtered$end_s <- substr(logs_filtered$Col4, 13, 20) # next 8 are end seconds
logs_filtered$end_ms <- substr(logs_filtered$Col4, 21, 24) # next 4 are end milliseconds
logs_filtered$hydrophone <- substr(logs_filtered$Col4, 25, 25) # LF or HF hydrophone number
logs_filtered$session <- substr(logs_filtered$Col4, 26, 33) # recording session
logs_filtered$samples <- substr(logs_filtered$Col4, 34, 41) # number of samples recorded 

# Function: Convert hexadecimal timestamp and milliseconds to UTC
hex_to_datetime <- function(hex_time, hex_ms) {
  seconds <- as.numeric(strtoi(hex_time, base = 16))
  ms <- as.numeric(strtoi(hex_ms, base = 16))
  datetime <- as.POSIXct(seconds + ms / 1000, origin = "1970-01-01", tz = "UTC")
  format(datetime, format = "%Y-%m-%d %H:%M:%OS3")
}

# Convert hex to values
logs_filtered$start_time <- hex_to_datetime(logs_filtered$start_s, logs_filtered$start_ms)
logs_filtered$end_time   <- hex_to_datetime(logs_filtered$end_s, logs_filtered$end_ms)
logs_filtered$hydrophone_type <- as.integer(logs_filtered$hydrophone)
logs_filtered$session_number <- as.integer(strtoi(logs_filtered$session, base = 16))
logs_filtered$num_samples <- as.integer(strtoi(logs_filtered$samples, base = 16))

# Convert hydrophone type (2 -> LF, 3 -> HF)
logs_filtered$hydrophone_type <- ifelse(logs_filtered$hydrophone_type == 3, "HF", "LF")

# keep and rename cleaned columns
final_df <- logs_filtered %>%
  select(source_file, session_number, start_time, end_time, hydrophone_type, num_samples)

write.csv(final_df, file = paste0(processed_dir, "/", deployment, "_logs.csv"), row.names = FALSE)

# ----Process wavs----
## ----copy and rename .wav2 and .wav3 files----
renamed_wav_dir <- file.path(raw_dir, "renamed_wav")

# Create the directory if it doesn't exist
if (!dir.exists(renamed_wav_dir)) {
  dir.create(renamed_wav_dir, recursive = TRUE)
}

# List all .wav2 and .wav3 files in raw_dir
wav_list <- list.files(path = raw_dir, pattern = '\\.wav(2|3)$', full.names = TRUE)

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

## ----segment renamed wavs using logs----
### ----Low Frequency Continuous Data----
# list all LF data from wavs and log
LF_wavs <- list.files(renamed_wav_dir, pattern = "\\_LF.wav$", full.names = TRUE)

LF_logs <- final_df %>%
  filter(hydrophone_type == "LF")

# segment data based on start times and samples from logs



### ----High Frequency Duty Cycled Data----
# list all HF data from wavs and log
HF_wavs <- list.files(renamed_wav_dir, pattern = "\\_HF.wav$", full.names = TRUE)

HF_logs <- final_df %>%
  filter(hydrophone_type == "HF")

# Idea from Selene 
# counter = 1;
# for session 1 to 10
# startsample = counter;
# end sample = counter + num_samples -1
# READ (startsample:endsample)
# counter = endsample
# end
# Selene Fregosi - NOAA Affiliate
# 4:14 PM
# counter = endsample + 1