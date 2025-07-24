# ----Set Up----
## ----Load Packages----
library(dplyr)
library(tuneR)

## ----Global Variables----
deployment <- "risso-04142025"
raw_dir <- "R/dmon_convert_wav/dmon_test_data/raw"
processed_dir <- "R/dmon_convert_wav/dmon_test_data/processed/"

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
logs_filtered$start_s <- substr(logs_filtered$Col4, 1, 8)
# next 4 are start milliseconds
logs_filtered$start_ms <- substr(logs_filtered$Col4, 9, 12)
# next 8 are end seconds
logs_filtered$end_s <- substr(logs_filtered$Col4, 13, 20)
# next 4 are end milliseconds
logs_filtered$end_ms <- substr(logs_filtered$Col4, 21, 24)
# LF or HF hydrophone number
logs_filtered$hydrophone <- substr(logs_filtered$Col4, 25, 25)
# recording session
logs_filtered$session <- substr(logs_filtered$Col4, 26, 33)
# number of samples recorded
logs_filtered$samples <- substr(logs_filtered$Col4, 34, 41)

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
renamed_wav_dir <- file.path(raw_dir, "renamed_wav")

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

## ----segment renamed wavs using logs----
### ----Low Frequency Continuous Data----
# list all LF data from wavs and log
LF_wavs <- list.files(renamed_wav_dir, "\\_LF.wav$", full.names = TRUE)

LF_logs <- final_df %>%
  filter(hydrophone_type == "LF")

# segment data based on start times and samples from logs
# create LF directory if it doesn't exist
LF_dir <- file.path(processed_dir, "LF")
if (!dir.exists(LF_dir)) {
  dir.create(LF_dir, recursive = TRUE)
}

for (wav in LF_wavs) {
  log_entries <- LF_logs %>% filter(source_wav == basename(wav))
  if (nrow(log_entries) == 0) next
  wav_data <- tuneR::readWave(wav)
  counter <- 1
  for (i in seq_len(nrow(log_entries))) {
    start_sample <- counter
    end_sample <- start_sample + log_entries$num_samples[i] - 1
    segment <- wav_data[start_sample:end_sample]
    start_time_str <- format(as.POSIXct(log_entries$start_time[i],
                                        format = "%Y-%m-%d %H:%M:%OS"),
                             "%Y%m%d_%H%M%S")
    base_name <- sub("\\.wav$", "", log_entries$source_wav[i])
    filename <- paste0(base_name, "_", start_time_str, ".wav")
    out_path <- file.path(paste0(processed_dir, "LF"), filename)
    writeWave(segment, filename = out_path)
    counter <- end_sample + 1
  }
}

### ----High Frequency Duty Cycled Data----
# list all HF data from wavs and log
HF_wavs <- list.files(renamed_wav_dir, pattern = "\\_HF.wav$", full.names = TRUE)

HF_logs <- final_df %>%
  filter(hydrophone_type == "HF")

# segment data based on start times and samples from logs
# create HF directory if it doesn't exist
HF_dir <- file.path(processed_dir, "HF")
if (!dir.exists(HF_dir)) {
  dir.create(HF_dir, recursive = TRUE)
}

for (wav in HF_wavs) {
  log_entries <- HF_logs %>% filter(source_wav == basename(wav))
  if (nrow(log_entries) == 0) next
  wav_data <- tuneR::readWave(wav)
  counter <- 1
  for (i in seq_len(nrow(log_entries))) {
    start_sample <- counter
    end_sample <- start_sample + log_entries$num_samples[i] - 1
    if (start_sample < 1 || end_sample > length(wav_data@left) || is.na(start_sample) || is.na(end_sample)) next
    segment <- wav_data[start_sample:end_sample]
    start_time_str <- format(as.POSIXct(log_entries$start_time[i], format = "%Y-%m-%d %H:%M:%OS"), "%Y%m%d_%H%M%S")
    base_name <- sub("\\.wav$", "", log_entries$source_wav[i])
    filename <- paste0(base_name, "_", start_time_str, ".wav")
    out_path <- file.path(HF_dir, filename)
    writeWave(segment, filename = out_path)
    counter <- end_sample + 1
  }
}

# Idea from Selene 
# counter = 1;
# for session 1 to 10
# startsample = counter;
# end sample = counter + num_samples -1
# READ (startsample:endsample)
# counter = endsample + 1
# end