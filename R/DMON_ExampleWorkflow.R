############## Example Workflow GLOBAL VARIABLES ################
log_path <- "E:/risso-20250414/raw data" # Set path to Logs
dep_ID <- "risso-20250414"# Set deployment ID 
wav_path <- log_path

# Create a subfolder for processed data if not done so already
processed_dir <- file.path(log_path, "processed")
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir)
} else {
  message("processed directory exists")
}

output_dir <- file.path(processed_dir, "Segmented_wav")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
} else {
  message("output directory exists")
}

############## RUN FUNCTIONS ################
# PROCESS LOGS
source("R/DMON_LogProcessing.r") # load functions from (link to GitHub once finalized)
log_files <- list.files(log_path, pattern = "\\.log$", full.names = TRUE)
all_duty_data <- do.call(rbind, lapply(log_files, process_log_file))

write.csv(all_duty_data, paste0(processed_dir, "/", dep_ID, "_DutyCycles.csv"), row.names = FALSE)
# may need to format columns in excel with custom format yyyy-mm-dd hh:mm:ss.000


# PROCESS AUDIO FILES
source("R/DMON_WavProcessing.r")  # Load WAV functions

csv_file <- paste0(processed_dir, "/", dep_ID, "_DutyCycles.csv")
DMON_audioRename(wav_path, processed_dir) # rename and move wav files 

process_wav_files(csv_file, processed_dir, output_dir, dep_ID) # rename again and separate into duty cycle chunks (and 1 hour chunks for non duty cycled data)
