# ----------------------------
# Front End Paths / Decisions
# ----------------------------
Local_dir <- "C:/Users/kourtney.burger/Desktop/test folder"     # Path to Local Directory you want to copy FROM
Cloud_dir <- "swfsc-1"      # Path to Cloud Directory you want to copy TO
offHoursCopy <- FALSE                                 # TRUE = Copy on nights/weekends, FALSE = copy until complete
log_file <- paste0("transfer_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")  # Log file name

# ----------------------------
# Helper Functions
# ----------------------------

# Check if current time is a weekday
is_weekday <- function(x) {
  weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
  return(weekdays(x) %in% weekdays)
}

# Check if current time is outside working hours (before 6 AM or after 6 PM)
is_after_hours <- function(x) {
  hour <- as.integer(format(x, "%H"))
  return(hour < 6 | hour > 18)
}

# Check if current time is OK to transfer based on offHoursCopy setting
is_valid_transfer_time <- function() {
  now <- Sys.time()
  if (!offHoursCopy) return(TRUE)
  return(!is_weekday(now) || is_after_hours(now))
}

# Recursively get all files (with full paths and relative paths)
get_all_files <- function(base_dir) {
  files <- list.files(base_dir, recursive = TRUE, full.names = TRUE)
  rel_paths <- list.files(base_dir, recursive = TRUE, full.names = FALSE)
  return(data.frame(full = files, relative = rel_paths, stringsAsFactors = FALSE))
}

# Check if file exists in GCP bucket
file_exists_in_gcp <- function(gcp_uri) {
  check_cmd <- paste("gsutil ls \"gs://", gcp_uri, "\"", sep = "")
  result <- suppressWarnings(system(check_cmd, intern = TRUE, ignore.stderr = TRUE))
  return(length(result) > 0)
}

# Log a message to file
log_message <- function(msg) {
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

# ----------------------------
# Transfer Function
# ----------------------------

transfer_files <- function(local_base, cloud_base) {
  all_files <- get_all_files(local_base)
  
  for (i in seq_len(nrow(all_files))) {
    while (!is_valid_transfer_time()) {
      log_message(paste("Waiting... Not valid time to transfer. Current time:", Sys.time()))
      Sys.sleep(600)  # wait 10 minutes
    }
    
    local_file <- all_files$full[i]
    relative_path <- all_files$relative[i]
    gcp_path <- file.path(cloud_base, relative_path)
    
    # Check if file already exists in GCP
    if (file_exists_in_gcp(gcp_path)) {
      log_message(paste("SKIPPED (already exists):", relative_path))
      next
    }
    
    # Prepare and execute the copy command
    command <- paste0("gsutil -m cp \"", local_file, "\" \"gs://", gcp_path, "\"")
    log_message(paste("Uploading:", relative_path, "-> gs://", gcp_path))
    
    result <- system(command, intern = TRUE, ignore.stderr = TRUE)
    if (length(result) == 0) {
      log_message(paste("SUCCESS:", relative_path))
    } else {
      log_message(paste("FAILURE:", relative_path, "Error:", paste(result, collapse = " ")))
    }
  }
  
  log_message("All files processed.")
}

# ----------------------------
# Execute Transfer
# ----------------------------

log_message(paste("Transfer started at", Sys.time()))
transfer_files(Local_dir, Cloud_dir)
log_message(paste("Transfer completed at", Sys.time()))
