# Bucket to transfer to
my_fmc_bucket_name = 'swfsc-1/drifting_recorder/ADRIFT'

# List deployment IDs to copy to bucket
deployment_dir <- "Z:/RECORDINGS/DRIFTERS/ADRIFT/RAW" #path to top level directory
all_deployments <- list.dirs(deployment_dir, full.names = FALSE, recursive = FALSE) #list deployment IDs based on folder names within top level directory
ADRIFT_deployments <- grep("^ADRIFT_\\d{3}", all_deployments, value = TRUE) #remove Opps deployments

# Copy wav files
for (folder in ADRIFT_deployments) {
  LocalDir <- file.path(deployment_dir, folder) #local folder with wav files
  
  wav <- list.files(LocalDir, pattern = "\\.wav$", full.names = TRUE) #list of wav files to copy

  GCP_FolderName <- sub("_CENSOR$", "", folder) # create folder name for GCP bucket, removing CENSOR from folder name

  GCP_path <- file.path(my_fmc_bucket_name, GCP_FolderName) # create path to GCP folder
  
  for (file in wav) {
    string <- paste("gsutil -m cp -r ", file, " ", "gs://", GCP_path, "/", sep="")
    
    message("Uploading: ", file, " -> ", GCP_path)
    
    system(string)
  }
}
