#EXAMPLES 
DMON_audioRename("E:/your_directory") # Rename using default frequencies

DMON_audioRename("E:/your_directory", LF = "1kHz", HF = "100kHz") # Rename using custom low/high frequency labels



# wav rename function
DMON_audioRename <- function(dir, LF = "2kHz", HF = "120kHz") {
  # list .wav2 and .wav3 files 
  wav_list <- list.files(path = dir, pattern = '\\.wav(2|3)?$', full.names = TRUE)
  
  # loop through each file and rename it based on its extension
  for (file in wav_list) {
    if (grepl("\\.wav2$", file, ignore.case = TRUE)) {
      new_fileExt <- sub("\\.wav2$", paste0("_", LF, ".wav"), file, ignore.case = TRUE)
      file.rename(file, new_fileExt)
    } else if (grepl("\\.wav3$", file, ignore.case = TRUE)) {
      new_fileExt <- sub("\\.wav3$", paste0("_", HF, ".wav"), file, ignore.case = TRUE)
      file.rename(file, new_fileExt)
    }
  }
}