# example function use
# DMON_audioRename("E:/SWFSC Slocum DMON Test April 2025/wav test/WavRenameTest")

# wav rename function
DMON_audioRename <- function(dir) {
  # list .wav2 and .wav3 files 
  wav_list <- list.files(path = dir, pattern = '\\.wav(2|3)?$', full.names = TRUE)
  
  # loop through each file and rename it based on its extension
  for (file in wav_list) {
    if (grepl("\\.wav2$", file, ignore.case = TRUE)) {
      new_fileExt <- sub("\\.wav2$", "_2kHz.wav", file, ignore.case = TRUE)
      file.rename(file, new_fileExt)
    } else if (grepl("\\.wav3$", file, ignore.case = TRUE)) {
      new_fileExt <- sub("\\.wav3$", "_120kHz.wav", file, ignore.case = TRUE)
      file.rename(file, new_fileExt)
    }
  }
}