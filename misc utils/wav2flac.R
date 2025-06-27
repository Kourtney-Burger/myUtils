# Convert wav files to flac files
# From Shannon

install.packages("seewave")
install.packages("tuneR")

library(tuneR)

input_dir <- "O:/2017_OceanNoiseReferenceStation_Barlow/RECORDINGS"
output_dir <- "C:/tempAcoustics2Transfer"
flac_path <- "C:/Users/shannon.rankin/Documents/flac-1.5.0-win/flac-1.5.0-win/Win64/flac.exe"
if (!dir.exists(output_dir)) dir.create(output_dir)

# List all .wav files recursively
wav_files <- list.files(input_dir, pattern = "\\.[Ww][Aa][Vv]$", full.names = TRUE, recursive = TRUE)

for (wav_file in wav_files) {
  # Create relative path from input_dir
  relative_path <- sub(paste0("^", normalizePath(input_dir, winslash = "/")), "", normalizePath(wav_file, winslash = "/"))
  relative_path <- sub("^/", "", relative_path)  # Remove leading slash if exists
  
  # Replace .wav with .flac
  flac_relative <- sub("\\.[Ww][Aa][Vv]$", ".flac", relative_path)
  
  # Construct full output path
  flac_full_path <- file.path(output_dir, flac_relative)
  flac_dir <- dirname(flac_full_path)
  
  # Create subdirectories if they don't exist
  if (!dir.exists(flac_dir)) dir.create(flac_dir, recursive = TRUE)
  
  # Run the flac command
  cmd <- sprintf('"%s" -f --best "%s" -o "%s"', flac_path, wav_file, flac_full_path)
  system(cmd)
}

  
  
