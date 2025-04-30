library(readxl)
library(stringr)

dir_path <- "C:/Users/kourtney.burger/Documents/GitHub/TethysSAEL/Detection Worksheets/Humpback and Grays"
files <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)  # This lists all Excel files with .xlsx extension

files_with_Oo <- list()  # Initialize an empty list to store the names of files with "Oo"

for (file in files) {
  data <- read_excel(file)  # Read the Excel file
  # Flatten the data into a character vector
  flattened_data <- as.vector(as.character(data))
  
  # Check if "Oo" is in any cell
  if (any(str_detect(flattened_data, "Oo"))) {
    files_with_Oo <- c(files_with_Oo, file)
  }
}

# Print the files that contain "Oo"
print(files_with_Oo)





dir_path <- "C:/Users/kourtney.burger/Documents/GitHub/TethysSAEL/Detection Worksheets/Humpback and Grays"
files <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)  # List all .xlsx files

files_with_adhoc_detections <- list()  # Initialize an empty list to store files with the "AdhocDetections" sheet

for (file in files) {
  sheet_names <- excel_sheets(file)  # Get the sheet names in the file
  
  # Check if "AdhocDetections" is one of the sheet names
  if ("AdhocDetections" %in% sheet_names) {
    files_with_adhoc_detections <- c(files_with_adhoc_detections, file)
  }
}

# Print the list of files containing the "AdhocDetections" sheet
print(files_with_adhoc_detections)







library(readxl)

# Set the directory containing the Excel files
dir_path <- "C:/Users/kourtney.burger/Documents/GitHub/TethysSAEL/Detection Worksheets/Humpback and Grays"

# List all .xlsx files in the directory
files <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)

# Create a new folder called "Oo Detections" if it doesn't exist
new_folder <- file.path(dir_path, "Oo Detections")
if (!dir.exists(new_folder)) {
  dir.create(new_folder)
}

# Initialize an empty list to store files with the "AdhocDetections" sheet
files_with_adhoc_detections <- list()

# Check each file for the "AdhocDetections" sheet
for (file in files) {
  sheet_names <- excel_sheets(file)  # Get the sheet names in the file
  
  if ("AdhocDetections" %in% sheet_names) {
    # If the sheet exists, add the file to the list
    files_with_adhoc_detections <- c(files_with_adhoc_detections, file)
    
    # Copy the file to the "Oo Detections" folder
    file_copy_path <- file.path(new_folder, basename(file))
    file.copy(file, file_copy_path)
  }
}

# Print the files that were copied
cat("Files copied to 'Oo Detections' folder:\n")
print(files_with_adhoc_detections)





library(readxl)
library(writexl)

# Set the directory containing the "Oo Detections" files
oo_detections_dir <- "C:/Users/kourtney.burger/Documents/GitHub/TethysSAEL/Detection Worksheets/Humpback and Grays/Oo Detections"

# List all .xlsx files in the "Oo Detections" directory
files <- list.files(path = oo_detections_dir, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each file and extract data from the "AdhocDetections" sheet
for (file in files) {
  sheet_names <- excel_sheets(file)  # Get the sheet names in the file
  
  # Check if the "AdhocDetections" sheet exists in the file
  if ("AdhocDetections" %in% sheet_names) {
    # Read the data from the "AdhocDetections" sheet
    data <- read_excel(file, sheet = "AdhocDetections")
    
    # Add a new column with the original file name
    data$SourceFile <- basename(file)
    
    # Combine the data into the 'combined_data' data frame
    combined_data <- rbind(combined_data, data)
  }
}

# Write the combined data to a new Excel file called "Oo Detections.xlsx"
write_xlsx(combined_data, path = file.path(oo_detections_dir, "Oo Detections.xlsx"))

# Print a message confirming the operation
cat("All 'AdhocDetections' data has been combined, with file names included, and saved to 'Oo Detections.xlsx'.\n")
