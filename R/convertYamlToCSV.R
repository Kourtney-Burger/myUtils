# install.packages("yaml")
# install.packages("readr")

library(yaml)
library(readr)

# Load the YAML file (replace with your local file path)
yaml_file <- "C:/Users/kourtney.burger/Documents/Data & Analysis/PyPAM/IOOS SoundCoop Examples/variableAttributes_NRS11.yaml"

# Read the YAML file
attributes <- yaml.load_file(yaml_file)

# Convert the list of attributes to a data frame
attributes_df <- data.frame(Attribute = names(attributes), Value = unlist(attributes), stringsAsFactors = FALSE)

# Specify the CSV file to write to
csv_file <- "C:/Users/kourtney.burger/Documents/Data & Analysis/PyPAM/IOOS SoundCoop Examples/variable_attributes_NRS11.csv"

# Write the data frame to a CSV file
write_csv(attributes_df, csv_file)

cat("Attributes have been written to", csv_file, "\n")




library(yaml)
library(readr)

# Load the YAML file (replace with your local file path)
yaml_file <- "C:/Users/kourtney.burger/Documents/Data & Analysis/PyPAM/IOOS SoundCoop Examples/variableAttributes_NRS11.yaml"

# Read the YAML file
attributes <- yaml.load_file(yaml_file)

# Check the structure of the attributes to identify any nested elements
str(attributes)

# Function to flatten the list and return as a data frame
flatten_to_df <- function(attrs) {
  # Flatten the list into a named vector
  flattened <- unlist(attrs, recursive = TRUE)
  
  # Convert the flattened list into a data frame
  attributes_df <- data.frame(Attribute = names(flattened), Value = flattened, stringsAsFactors = FALSE)
  
  return(attributes_df)
}

# Apply the function to convert to a data frame
attributes_df <- flatten_to_df(attributes)

# Specify the CSV file to write to
csv_file <- "C:/Users/kourtney.burger/Documents/Data & Analysis/PyPAM/IOOS SoundCoop Examples/variable__attributes_NRS11.csv"

# Write the data frame to a CSV file
write_csv(attributes_df, csv_file)

cat("Attributes have been written to", csv_file, "\n")

