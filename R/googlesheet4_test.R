# Test script for googlesheets4 r package - https://googlesheets4.tidyverse.org/index.html

# ----Install and Load Package----
# install.packages("googlesheets4") # Comment out this line after initial install
library(googlesheets4)

# ----Authenticate User----
# gs4_auth() # comment out once authenticated
# select Y (it is ok to cache OAuth access credentials) as long as the folder is on your user profile and not in an accessible folder
# follow the prompts and pop up window to complete authentication

# ----Create A New Sheet----
## ----create a sample data frame----
df <- data.frame(
  Month = c("Jan", "Feb", "Mar"),
  Number = c(1, 2, 3)
)

## ----create a new sheet and write data----
sheet <- gs4_create("TestSheet_Months", sheets = list(Data = df)) 

## ----print the URL of the new sheet----
url <- paste0("https://docs.google.com/spreadsheets/d/", sheet)
#url # uncomment to print the url to your console 

# ----Edit existing sheet----
## ----read in sheet----
df_read <- read_sheet(url)

## ----Change cells----
NewData <- data.frame(Month = "April", Number = 4)

range_write(
  ss = url,
  data = NewData,
  range = "A4", # start at A4 and fill horizontally
  col_names = FALSE
)

