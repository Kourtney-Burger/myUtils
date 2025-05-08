#install.packages("ncdf4")  # If not already installed
library(ncdf4)

# open the NetCDF file
nc <- nc_open("Gliders/swfsc gps/stenella-20250414-delayed-raw.nc")

# check variable names
print(nc$var)

# read in the variables
time <- ncvar_get(nc, "time")
lat <- ncvar_get(nc, "latitude") 
lon <- ncvar_get(nc, "longitude")

# close the NetCDF file
nc_close(nc)

# create a data frame
df <- data.frame(
  time = as.vector(time),
  latitude = as.vector(lat),
  longitude = as.vector(lon)
)

# convert times 
df$time_utc <- as.POSIXct(df$time, origin = "1970-01-01", tz = "UTC")
df$time_utc <- format(df$time_utc, "%Y-%m-%dT%H:%M:%S")

# remove NAs
df <- na.omit(df)

# remove original time column
df$time <- NULL

# quick plot to test
#plot(df$longitude, df$latitude)

# Save to CSV
write.csv(df, "Gliders/swfsc gps/stenella-20250414_SimpleGPS.csv", row.names = FALSE)
