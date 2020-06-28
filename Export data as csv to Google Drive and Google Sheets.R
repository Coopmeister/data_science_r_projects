
# Check local directory and assign to "directory" for easy reference
directory <- getwd()


# Import libraries, including "googledrive" to export to Google Drive and Google Sheets
library("tidyverse")

# We want the latest version of the Coronavirus data, so I go to Rami Krisipin's Github and grab the following:
# install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")

library("coronavirus")
library("googledrive")


# Bring in Coronavirus data
data("coronavirus") 

coronavirus <- coronavirus

# Check out fields in the Coronavirusdata
glimpse(coronavirus)

latest_coronavirus_data <- coronavirus %>%
      
      # Confirm latest date data available
      # summarise(max(date)) 
      filter(date == "2020-03-06")

glimpse(latest_coronavirus_data)

# First we'll save to CSV

write.csv(latest_coronavirus_data, "latest_coronavirus_20200306.csv")

# Now we will upload to Google Drive and Google Sheets

drive_upload("latest_coronavirus_20200306.csv", name = "latest_coronavirus_20200306")

# To view the file in Googlesheets, we refer to the "name" above

drive_browse("latest_coronavirus_20200306")

# If we wanted to import the above file at a later time, we can use "drive_get" to get the ID. This may take some time.

corona_20200306 <- drive_get("latest_coronavirus_20200306")
as_id(corona_20200306)

# We actually download the file with "drive_download"

download_google <- drive_download(as_id("1uRV_vWtrT9iGSngrXYKDrOSBhKy1TUfM"), overwrite = TRUE)

# File was stored to working directory.  We need to add the extension of the file to open it in R, using the paste function.

google_file <- download_google$local_path
path <- paste(google_file, ".csv", sep = "")     
google_table_corona <- read.csv(path)
google_table_corona
