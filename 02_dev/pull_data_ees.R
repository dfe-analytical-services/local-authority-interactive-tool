# This part is static for all publications
ees_url_header <- "https://content.explore-education-statistics.service.gov.uk/api/releases/"

# Unique for each publication (and year of each publication)
ees_url_tail <- "5e1fafd6-6ff8-4025-affe-ec851785f46e/files"

# Join together
ees_url <- paste0(ees_url_header, ees_url_tail)

# When an EES archive is extracted the underlying data is found in the data/ dir
ees_data_dir <- "data"

# Will need to specify the file which contains the data you need
ees_file <- "sen2_mi.csv"

# File path of where to save the archive
unzip_destination <- here::here("temp")

# Download and save the ees archive
download.file(ees_url,
  destfile = paste0(unzip_destination, ".zip"),
  mode = "wb"
)

# Extract archive
unzip(paste0(unzip_destination, ".zip"), overwrite = TRUE, exdir = unzip_destination)

# Read-in selected file from extracted archive
dataframe <- read.csv(here::here(unzip_destination, ees_data_dir, ees_file))
