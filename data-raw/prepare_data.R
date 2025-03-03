# Script to prepare the cars2_hf dataset

# Create data-raw directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Read the CSV file
cars2_hf <- read.csv("inst/extdata/cars2_hf.csv", stringsAsFactors = FALSE)

# Save as an R dataset in the data/ directory
usethis::use_data(cars2_hf, overwrite = TRUE)
