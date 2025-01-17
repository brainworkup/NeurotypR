# Parameters

patient <- "Eric"

# WAIS5 subtests
test <- "wais5"
test_name <- "WAIS-V"
pages <- c(5)
extract_columns <- c(2, 4, 5, 6)
variables <- c("scale", "raw_score", "score", "percentile")
score_type <- "scaled_score"

# WAIS5 composites
test <- "wais5"
test_name <- "WAIS-V"
pages <- c(7)
extract_columns <- c(1, 2, 3, 4, 5)
variables <- c("scale", "abbrev", "raw_score", "score", "percentile", "ci_95")
score_type <- "standard_score"

# WRAT5
test <- "wrat5"
test_name <- "WRAT-5"
pages <- c(2)
extract_columns <- c(1, 2, 3, 4, 5)
score_type <- "standard_score"
variables <- c("scale", "raw_score", "score", "ci_95", "percentile")

# WMS4
test <- "wms4"
test_name <- "WMS-IV"
pages <- c(12)
extract_columns <- c(1, 3, 4, 5)
variables <- c("scale", "raw_score", "score", "percentile")
score_type <- "scaled_score"

# File and path
file <- file.path(file.choose())
qs::qsave(file, paste0(test, "_path.rds"))

# Parameters
params <-
  list(
    patient = patient,
    test = test,
    test_name = test_name,
    file = file,
    pages = pages,
    extract_columns = extract_columns,
    score_type = score_type,
    variables = variables
  )

# Extracted areas
extracted_areas <- tabulapdf::extract_areas(
  file = file,
  pages = pages,
  method = "decide",
  output = "matrix",
  copy = TRUE
)

# Loop over the list and write each matrix to a CSV file
for (i in seq_along(extracted_areas)) {
  write.csv(extracted_areas[[i]], file = paste0(test, "_", i, ".csv"), row.names = FALSE)
}

# Save the entire list to an R data file
save(extracted_areas, file = paste0(test, "_extracted_areas.rds"))

# Check the extracted areas
str(extracted_areas)
