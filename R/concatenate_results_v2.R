# Load necessary libraries
library(readr)
library(dplyr)
library(glue)

# added this to bwu
concatenate_results <- function(df) {
  df$summary <- apply(df, 1, function(row) {
    sw <- ifelse(row["range"] %in% c("Above { range }", "{ range }", "Exceptionally High"), "a relative strength",
      ifelse(row["range"] %in% c("Below { range }", "{ range }", "Exceptionally Low"), "a relative weakness", "an area of typical functioning")
    )
    percentile_as_percentage <- paste0(row["percentile"], "%")
    glue("The patient's {row['scale']} score of {row['score']} ({row['ci_95']}) is classified as {row['range']} and is ranked at the {row['percentile']}th percentile, indicating performance as good as or better than {percentile_as_percentage} of same age peers from the general population. This estimate of {row['description']} is considered {sw}.")
  })
  return(df$summary)
}

# Read the dataset
executive <- vroom::vroom("{{< var patient >}}/executive.csv")

# Sort the dataset by 'percentile' in descending order
executive <- executive %>% arrange(desc(percentile))

# Concatenate the results
executive$summary <- concatenate_results(executive)

# Print the summary
cat(executive$summary, sep = "\n\n")

# Write the summary to a text file
readr::write_lines(executive$summary, "{{< var patient >}}/_02-05_executive_text.txt", sep = "\n\n")


# Read the dataset
verbal <- vroom::vroom("verbal.csv")

# Sort the dataset by 'percentile' in descending order
verbal <- verbal %>% arrange(desc(percentile))

# Concatenate the results
verbal$summary <- concatenate_results(verbal)

# Print the summary
cat(verbal$summary, sep = "\n\n")

# Write the summary to a text file
write_lines(verbal$summary, "_02-03_verbal_text.txt", sep = "\n\n")


# Read the dataset
spatial <- vroom::vroom("spatial.csv")

# Sort the dataset by 'percentile' in descending order
spatial <- spatial %>% arrange(desc(percentile))

# Concatenate the results
spatial$summary <- concatenate_results(spatial)

# Print the summary
cat(spatial$summary, sep = "\n\n")

# Write the summary to a text file
write_lines(spatial$summary, "_02-04_spatial_text.txt", sep = "\n\n")

## Make this summary more succinct and integrative. Merge all separate sections into one paragraph if possible.


# Read the dataset
memory <- vroom::vroom("memory.csv")

# Sort the dataset by 'percentile' in descending order
memory <- memory %>% arrange(desc(percentile))

# Concatenate the results
memory$summary <- concatenate_results(memory)

# Print the summary
cat(memory$summary, sep = "\n\n")

# Write the summary to a text file
write_lines(memory$summary, "_02-06_memory_text.txt", sep = "\n\n")

Make this summary more succinct and integrative. Merge all separate sections into one paragraph if possible.


# Read the dataset
adhd <- vroom::vroom("adhd.csv")

# Sort the dataset by 'percentile' in descending order
adhd <- adhd %>% arrange(desc(percentile))

# Concatenate the results
adhd$summary <- concatenate_results(adhd)

# Print the summary
cat(adhd$summary, sep = "\n\n")

# Write the summary to a text file
write_lines(adhd$summary, "_02-09_adhd_text.txt", sep = "\n\n")

Make this summary more succinct and integrative. Merge all separate sections into one paragraph if possible.
