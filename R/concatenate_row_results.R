#' Generate a Concatenated Summary for Neuropsychological Test Results
#'
#' This function creates a descriptive summary for each row of a data frame based on
#' specific columns (`range`, `scale`, `score`, `ci_95`, `percentile`, and `description`).
#'
#' @param df A data frame containing test results with columns: `range`, `scale`, `score`,
#'   `ci_95`, `percentile`, and `description`.
#' @return A character vector containing the generated summaries for each row.
#' @examples
#' df <- data.frame(
#'   scale = "Executive Functioning",
#'   score = 110,
#'   ci_95 = "105-115",
#'   range = "Above Average",
#'   percentile = 90,
#'   description = "cognitive flexibility"
#' )
#' concatenate_results(df)
concatenate_results <- function(df) {
  df$summary <- apply(df, 1, function(row) {
    # Determine strength or weakness based on the `range` value
    sw <- ifelse(row["range"] %in% c("Above { range }", "{ range }", "Exceptionally High"),
      "a relative strength",
      ifelse(row["range"] %in% c("Below { range }", "{ range }", "Exceptionally Low"),
        "a relative weakness",
        "an area of typical functioning"
      )
    )

    # Percentile as a percentage string
    percentile_as_percentage <- paste0(row["percentile"], "%")

    # Construct the summary
    glue("The patient's {row['scale']} score of {row['score']} ({row['ci_95']}) is classified as {row['range']} and is ranked at the {row['percentile']}th percentile, indicating performance as good as or better than {percentile_as_percentage} of same-age peers from the general population. This estimate of {row['description']} is considered {sw}.")
  })
  return(df$summary)
}

#' Process and Save Test Results
#'
#' Reads, processes, and generates a concatenated summary for test results,
#' and writes the summary to a specified file.
#'
#' @param file_path The path to the CSV file containing test results.
#' @param output_path The path where the summarized text file will be saved.
#' @return NULL. Writes the processed summary to the file.
process_and_save_results <- function(file_path, output_path) {
  # Load the dataset
  df <- vroom::vroom(file_path)

  # Sort the dataset by 'percentile' in descending order
  df <- df |> arrange(desc(percentile))

  # Concatenate the results
  df$summary <- concatenate_results(df)

  # Write the summary to a text file
  readr::write_lines(df$summary, output_path, sep = "\n\n")
}

# Process all datasets and write summaries
# datasets <- list(
#   executive = list(file = "../executive.csv", output = ".._02-05_executive_text.txt"),
#   verbal = list(file = "verbal.csv", output = "_02-03_verbal_text.txt"),
#   spatial = list(file = "spatial.csv", output = "_02-04_spatial_text.txt"),
#   memory = list(file = "memory.csv", output = "_02-06_memory_text.txt"),
#   adhd = list(file = "adhd.csv", output = "_02-09_adhd_text.txt")
# )

# Loop through each dataset and process
# for (name in names(datasets)) {
#   dataset <- datasets[[name]]
#   process_and_save_results(dataset$file, dataset$output)
# }
