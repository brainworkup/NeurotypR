# Define the function
#' @title Summary Function
#' @description Reads in a vector of file names and returns a summary of each file
#' @param files A vector of file names
#' @return A list of summaries, one for each file
summary_function <- function(files) {
  summaries <- list()
  for (file in files) {
    summary <- paste(readLines(file), collapse = "\n\n")
    cat("== ", file, " ==\n")
    cat(summary, "\n\n")
    summaries <- c(summaries, list(summary))
  }
  return(summaries)
}

# Example usage
files <- c(
  "inst/templates/_02-01_iq_text.qmd",
  "inst/templates/_02-02_academics_text.qmd",
  "inst/templates/_02-03_verbal_text.qmd",
  "inst/templates/_02-04_spatial_text.qmd",
  "inst/templates/_02-05_memory_text.qmd",
  "inst/templates/_02-06_executive_text.qmd"
  # "inst/templates/_02-07_motor_text.qmd",
  # "inst/templates/_02-09_adhd_child_text_parent.qmd",
  # "inst/templates/_02-09_adhd_child_text_self.qmd",
  # "inst/templates/_02-10_emotion_child_text_parent.qmd"
)

summary <- summary_function(files)
