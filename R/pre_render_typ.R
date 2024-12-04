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
  "_02-01_iq_text.qmd",
  "_02-02_academics_text.qmd",
  "_02-03_verbal_text.qmd",
  "_02-04_spatial_text.qmd",
  "_02-05_memory_text.qmd",
  "_02-06_executive_text.qmd",
  "_02-07_motor_text.qmd",
  "_02-09_adhd_child_text_parent.qmd",
  "_02-09_adhd_child_text_self.qmd",
  "_02-10_emotion_child_text_parent.qmd"
)

summary <- summary_function(files)
