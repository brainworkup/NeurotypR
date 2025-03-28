#' Compute Cognitive Composites
#'
#' This function computes cognitive composite scores based on weighted averages of subtest scores.
#'
#' @param data A data frame containing columns 'Subtest', 'Score', and 'rpw' (relative process weight)
#' @return A named vector of composite scores
#' @export
#'
#' @examples
#' data <- data.frame(
#'   Subtest = c("Similarities", "Vocabulary", "Information"),
#'   Score = c(14, 14, 11),
#'   rpw = c(0.779333, 0.638047, 0.350256)
#' )
#' compute_composites(data)
compute_composites <- function(data) {
  # Validate input
  if (!all(c("Subtest", "Score", "rpw") %in% colnames(data))) {
    stop("Input data must contain 'Subtest', 'Score', and 'rpw' columns.")
  }

  # Create a new column for weighted scores
  data$Weighted_Score <- data$Score * data$rpw

  # Define composite categories (example based on the provided data structure)
  composites <- list(
    "Executive_Functions" = c("Similarities", "Vocabulary"),
    "Cognitive_Efficiency" = c("Information"),
    "Global_Index" = c("Similarities", "Vocabulary", "Information")
  )

  # Initialize a list to store composite scores
  composite_scores <- list()

  # Compute weighted average for each composite
  for (composite_name in names(composites)) {
    # Subset data for the current composite
    subtests <- composites[[composite_name]]
    subset_data <- data[data$Subtest %in% subtests, ]

    # Calculate composite score as weighted average
    total_weight <- sum(subset_data$rpw, na.rm = TRUE)
    composite_score <- sum(subset_data$Weighted_Score, na.rm = TRUE) / total_weight

    # Store the composite score
    composite_scores[[composite_name]] <- composite_score
  }

  # Return composite scores as a named vector
  return(unlist(composite_scores))
}

# End of function
