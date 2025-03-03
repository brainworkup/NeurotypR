#' Create a new Neurotypical Research Report
#'
#' This function creates a new R Markdown document using the neurotypical research
#' report template.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' neurotypr_report()
#' }
neurotypr_report <- function() {
  rmarkdown::draft("neurotypr-report.Rmd",
    template = "neurotypr",
    package = "NeurotypR",
    edit = FALSE
  )
}



#' Merge Dataset with a Lookup Table
#'
#' This function merges a given dataset with a specified lookup table based on
#' matching columns. It also ensures the resulting data is sorted and includes
#' specified missing columns with default values.
#'
#' @param df A data frame to be merged with the lookup table.
#' @param lookup_file_path A string specifying the path to the lookup table CSV file.
#' @return A data frame that is the result of merging the input dataset and the lookup table.
#' @examples
#' df <- data.frame(
#'   test = c("Test1", "Test2"),
#'   test_name = c("Name1", "Name2"),
#'   scale = c("Scale1", "Scale2")
#' )
#' merge_lookup_table(df, "~/reports/neuropsych_lookup_table_combined.csv")
merge_lookup_table <- function(df, lookup_file_path) {
  # Validate inputs
  if (!file.exists(lookup_file_path)) {
    stop("Lookup file does not exist at the specified path.")
  }
  if (!is.data.frame(df)) {
    stop("The input 'df' must be a data frame.")
  }

  # Load the lookup table
  lookup_table <- tryCatch(
    readr::read_csv(lookup_file_path),
    error = function(e) stop("Error reading lookup table: ", e$message)
  )

  # Merge the data with the lookup table
  df_merged <- df |>
    dplyr::left_join(
      lookup_table,
      by = c("test" = "test", "test_name" = "test_name", "scale" = "scale")
    ) |>
    dplyr::arrange(test, test_name, scale) |> # Sort by test, test_name, and scale
    dplyr::relocate(c(test, test_name), .before = scale) # Relocate columns

  # Add missing columns with default values
  df_final <- gpluck_make_columns(
    df_merged,
    range = "",
    result = "",
    absort = NULL
  )

  return(df_final)
}
