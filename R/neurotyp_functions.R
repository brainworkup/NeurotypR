#' Prepare and Export Spatial Data
#'
#' This function reads a CSV file, filters the data based on the specified domains and scales,
#' selects relevant columns, and writes the filtered data to a new CSV file.
#'
#' @param domains A character vector of domains to filter the data by.
#' @param pheno A string representing the phenotype to use as the base name for the output CSV file.
#' @param scales A character vector of scales to filter the data by.
#' @param input_file A string representing the name of the input CSV file. Default is "neurocog.csv".
#' @param output_file Optional string to specify a custom output file name. If NULL, the file name is derived from `pheno`.
#'
#' @return A data frame with the filtered data.
#' @export
#'
#' @examples
#' \dontrun{
#' domains <- c("Visual Perception/Construction")
#' pheno <- "spatial"
#' scales <- c("Arrows", "Bicycle Drawing")
#' data_spatial <- prepare_and_export_spatial_data(domains, pheno, scales)
#' }
prepare_and_export_spatial_data <- function(domains, pheno, scales, input_file = system.file("extdata", "neurocog.csv", package = "NeurotypR"), output_file = NULL) {
  # Check for required arguments
  if (missing(domains) || missing(pheno) || missing(scales)) {
    stop("All arguments (domains, pheno, scales) must be provided.")
  }

  # Read the CSV file into a data frame
  df_spatial <- readr::read_csv(input_file)

  # Ensure required columns are present in the data
  required_columns <- c("domain", "scale", "z_mean_domain")
  if (!all(required_columns %in% colnames(df_spatial))) {
    stop("The data does not contain all required columns.")
  }

  # Filter data by domains and scales
  df_spatial <- df_spatial |>
    dplyr::filter(domain %in% domains) |>
    dplyr::filter(scale %in% scales) |>
    dplyr::select(
      test,
      test_name,
      scale,
      raw_score,
      score,
      ci_95,
      percentile,
      range,
      domain,
      subdomain,
      narrow,
      pass,
      verbal,
      timed,
      description,
      result,
      z,
      z_mean_domain,
      z_sd_domain,
      z_mean_subdomain,
      z_sd_subdomain,
      z_mean_narrow,
      z_sd_narrow,
      z_mean_pass,
      z_sd_pass,
      z_mean_verbal,
      z_sd_verbal,
      z_mean_timed,
      z_sd_timed
    )

  # Define the output file name
  output_file <- if (is.null(output_file)) paste0(pheno, ".csv") else output_file

  # Write the filtered data to a CSV file
  readr::write_excel_csv(df_spatial, output_file, na = "", col_names = TRUE, append = FALSE)

  # Return the filtered data
  return(df_spatial)
}
