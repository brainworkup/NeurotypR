#' Generate Neuropsychological Report
#'
#' This function generates a comprehensive neuropsychological report using
#' Quarto templates and domain-specific data processing.
#'
#' @param data_file Path to the neuropsychological data file
#' @param template_name Name of the template to use (default: "report-template")
#' @param output_dir Directory to save the generated report
#' @param output_format Output format ("html", "pdf", "typst")
#' @return Path to the generated report file
#' @export
#'
#' @examples
#' \dontrun{
#' generate_report("data/neurocog.csv", output_dir = "output")
#' }
generate_report <- function(
  data_file,
  template_name = "report-template",
  output_dir = "output",
  output_format = "html"
) {
  # Validate inputs
  if (!file.exists(data_file)) {
    stop("Data file not found: ", data_file)
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Get template path
  template_path <- system.file(
    "templates",
    paste0(template_name, ".qmd"),
    package = "NeurotypR"
  )

  if (!file.exists(template_path)) {
    stop("Template not found: ", template_name)
  }

  # Process the data
  processed_data <- process_domains(data_file)

  # Render the report
  output_file <- file.path(
    output_dir,
    paste0(template_name, ".", output_format)
  )

  quarto::quarto_render(
    input = template_path,
    output_file = output_file,
    execute_params = list(data = processed_data)
  )

  return(output_file)
}
