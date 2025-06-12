# R/config.R
#' Create Report Configuration
#'
#' @param patient_file Path to patient data
#' @param template Report template type
#' @param custom_settings List of custom settings
#'
create_report_config <- function(
    patient_file,
    template = "adult",
    custom_settings = list()) {
  # Load base configuration
  config <- load_template_config(template)

  # Load patient data
  patient <- load_patient_data(patient_file)

  # Merge with custom settings
  config <- modifyList(config, custom_settings)

  # Add patient data to config
  config$patient <- patient

  # Validate configuration
  validate_config(config)

  return(config)
}
