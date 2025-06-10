#' Utility Functions for Neuropsychological Reports
#'
#' Collection of utility functions used throughout the package.

#' Check Package Dependencies
#'
#' Checks if required packages are installed and loads them
#'
#' @param packages Character vector of package names
#' @return Logical indicating success
#' @export
check_dependencies <- function(packages = NULL) {
  if (is.null(packages)) {
    packages <- c("quarto", "rmarkdown", "dplyr", "readr", "gt", "ggplot2")
  }

  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]

  if (length(missing_packages) > 0) {
    message("Missing packages: ", paste(missing_packages, collapse = ", "))
    message(
      "Please install with: install.packages(c('",
      paste(missing_packages, collapse = "', '"),
      "'))"
    )
    return(FALSE)
  }

  # Load packages
  sapply(packages, require, character.only = TRUE, quietly = TRUE)
  return(TRUE)
}

#' Get Template Path
#'
#' Helper function to get the full path to a template file
#'
#' @param template_name Name of the template file
#' @param template_type Type of template ('templates', 'assets', etc.)
#' @return Full path to template file
#' @export
get_template_path <- function(template_name, template_type = "templates") {
  template_path <- system.file(
    template_type,
    template_name,
    package = "neuropsychreport"
  )

  if (!file.exists(template_path)) {
    stop("Template not found: ", file.path(template_type, template_name))
  }

  return(template_path)
}

#' Validate Data Structure
#'
#' Validates that the input data has the expected structure for processing
#'
#' @param data Data frame or list to validate
#' @return Logical indicating if data is valid
#' @export
validate_data_structure <- function(data) {
  if (is.null(data)) {
    warning("Data is NULL")
    return(FALSE)
  }

  if (!is.data.frame(data) && !is.list(data)) {
    warning("Data must be a data frame or list")
    return(FALSE)
  }

  if (is.data.frame(data) && nrow(data) == 0) {
    warning("Data frame is empty")
    return(FALSE)
  }

  return(TRUE)
}

#' Create Output Directory
#'
#' Creates output directory structure for reports
#'
#' @param base_dir Base directory for output
#' @param subdirs Additional subdirectories to create
#' @return Path to created directory
#' @export
create_output_dir <- function(base_dir = "output", subdirs = NULL) {
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
  }

  if (!is.null(subdirs)) {
    for (subdir in subdirs) {
      full_path <- file.path(base_dir, subdir)
      if (!dir.exists(full_path)) {
        dir.create(full_path, recursive = TRUE)
      }
    }
  }

  return(base_dir)
}
