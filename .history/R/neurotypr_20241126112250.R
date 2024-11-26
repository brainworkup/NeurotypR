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
                  edit = FALSE)
}
