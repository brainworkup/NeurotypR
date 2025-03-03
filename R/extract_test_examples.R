#' Examples of using the extract_test_data function for various tests
#'
#' This file contains examples for using the extract_test_data function
#' for different neuropsychological tests.

#' @examples
#' # WISC-V subtests example
#' wisc5_subtests <- extract_test_data(
#'   patient = "Biggie",
#'   test = "wisc5",
#'   test_name = "WISC-V",
#'   pages = c(30),
#'   extract_columns = c(2, 4, 5, 6),
#'   variables = c("scale", "raw_score", "score", "percentile"),
#'   score_type = "scaled_score"
#' )
#'
#' # WISC-V composites/indexes example
#' wisc5_composites <- extract_test_data(
#'   patient = "Biggie",
#'   test = "wisc5",
#'   test_name = "WISC-V",
#'   pages = c(31, 34),
#'   extract_columns = c(1, 2, 3, 4, 5, 6),
#'   variables = c("scale", "abbrev", "raw_score", "score", "percentile", "ci_95"),
#'   score_type = "standard_score"
#' )
#'
#' # WISC-V process scores example
#' wisc5_process <- extract_test_data(
#'   patient = "Biggie",
#'   test = "wisc5",
#'   test_name = "WISC-V",
#'   pages = c(37, 38),
#'   extract_columns = c(1, 2, 3, 4),
#'   variables = c("scale", "raw_score", "score", "percentile"),
#'   score_type = "scaled_score"
#' )
#'
#' # WAIS-5 subtests example
#' wais5_subtests <- extract_test_data(
#'   patient = "Biggie",
#'   test = "wais5_subtest",
#'   test_name = "WAIS-V",
#'   pages = c(5),
#'   extract_columns = c(2, 4, 5, 6),
#'   variables = c("scale", "raw_score", "score", "percentile"),
#'   score_type = "scaled_score"
#' )
#'
#' # WAIS-5 composites example
#' wais5_composites <- extract_test_data(
#'   patient = "Biggie",
#'   test = "wais5",
#'   test_name = "WAIS-V",
#'   pages = c(7),
#'   extract_columns = c(1, 2, 3, 4, 5),
#'   variables = c("scale", "abbrev", "raw_score", "score", "percentile", "ci_95"),
#'   score_type = "standard_score"
#' )
#'
#' # WRAT-5 example
#' wrat5_data <- extract_test_data(
#'   patient = "Biggie",
#'   test = "wrat5",
#'   test_name = "WRAT-5",
#'   pages = c(2),
#'   extract_columns = c(1, 2, 3, 4, 5),
#'   variables = c("scale", "raw_score", "score", "ci_95", "percentile"),
#'   score_type = "standard_score"
#' )
#'
#' # WMS-IV example
#' wms4_data <- extract_test_data(
#'   patient = "Biggie",
#'   test = "wms4",
#'   test_name = "WMS-IV",
#'   pages = c(12),
#'   extract_columns = c(1, 3, 4, 5),
#'   variables = c("scale", "raw_score", "score", "percentile"),
#'   score_type = "scaled_score"
#' )
