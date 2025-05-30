% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sort_nab_scales.R
\name{sort_nab_scales}
\alias{sort_nab_scales}
\title{Sort NAB scales in standard order}
\usage{
sort_nab_scales(data)
}
\arguments{
\item{data}{A data frame containing a column named "scale" with NAB scale names}
}
\value{
A data frame sorted by NAB scale names in standard order
}
\description{
Sorts a data frame containing Neuropsychological Assessment Battery (NAB) scales
into the standard hierarchical order: Total Index followed by domain indices with
their respective subtests.
}
\examples{
\dontrun{
nab_data <- tibble::tibble(
  scale = c("Memory Index (MEM)", "Attention Index (ATT)", "Dots"),
  score = c(100, 95, 8)
)
sort_nab_scales(nab_data)
}
}
