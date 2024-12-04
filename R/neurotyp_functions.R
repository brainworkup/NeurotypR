library(tidyverse)
library(vroom)
library(ggplot2)
library(gt)

# To extract the R code from the provided Quarto `.qmd` document and convert it into a reusable R function, I'll take the relevant code blocks and encapsulate them in functions with appropriate documentation and error checking. Below is the conversion:

### 1. **Prepare and Export Spatial Data**

# This function prepares and exports the spatial data based on the provided domains and scales.


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
#' domains <- c("Visual Perception/Construction")
#' pheno <- "spatial"
#' scales <- c("Arrows", "Bicycle Drawing")
#' data_spatial <- prepare_and_export_spatial_data(domains, pheno, scales)
prepare_and_export_spatial_data <- function(domains, pheno, scales, input_file = c("neurocog.csv", "neurobehav.csv"), output_file = NULL) {

  # Check for required arguments
  if (missing(domains) || missing(pheno) || missing(scales)) {
    stop("All arguments (domains, pheno, scales) must be provided.")
  }

  # Read the CSV file into a data frame
  data <- vroom::vroom(input_file)

  # Ensure required columns are present in the data
  required_columns <- c("domain", "scale", "z_mean_domain")
  if (!all(required_columns %in% colnames(data))) {
    stop("The data does not contain all required columns.")
  }

  # Filter data by domains and scales
  data <- data |>
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
  readr::write_excel_csv(data, output_file, na = "", col_names = TRUE, append = FALSE)

  # Return the filtered data
  return(data)
}

# Define the variables for domains, phenotype, and scales of interest
# domains <- c("")
pheno <- "spatial"
# Define the scales of interest
scales <- c(
  "Arrows",
  "Bicycle Drawing",
  "Block Design No Time Bonus",
  "Block Design Partial Score",
  "Block Design",
  "Clock Drawing",
  "Clocks",
  "Design Construction",
  "Design Copying General",
  "Design Copying Motor",
  "Design Copying Process",
  "Design Copying",
  "Figure Copy",
  "Figure Drawing Copy",
  "Figure Weights",
  "Figure Weights (Double-Time)",
  "Geometric Puzzles",
  "Line Orientation",
  "Map Reading",
  "Matrix Reasoning",
  "NAB Spatial Index",
  "Object Assembly",
  "Picture Concepts",
  "ROCF Copy",
  "ROCFT Copy",
  "Spatial Domain",
  "Visual Discrimination",
  "Visual Puzzles",
  "Visuospatial/Constructional Index",
  "Spatial Index (SPT)",
  "Visual Discrimination",
  "Design Construction",
  "Figure Drawing Copy",
  "Figure Drawing Copy Organization",
  "Figure Drawing Copy Fragmentation",
  "Figure Drawing Copy Planning",
  "Figure Drawing Immediate Recall",
  "Figure Drawing Immediate Recall Organization",
  "Figure Drawing Immediate Recall Fragmentation",
  "Figure Drawing Immediate Recall Planning",
  "Figure Drawing Percent Retention",
  "Map Reading"
)

data <- prepare_and_export_spatial_data(c("Visual Perception/Construction"), "spatial", scales = scales, input_file = "neurocog.csv", output_file = "spatial.csv")

### 2. **Generate Spatial Data Table**

# This function generates a `gt` table for spatial data.

#' Generate Spatial Data Table
#'
#' This function creates a `gt` table for the spatial data using the specified phenotype and grouping.
#'
#' @param data A data frame containing the filtered spatial data.
#' @param pheno A string representing the phenotype.
#' @param table_name A string representing the table name.
#' @param grp_spatial A list of grouping variables for the table.
#' @param vertical_padding An integer for vertical padding in the table. Default is 0.
#' @param multiline A boolean to indicate if multiline should be enabled. Default is TRUE.
#'
#' @return A `gt` object representing the table.
#' @export
#'
#' @examples
#' # Assuming `data_spatial` is the result from `prepare_and_export_spatial_data`
#' table <- generate_spatial_data_table(data_spatial, "spatial", "table_spatial", grp_spatial)
generate_spatial_data_table <- function(data, pheno, table_name, grp_spatial, vertical_padding = 0, multiline = TRUE) {

  # Footnotes and source notes
  fn_scaled_score <- gt::md("Score = Scaled score (Mean = 10 [50th‰], SD ± 3 [16th‰, 84th‰])")
  fn_standard_score <- gt::md("Score = Index score (Mean = 100 [50th‰], SD ± 15 [16th‰, 84th‰])")
  fn_t_score <- gt::md("Score = T score (Mean = 50 [50th‰], SD ± 10 [16th‰, 84th‰])")
  fn_z_score <- gt::md("Score = z-score (Mean = 0 [50th‰], SD ± 1 [16th‰, 84th‰])")
  source_note <- gt::md("Score = _T_ score (Mean = 50 [50th‰], SD ± 10 [16th‰, 84th‰])")

  # Create the gt table
  tbl_gt <- bwu::tbl_gt(
    data = data,
    pheno = pheno,
    table_name = table_name,
    source_note = source_note,
    fn_scaled_score = fn_scaled_score,
    fn_standard_score = fn_standard_score,
    fn_t_score = fn_t_score,
    dynamic_grp = grp_spatial,
    vertical_padding = vertical_padding,
    multiline = multiline
  )

  return(tbl_gt)
}

# Call function
table_name <- "table_spatial"
# grouping
grp_spatial <- list(
  scaled_score = c("WAIS-IV", "WISC-5", "NEPSY-2", "WPPSI-IV", "WISC-V"),
  standard_score = c("NAB", "WPPSI-IV", "NAB-S", "NAB Spatial"),
  t_score = c("NAB", "Rey Complex Figure", "WASI-2", "NAB-S", "NAB Spatial")
)

generate_spatial_data_table(
    data,
    pheno,
    table_name,
    grp_spatial,
    vertical_padding = 0,
    multiline = TRUE
    )


### 3. **Create Spatial Dot Plot**

# This function creates a dot plot for the spatial data.

#' Create Spatial Dot Plot
#'
#' This function creates a dot plot for the spatial data.
#'
#' @param data A data frame containing the filtered spatial data.
#' @param x A vector representing the x-axis values.
#' @param y A vector representing the y-axis values.
#' @param filename A string representing the filename for the output plot.
#' @param colors A vector of colors for the plot. Default is NULL.
#' @param return_plot A boolean indicating whether to return the plot object. Default is TRUE.
#'
#' @return A plot object if `return_plot` is TRUE; otherwise, the plot is saved to `filename`.
#' @export
#'
#' @examples
#' # Assuming `data_spatial` is the result from `prepare_and_export_spatial_data`
#' plot <- create_spatial_dotplot(data_spatial, data_spatial$z_mean_subdomain, data_spatial$subdomain, "fig_spatial.svg")
create_spatial_dotplot <- function(data, x, y, filename, colors = NULL, return_plot = TRUE) {

  # Create the dot plot using bwu::dotplot
  dotplot <- bwu::dotplot(
    data = data,
    x = x,
    y = y,
    colors = colors,
    return_plot = return_plot,
    filename = filename,
    na.rm = TRUE
  )

  return(dotplot)
}

# Arguments
filename <- "fig_spatial.svg"
x <- data$z_mean_subdomain
y <- data$subdomain

# Call function
create_spatial_dotplot(data, x, y, filename)


### 4. **Generate Neuropsychology Results**

# This function generates the neuropsychology results text from the spatial data.

#' Generate Neuropsychology Results
#'
#' This function generates the neuropsychology results text for the spatial data and writes it to a specified file.
#'
#' @param data A data frame containing the filtered spatial data.
#' @param file A string representing the filename to write the results to.
#'
#' @return NULL (invisible).
#' @export
#'
#' @examples
#' # Assuming `data_spatial` is the result from `prepare_and_export_spatial_data`
#' generate_neuropsych_results(data_spatial, "_02-04_spatial_text.qmd")
generate_neuropsych_results <- function(data, file) {

  # Generate the neuropsychology results text and write to file
  bwu::cat_neuropsych_results(data = data, file = file)

  invisible(NULL)
}

# Call function
generate_neuropsych_results(data, "_02-04_spatial_text.qmd")

### Summary
# The extracted R functions above encapsulate the main steps from the `.qmd` file, making the workflow modular and reusable in other contexts. Each function is documented with Roxygen comments and includes error handling where necessary.

# ```typst
# #let domain(title: none, file_qtbl, file_fig) = {
# let font = (font: "Roboto Slab", size: 0.5em)
# set text(..font)
# pad(top: 0.5em)[]
# grid(
#   columns: (50%, 50%),
#   gutter: 8pt,
#   figure([#image(file_qtbl)],
#     caption: figure.caption(position: top, [#title]),
#       kind: "qtbl",
#       supplement: [*Table*],
#       ),
#     figure([#image(file_fig)],
#       caption: figure.caption(position: bottom, [
#         Perception, construction, and visuospatial processing refer to
#         abilities such as mentally visualizing how objects should look from
#         different angles, visualizing how to put objects together so that
#         they fit correctly, and being able to accurately and efficiently
#         copy and/or reproduce visual-spatial information onto paper.
#       ]),
#       placement: none,
#       kind: "image",
#       supplement: [*Figure*],
#       gap: 0.5em,
#       ),
#     )
#   }
# ```
#
# ```{=typst}
# #let title = "Visual Perception/Construction"
# #let file_qtbl = "table_spatial.png"
# #let file_fig = "fig_spatial.svg"
# #domain(
# title: [#title Scores],
#   file_qtbl,
#   file_fig
#     )
# ```

# Save Typst code to a file
typst_code <- '
#let domain(title: none, file_qtbl, file_fig) = {
let font = (font: "Roboto Slab", size: 0.5em)
set text(..font)
pad(top: 0.5em)[]
grid(
  columns: (50%, 50%),
  gutter: 8pt,
  figure([#image(file_qtbl)],
    caption: figure.caption(position: top, [#title]),
    kind: "qtbl",
    supplement: [*Table*],
  ),
  figure([#image(file_fig)],
    caption: figure.caption(position: bottom, [
      Perception, construction, and visuospatial processing refer to
      abilities such as mentally visualizing how objects should look from
      different angles, visualizing how to put objects together so that
      they fit correctly, and being able to accurately and efficiently
      copy and/or reproduce visual-spatial information onto paper.
    ]),
    placement: none,
    kind: "image",
    supplement: [*Figure*],
    gap: 0.5em,
  ),
)
}

#let title = "Visual Perception/Construction"
#let file_qtbl = "table_spatial.png"
#let file_fig = "fig_spatial.svg"
#domain(
title: [#title Scores],
file_qtbl,
file_fig
'

# Write the Typst code to a file
writeLines(typst_code, "document.typ")

# Run the Typst command to compile the file (assuming Typst is installed and in your PATH)
system("typst compile document.typ")
