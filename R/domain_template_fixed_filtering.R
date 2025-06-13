#' Load and filter neuropsychological test scales
#'
#' This function imports the comprehensive list of neuropsychological test scales
#' from the scales.R file and provides flexible filtering capabilities for
#' selecting appropriate scales for specific domains, tests, or categories.
#'
#' @param test_pattern Character vector or regex pattern to filter scales by test name.
#'   Examples: "CVLT", "NAB", "WAIS", etc. Can be a vector for multiple tests.
#' @param domain_pattern Character vector to filter scales by domain-related keywords.
#'   Examples: "Memory", "Executive", "Attention", etc.
#' @param exclude_pattern Character vector of patterns to exclude from results.
#' @param include_composites Logical. Whether to include composite/index scores.
#'   Default is TRUE.
#' @param include_subtest_scores Logical. Whether to include individual subtest scores.
#'   Default is TRUE.
#' @param custom_scales Character vector of additional custom scales to include.
#' @param return_all Logical. If TRUE, returns all scales without filtering.
#'   Default is FALSE.
#'
#' @return Character vector of filtered scale names
#' @export
#'
#' @examples
#' # Get all memory-related scales
#' memory_scales <- get_neuropsych_scales(
#'   domain_pattern = c("Memory", "Recall", "Recognition")
#' )
#'
#' # Get CVLT scales specifically
#' cvlt_scales <- get_neuropsych_scales(test_pattern = "CVLT")
#'
#' # Get NAB scales excluding certain patterns
#' nab_scales <- get_neuropsych_scales(
#'   test_pattern = "NAB",
#'   exclude_pattern = c("False Alarms", "Errors")
#' )
#'
#' # Get all available scales
#' all_scales <- get_neuropsych_scales(return_all = TRUE)
get_neuropsych_scales <- function(
  test_pattern = NULL,
  domain_pattern = NULL,
  exclude_pattern = NULL,
  include_composites = TRUE,
  include_subtest_scores = TRUE,
  custom_scales = NULL,
  return_all = FALSE
) {
  # Source the scales from scales.R
  if (!exists("scales") || is.null(scales)) {
    # Try to load scales from the scales.R file
    scales_file <- system.file("R", "scales.R", package = "NeurotypR")
    if (file.exists(scales_file)) {
      source(scales_file, local = environment())
    } else {
      # Fallback: try to load from current directory
      if (file.exists("R/scales.R")) {
        source("R/scales.R", local = environment())
      } else {
        stop(
          "Cannot find scales.R file. Please ensure it exists in R/ directory."
        )
      }
    }
  }

  # If return_all is TRUE, return all scales plus any custom scales
  if (return_all) {
    result_scales <- scales
    if (!is.null(custom_scales)) {
      result_scales <- unique(c(result_scales, custom_scales))
    }
    return(result_scales)
  }

  # Start with all scales
  filtered_scales <- scales

  # Filter by test pattern
  if (!is.null(test_pattern)) {
    test_matches <- character(0)
    for (pattern in test_pattern) {
      matches <- grep(
        pattern,
        filtered_scales,
        value = TRUE,
        ignore.case = TRUE
      )
      test_matches <- c(test_matches, matches)
    }
    filtered_scales <- unique(test_matches)
  }

  # Filter by domain pattern
  if (!is.null(domain_pattern)) {
    domain_matches <- character(0)
    for (pattern in domain_pattern) {
      matches <- grep(
        pattern,
        filtered_scales,
        value = TRUE,
        ignore.case = TRUE
      )
      domain_matches <- c(domain_matches, matches)
    }
    if (!is.null(test_pattern)) {
      # If both test and domain patterns specified, take intersection
      filtered_scales <- intersect(filtered_scales, domain_matches)
    } else {
      # If only domain pattern specified, use domain matches
      filtered_scales <- unique(domain_matches)
    }
  }

  # Exclude patterns
  if (!is.null(exclude_pattern)) {
    for (pattern in exclude_pattern) {
      filtered_scales <- grep(
        pattern,
        filtered_scales,
        value = TRUE,
        invert = TRUE,
        ignore.case = TRUE
      )
    }
  }

  # Filter by composite/subtest preferences
  if (!include_composites) {
    # Remove common composite indicators
    composite_patterns <- c("Index", "Composite", "Total", "Overall", "Domain")
    for (pattern in composite_patterns) {
      filtered_scales <- grep(
        pattern,
        filtered_scales,
        value = TRUE,
        invert = TRUE,
        ignore.case = TRUE
      )
    }
  }

  if (!include_subtest_scores) {
    # Keep only scales that appear to be composites or indices
    composite_patterns <- c("Index", "Composite", "Total", "Overall", "Domain")
    composite_matches <- character(0)
    for (pattern in composite_patterns) {
      matches <- grep(
        pattern,
        filtered_scales,
        value = TRUE,
        ignore.case = TRUE
      )
      composite_matches <- c(composite_matches, matches)
    }
    filtered_scales <- unique(composite_matches)
  }

  # Add custom scales
  if (!is.null(custom_scales)) {
    filtered_scales <- unique(c(filtered_scales, custom_scales))
  }

  # Remove duplicates and sort
  filtered_scales <- unique(filtered_scales)
  filtered_scales <- sort(filtered_scales)

  # Return empty character vector if no matches
  if (length(filtered_scales) == 0) {
    warning("No scales matched the specified criteria.")
    return(character(0))
  }

  return(filtered_scales)
}

#' Create a neuropsychological domain-specific Quarto document
#'
#' @param domain Character vector of domain names (e.g., "Attention/Executive")
#' @param pheno Short name for the domain used in filenames (e.g., "executive")
#' @param scales Vector of test scales to include in the domain
#' @param table_filters Optional additional filtering criteria for tables
#' @param exclude_from_plots Vector of scales to exclude from plots
#' @param subdomain_plot_title Caption for subdomain plot
#' @param narrow_plot_title Caption for narrow plot
#' @param output_file Output filename (if NULL, uses pattern "_02-XX_pheno.qmd")
#'
#' @return Invisibly returns TRUE if successful
#' @export
#'
#' @examples
#' # Basic usage
#' create_neuropsych_domain(
#'   domain = "Attention/Executive",
#'   pheno = "executive"
#' )
#'
#' # With custom scales and titles
#' create_neuropsych_domain(
#'   domain = "Memory",
#'   pheno = "memory",
#'   subdomain_plot_title = "Memory function across subdomains"
#' )
create_neuropsych_domain <- function(
  domain,
  pheno,
  scales = NULL,
  table_filters = NULL,
  exclude_from_plots = NULL,
  subdomain_plot_title = "Domain-specific cognitive functions are essential for successful cognitive functioning, enabling us to perform everyday tasks, handle academic challenges, solve problems, manage our emotions, and interact effectively with others and our environment.",
  narrow_plot_title = subdomain_plot_title,
  output_file = NULL
) {
  # Set default output file if not specified
  if (is.null(output_file)) {
    # Find a domain number (assumes standard naming pattern)
    domain_numbers <- c(
      "iq" = "01",
      "academics" = "02",
      "verbal" = "03",
      "spatial" = "04",
      "memory" = "05",
      "executive" = "06",
      "motor" = "07",
      "social" = "08",
      "adhd_child" = "09",
      "adhd_adult" = "09",
      "emotion_child" = "10",
      "emotion_adult" = "10",
      "adaptive" = "11",
      "daily_living" = "12"
    )

    domain_num <- domain_numbers[pheno]
    if (is.na(domain_num)) {
      domain_num <- "XX"
    } # Fallback if domain not found

    output_file <- paste0("_02-", domain_num, "_", pheno, ".qmd")
  }

  # Default scales appropriate for common executive function tests
  if (is.null(scales)) {
    scales <- get_default_scales(pheno)
  }

  # Generate the Quarto document content
  qmd_content <- generate_qmd_content(
    domain = domain,
    pheno = pheno,
    scales = scales,
    table_filters = table_filters,
    exclude_from_plots = exclude_from_plots,
    subdomain_plot_title = subdomain_plot_title,
    narrow_plot_title = narrow_plot_title
  )

  # Write to file
  writeLines(qmd_content, output_file)

  # Generate the text file as well
  text_file <- gsub("\\.qmd$", "_text.qmd", output_file)
  if (!file.exists(text_file)) {
    writeLines("", text_file)
  }

  return(invisible(TRUE))
}

#' Generate the QMD content for a neuropsychological domain
#'
#' @param domain Domain name
#' @param pheno Short name for domain
#' @param scales Vector of test scales
#' @param table_filters Additional filtering criteria
#' @param exclude_from_plots Scales to exclude from plots
#' @param subdomain_plot_title Caption for subdomain plot
#' @param narrow_plot_title Caption for narrow plot
#'
#' @return A character string containing the QMD content
#' @keywords internal
generate_qmd_content <- function(
  domain,
  pheno,
  scales,
  table_filters = NULL,
  exclude_from_plots = NULL,
  subdomain_plot_title,
  narrow_plot_title
) {
  # Create the header section
  header <- paste0("## ", domain, " {#sec-", pheno, "}\n\n")
  include_line <- paste0(
    "{{< include _02-",
    get_domain_number(pheno),
    "_",
    pheno,
    "_text.qmd >}}\n\n"
  )

  # Setup chunk
  setup_chunk <- paste0(
    "```{r}\n",
    "#| label: setup-",
    pheno,
    "\n",
    "#| include: false\n\n",
    "# domain\n",
    "domains <- c(\"",
    paste(domain, collapse = "\", \""),
    "\")\n\n",
    "# Target phenotype\n",
    "pheno <- \"",
    pheno,
    "\"\n",
    "```\n\n"
  )

  # Export chunk
  export_chunk <- paste0(
    "```{r}\n",
    "#| label: export-",
    pheno,
    "\n",
    "#| include: false\n\n",
    "# Read the CSV file into a data frame\n",
    pheno,
    " <- readr::read_csv(\"data/neurocog.csv\")\n\n",
    "# Filter the data frame based on certain conditions\n",
    "# Keep only the rows where 'domain' equals 'domains' and 'z_mean_domain' is not NA\n",
    pheno,
    " <- ",
    pheno,
    " |>\n",
    "  dplyr::filter(domain %in% domains)\n\n",
    "# Select specific columns from the data frame\n",
    pheno,
    " <- ",
    pheno,
    " |>\n",
    "  dplyr::select(\n",
    "    scale,\n",
    "    test,\n",
    "    test_name,\n",
    "    raw_score,\n",
    "    score,\n",
    "    score_type,\n",
    "    ci_95,\n",
    "    percentile,\n",
    "    range,\n",
    "    domain,\n",
    "    subdomain,\n",
    "    narrow,\n",
    "    pass,\n",
    "    verbal,\n",
    "    timed,\n",
    "    result,\n",
    "    z,\n",
    "    z_mean_domain,\n",
    "    z_sd_domain,\n",
    "    z_mean_subdomain,\n",
    "    z_sd_subdomain,\n",
    "    z_mean_narrow,\n",
    "    z_sd_narrow,\n",
    "    z_mean_pass,\n",
    "    z_sd_pass,\n",
    "    z_mean_verbal,\n",
    "    z_sd_verbal,\n",
    "    z_mean_timed,\n",
    "    z_sd_timed\n",
    "  )\n\n",
    "# Write the '",
    pheno,
    "' data frame to a CSV file\n",
    "readr::write_excel_csv(\n",
    "  ",
    pheno,
    ",\n",
    "  here::here(\"data\", paste0(pheno, \".csv\")),\n",
    "  na = \"\",\n",
    "  col_names = TRUE,\n",
    "  append = FALSE\n",
    ")\n",
    "```\n\n"
  )

  # Data chunk with scales
  data_chunk <- paste0(
    "```{r}\n",
    "#| label: data-",
    pheno,
    "\n",
    "#| include: false\n\n",
    "scales <- c(\n  ",
    paste0("\"", scales, "\"", collapse = ",\n  "),
    "\n)\n\n",
    "# Filter the data using the filter_data function from the `NeurotypR` library\n",
    "data_",
    pheno,
    " <- NeurotypR::filter_data(\n",
    "  data = ",
    pheno,
    ",\n",
    "  domain = domains,\n",
    "  scale = scales\n",
    ")\n",
    "```\n\n"
  )

  # Text generation chunk
  text_chunk <- paste0(
    "```{r}\n",
    "#| label: text-",
    pheno,
    "\n",
    "#| cache: true\n",
    "#| include: false\n\n",
    "# Generate the text for the ",
    pheno,
    " domain\n",
    "NeurotypR::cat_neuropsych_results(\n",
    "  data = data_",
    pheno,
    ",\n",
    "  file = \"_02-",
    get_domain_number(pheno),
    "_",
    pheno,
    "_text.qmd\"\n",
    ")\n",
    "```\n\n"
  )

  # Table generation chunk with dynamic groupings
  table_chunk <- paste0(
    "```{r}\n",
    "#| label: qtbl-",
    pheno,
    "\n",
    "#| dev: tikz\n",
    "#| fig-process: pdf2png\n",
    "#| include: false\n\n",
    "# Set the default engine for tikz to \"xetex\"\n",
    "options(tikzDefaultEngine = \"xetex\")\n\n",
    "# more filtering for tables\n",
    eval(parse(
      text = paste0(
        "data_",
        pheno,
        "_tbl <- data_",
        pheno,
        " |>\n",
        "  dplyr::filter(scale %in% c(\"",
        paste(table_filters, collapse = "\", \""),
        "\"))"
      )
    )),
    "# table arguments\n",
    "table_name <- paste0(\"table_\", pheno)\n",
    "vertical_padding <- 0\n",
    "multiline <- TRUE\n\n",
    "# footnotes\n",
    "fn_scaled_score <- gt::md(\n",
    "  \"Scaled score: Mean = 10 [50th\\u2030], SD \u00b1 3 [16th\\u2030, 84th\\u2030]\"\n",
    ")\n\n",
    "fn_standard_score <- gt::md(\n",
    "  \"Standard score: Mean = 100 [50th\\u2030], SD \u00b1 15 [16th\\u2030, 84th\\u2030]\"\n",
    ")\n\n",
    "fn_t_score <- gt::md(\n",
    "  \"T score: Mean = 50 [50th\\u2030], SD \u00b1 10 [16th\\u2030, 84th\\u2030]\"\n",
    ")\n\n",
    "fn_z_score <- gt::md(\n",
    "  \"z-score: Mean = 0 [50th\\u2030], SD \u00b1 1 [16th\\u2030, 84th\\u2030]\"\n",
    ")\n\n",
    "source_note <- gt::md(\n",
    "  \"_T_ score: Mean = 50 [50th\\u2030], SD \u00b1 10 [16th\\u2030, 84th\\u2030]\"\n",
    ")\n\n",
    "# Dynamic groupings based on actual data\n",
    "# Get unique test names and score types that actually exist in the data\n",
    "existing_test_names <- unique(data_",
    pheno,
    "_tbl$test_name)\n",
    "existing_test_names <- existing_test_names[!is.na(existing_test_names)]\n\n",
    "# Get unique score types from the data\n",
    "existing_score_types <- unique(data_",
    pheno,
    "_tbl$score_type)\n",
    "existing_score_types <- existing_score_types[!is.na(existing_score_types)]\n\n",
    "# Dynamically create groupings based on score types and test names in the data\n",
    "# Initialize empty grouping list\n",
    "grp_",
    pheno,
    " <- list()\n\n",
    "# For each score type found in the data, collect the test names\n",
    "for (score_type in existing_score_types) {\n",
    "  test_names_for_score_type <- data_",
    pheno,
    "_tbl |>\n",
    "    dplyr::filter(score_type == !!score_type) |>\n",
    "    dplyr::pull(test_name) |>\n",
    "    unique() |>\n",
    "    sort()\n",
    "  \n",
    "  test_names_for_score_type <- test_names_for_score_type[!is.na(test_names_for_score_type)]\n",
    "  \n",
    "  if (length(test_names_for_score_type) > 0) {\n",
    "    grp_",
    pheno,
    "[[score_type]] <- test_names_for_score_type\n",
    "  }\n",
    "}\n\n",
    "# Only create table if we have data\n",
    "if (nrow(data_",
    pheno,
    "_tbl) > 0) {\n",
    "  # make `gt` table\n",
    "  NeurotypR::tbl_gt2(\n",
    "    data = data_",
    pheno,
    "_tbl,\n",
    "    pheno = pheno,\n",
    "    table_name = table_name,\n",
    "    # source_note = source_note,\n",
    "    fn_scaled_score = if (\"scaled_score\" %in% names(grp_",
    pheno,
    ") && length(grp_",
    pheno,
    "[[\"scaled_score\"]]) > 0) {\n",
    "      fn_scaled_score\n",
    "    } else {\n",
    "      NULL\n",
    "    },\n",
    "    fn_standard_score = if (\"standard_score\" %in% names(grp_",
    pheno,
    ") && length(grp_",
    pheno,
    "[[\"standard_score\"]]) > 0) {\n",
    "      fn_standard_score\n",
    "    } else {\n",
    "      NULL\n",
    "    },\n",
    "    fn_t_score = if (\"t_score\" %in% names(grp_",
    pheno,
    ") && length(grp_",
    pheno,
    "[[\"t_score\"]]) > 0) {\n",
    "      fn_t_score\n",
    "    } else {\n",
    "      NULL\n",
    "    },\n",
    "    grp_scaled_score = if (\"scaled_score\" %in% names(grp_",
    pheno,
    ") && length(grp_",
    pheno,
    "[[\"scaled_score\"]]) > 0) {\n",
    "      grp_",
    pheno,
    "[[\"scaled_score\"]]\n",
    "    } else {\n",
    "      NULL\n",
    "    },\n",
    "    grp_standard_score = if (\"standard_score\" %in% names(grp_",
    pheno,
    ") && length(grp_",
    pheno,
    "[[\"standard_score\"]]) > 0) {\n",
    "      grp_",
    pheno,
    "[[\"standard_score\"]]\n",
    "    } else {\n",
    "      NULL\n",
    "    },\n",
    "    grp_t_score = if (\"t_score\" %in% names(grp_",
    pheno,
    ") && length(grp_",
    pheno,
    "[[\"t_score\"]]) > 0) {\n",
    "      grp_",
    pheno,
    "[[\"t_score\"]]\n",
    "    } else {\n",
    "      NULL\n",
    "    },\n",
    "    dynamic_grp = grp_",
    pheno,
    ",\n",
    "    vertical_padding = vertical_padding,\n",
    "    multiline = multiline\n",
    "  )\n",
    "} else {\n",
    "  # Create empty placeholder if no data\n",
    "  message(paste(\"No\", pheno, \"data available for table creation\"))\n",
    "}\n",
    "```\n\n"
  )

  # Subdomain figure chunk
  subdomain_fig_chunk <- paste0(
    "```{r}\n",
    "#| label: fig-",
    pheno,
    "-subdomain\n",
    "#| include: false\n",
    "#| fig-cap: \"",
    subdomain_plot_title,
    "\"\n\n"
  )

  # Add exclusions if provided
  if (!is.null(exclude_from_plots)) {
    exclusion_list <- paste0(
      "c(\n  \"",
      paste(exclude_from_plots, collapse = "\",\n  \""),
      "\"\n)"
    )
    subdomain_fig_chunk <- paste0(
      subdomain_fig_chunk,
      "exclude_scales <- ",
      exclusion_list,
      "\n\n",
      "data_",
      pheno,
      " <- data_",
      pheno,
      " |>\n",
      "  dplyr::filter(!scale %in% exclude_scales)\n\n"
    )
  }

  # Continue subdomain figure chunk
  subdomain_fig_chunk <- paste0(
    subdomain_fig_chunk,
    "# dotplot arguments\n",
    "filename <- \"fig_",
    pheno,
    "_subdomain.svg\"\n",
    "colors <- NULL\n",
    "return_plot <- TRUE\n\n",
    "# dotplot variables to plot (x, y)\n",
    "x <- data_",
    pheno,
    "$z_mean_subdomain\n",
    "y <- data_",
    pheno,
    "$subdomain\n\n",
    "NeurotypR::dotplot2(\n",
    "  data = data_",
    pheno,
    ",\n",
    "  x = x,\n",
    "  y = y,\n",
    "  colors = colors,\n",
    "  return_plot = return_plot,\n",
    "  filename = filename,\n",
    "  na.rm = TRUE\n",
    ")\n",
    "```\n\n"
  )

  # Narrow figure chunk
  narrow_fig_chunk <- paste0(
    "```{r}\n",
    "#| label: fig-",
    pheno,
    "-narrow\n",
    "#| include: false\n",
    "#| fig-cap: \"",
    narrow_plot_title,
    "\"\n\n",
    "# dotplot arguments\n",
    "filename <- \"fig_",
    pheno,
    "_narrow.svg\"\n",
    "colors <- NULL\n",
    "return_plot <- TRUE\n\n",
    "# dotplot variables to plot (x, y)\n",
    "x <- data_",
    pheno,
    "$z_mean_narrow\n",
    "y <- data_",
    pheno,
    "$narrow\n\n",
    "NeurotypR::dotplot2(\n",
    "  data = data_",
    pheno,
    ",\n",
    "  x = x,\n",
    "  y = y,\n",
    "  colors = colors,\n",
    "  return_plot = return_plot,\n",
    "  filename = filename,\n",
    "  na.rm = TRUE\n",
    ")\n",
    "```\n\n"
  )

  # Typst code for domain layout
  typst_domain_function <- paste0(
    "```{=typst}\n",
    "#let domain(title: none, file_qtbl, file_fig) = {\n",
    "  let font = (font: \"Roboto Slab\", size: 0.7em)\n",
    "  set text(..font)\n",
    "  pad(top: 0.5em)[]\n",
    "    grid(\n",
    "      columns: (50%, 50%),\n",
    "      gutter: 8pt,\n",
    "        figure([#image(file_qtbl)],\n",
    "          caption: figure.caption(position: top, [#title]),\n",
    "          kind: \"qtbl\",\n",
    "          supplement: [Table],\n",
    "          ),\n",
    "        figure([#image(file_fig, width: auto)],\n",
    "          caption: figure.caption(position: bottom, [\n",
    "            ",
    subdomain_plot_title,
    "\n",
    "            ]),\n",
    "          placement: none,\n",
    "          kind: \"image\",\n",
    "          supplement: [Figure],\n",
    "          gap: 0.5em,\n",
    "        ),\n",
    "      )\n",
    "  }\n",
    "```\n\n"
  )

  # Typst code for subdomain figure
  typst_subdomain <- paste0(
    "```{=typst}\n",
    "#let title = \"",
    domain,
    "\"\n",
    "#let file_qtbl = \"table_",
    pheno,
    ".png\"\n",
    "#let file_fig = \"fig_",
    pheno,
    "_subdomain.svg\"\n",
    "#domain(\n",
    "  title: [#title Scores],\n",
    "  file_qtbl,\n",
    "  file_fig\n",
    "  )\n",
    "```\n\n"
  )

  # Typst code for narrow figure
  typst_narrow <- paste0(
    "```{=typst}\n",
    "#let title = \"",
    domain,
    "\"\n",
    "#let file_qtbl = \"table_",
    pheno,
    ".png\"\n",
    "#let file_fig = \"fig_",
    pheno,
    "_narrow.svg\"\n",
    "#domain(\n",
    "  title: [#title Scores],\n",
    "  file_qtbl,\n",
    "  file_fig\n",
    "  )\n",
    "```\n\n"
  )

  # Combine all sections
  qmd_content <- paste0(
    header,
    include_line,
    setup_chunk,
    export_chunk,
    data_chunk,
    text_chunk,
    table_chunk,
    subdomain_fig_chunk,
    narrow_fig_chunk,
    typst_domain_function,
    typst_subdomain,
    typst_narrow
  )

  return(qmd_content)
}

#' Get the domain number for a given domain
#'
#' @param pheno Domain phenotype
#' @return Character string with domain number
#' @keywords internal
get_domain_number <- function(pheno) {
  domain_numbers <- c(
    "iq" = "01",
    "academics" = "02",
    "verbal" = "03",
    "spatial" = "04",
    "memory" = "05",
    "executive" = "06",
    "motor" = "07",
    "social" = "08",
    "adhd_child" = "09",
    "adhd_adult" = "09",
    "emotion_child" = "10",
    "emotion_adult" = "10",
    "adaptive" = "11",
    "daily_living" = "12"
  )

  domain_num <- domain_numbers[pheno]
  if (is.na(domain_num)) {
    domain_num <- "XX"
  }

  return(domain_num)
}

#' Get default scales for a given domain using imported scales from scales.R
#'
#' @param pheno Domain phenotype
#' @return Vector of scale names
#' @keywords internal
get_default_scales <- function(pheno) {
  # Use the get_neuropsych_scales function to dynamically filter scales
  # based on domain-specific patterns from the imported scales.R file

  if (pheno == "memory") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Memory",
        "Recall",
        "Recognition",
        "Learning",
        "CVLT",
        "NAB"
      ),
      test_pattern = c("CVLT", "NAB", "WMS", "RBANS"),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  if (pheno == "executive") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Executive",
        "Attention",
        "Working Memory",
        "Fluency",
        "Switching",
        "Inhibition"
      ),
      test_pattern = c("D-KEFS", "NEPSY", "NAB", "WAIS", "WISC", "TMT", "NIH"),
      exclude_pattern = c("False Alarms")
    ))
  }

  if (pheno == "iq") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Full Scale",
        "General",
        "Total",
        "Index",
        "IQ",
        "Intelligence",
        "Reasoning",
        "Comprehension"
      ),
      test_pattern = c("WAIS", "WISC", "WPPSI", "NAB", "RBANS", "TOPF"),
      include_composites = TRUE,
      include_subtest_scores = FALSE
    ))
  }

  if (pheno == "academics") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Academic",
        "Reading",
        "Math",
        "Writing",
        "Spelling",
        "Comprehension",
        "Fluency"
      ),
      test_pattern = c("WIAT", "KTEA", "WJ", "TOWRE"),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  if (pheno == "verbal") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Verbal",
        "Language",
        "Vocabulary",
        "Comprehension",
        "Expression",
        "Fluency"
      ),
      test_pattern = c("WAIS", "WISC", "NEPSY", "NAB", "CELF"),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  if (pheno == "spatial") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Spatial",
        "Visual",
        "Perceptual",
        "Construction",
        "Drawing",
        "Copy"
      ),
      test_pattern = c("WAIS", "WISC", "NAB", "ROCFT", "Spatial"),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  if (pheno == "motor") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Motor",
        "Pegboard",
        "Finger",
        "Psychomotor",
        "Speed",
        "Dexterity"
      ),
      test_pattern = c("Pegboard", "Finger", "Motor", "NAB"),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  if (pheno == "social") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Social",
        "Emotion",
        "Theory of Mind",
        "Affect",
        "Recognition"
      ),
      test_pattern = c("NEPSY", "Social", "Emotion", "Affect"),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  if (pheno == "adhd_child" || pheno == "adhd_adult") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Attention",
        "Executive",
        "Working Memory",
        "Processing Speed",
        "Hyperactivity"
      ),
      test_pattern = c("ADHD", "Attention", "Conners", "BASC", "CBCL"),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  if (pheno == "emotion_child" || pheno == "emotion_adult") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Emotion",
        "Mood",
        "Anxiety",
        "Depression",
        "Affect",
        "Behavioral"
      ),
      test_pattern = c("BASC", "CBCL", "Beck", "Emotion", "Mood"),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  if (pheno == "adaptive") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Adaptive",
        "Daily Living",
        "Communication",
        "Socialization",
        "Practical"
      ),
      test_pattern = c("Vineland", "ABAS", "Adaptive", "Daily Living"),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  if (pheno == "daily_living") {
    return(get_neuropsych_scales(
      domain_pattern = c(
        "Daily Living",
        "Practical",
        "Independent Living",
        "Self-Care",
        "Functional"
      ),
      test_pattern = c(
        "Daily Living",
        "Practical",
        "Independent",
        "Functional"
      ),
      exclude_pattern = c("False Alarms", "Error")
    ))
  }

  # Default fallback: return all scales if no domain match
  warning(paste(
    "No specific domain patterns found for:",
    pheno,
    ". Returning all available scales."
  ))
  return(get_neuropsych_scales(return_all = TRUE))
}

#' Get domain groupings for a specific domain
#'
#' @param pheno Domain phenotype
#' @return List of groupings for the domain
#' @keywords internal
get_domain_groupings <- function(pheno) {
  if (pheno == "executive") {
    return(
      'list(
  scaled_score = c(
    "Color-Word Interference",
    "NEPSY-2",
    "RBANS",
    "WAIS-5",
    "WAIS-IV",
    "WISC-V",
    "WPPSI-IV"
  ),
  standard_score = c(
    # "NAB Attention Index",
    # "NAB Executive Functions",
    "NAB-S",
    "NAB",
    "RBANS",
    "WAIS-5",
    "WAIS-IV",
    # "WASI-II",
    "WISC-V",
    "WPPSI-IV"
  ),
  t_score = c(
    "NAB-S",
    "NAB",
    "NIH EXAMINER",
    "Trail Making Test"
  )
)'
    )
  }

  # Add cases for other domains as needed
  # ...

  # Default empty groupings
  return(
    "list(
  scaled_score = character(0),
  standard_score = character(0),
  t_score = character(0)
)"
  )
}

#' Run a neuropsych domain through the entire workflow
#'
#' @param domain_info List with domain parameters
#'
#' @return Invisibly returns TRUE
#' @export
run_neuropsych_domain <- function(domain_info) {
  # Create the domain QMD file
  create_neuropsych_domain(
    domain = domain_info$domain,
    pheno = domain_info$pheno,
    scales = domain_info$scales,
    table_filters = domain_info$table_filters,
    exclude_from_plots = domain_info$exclude_from_plots,
    subdomain_plot_title = domain_info$subdomain_plot_title,
    narrow_plot_title = domain_info$narrow_plot_title,
    output_file = domain_info$output_file
  )

  # Render the QMD file to Typst
  quarto::quarto_render(
    input = domain_info$output_file %||%
      paste0(
        "_02-",
        get_domain_number(domain_info$pheno),
        "_",
        domain_info$pheno,
        ".qmd"
      ),
    output_format = "typst"
  )

  return(invisible(TRUE))
}

# Helper for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
