#' @title Make Table Using gt Package for Neurocognitive Domains
#' @description Create a table of domain counts using dplyr and gt packages.
#' @importFrom dplyr across mutate select if_else .data
#' @importFrom gt gt cols_label tab_stub_indent tab_header sub_missing tab_options cols_align tab_source_note gtsave tab_style tab_stubhead cell_text cells_stub cells_row_groups md tab_footnote opt_vertical_padding google_font default_fonts
#' @importFrom gtExtras gt_theme_538
#' @importFrom glue glue
#' @param data Data frame containing neuropsychological test results.
#' @param pheno Phenotype name for file naming.
#' @param source_note Source note to be added to the table.
#' @param title Title of the table.
#' @param tab_stubhead Stubhead of the table.
#' @param caption Caption of the table.
#' @param process_md Process markdown, Default = FALSE.
#' @param fn_scaled_score Footnote for scaled score.
#' @param fn_standard_score Footnote for standard score.
#' @param fn_t_score Footnote for t score.
#' @param fn_z_score Footnote for z score.
#' @param fn_raw_score Footnote for raw scores.
#' @param grp_standard_score Groups for standard score.
#' @param grp_t_score Groups for t score.
#' @param grp_scaled_score Groups for scaled score.
#' @param grp_z_score Groups for z score.
#' @param grp_raw_score Groups for raw scores.
#' @param vertical_padding Vertical padding scale.
#' @param multiline Multiline footnotes, Default = TRUE.
#' @param save_files Whether to save PNG and PDF files, Default = TRUE.
#' @param ... Additional arguments to be passed to the function.
#' @return A formatted gt table object.
#' @rdname tbl_gt2
#' @export
tbl_gt2 <-
  function(
      data,
      pheno = NULL,
      source_note = NULL,
      title = NULL,
      tab_stubhead = NULL,
      caption = NULL,
      process_md = FALSE,
      fn_scaled_score = NULL,
      fn_standard_score = NULL,
      fn_t_score = NULL,
      fn_z_score = NULL,
      fn_raw_score = NULL,
      grp_scaled_score = NULL,
      grp_standard_score = NULL,
      grp_t_score = NULL,
      grp_z_score = NULL,
      grp_raw_score = NULL,
      vertical_padding = NULL,
      multiline = TRUE,
      save_files = TRUE,
      ...) {
    # Input validation
    required_cols <- c("test_name", "scale", "score", "percentile", "range")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Define index scales (avoid duplication)
    INDEX_SCALES <- c(
      "Attention Index (ATT)",
      "Executive Functions Index (EXE)",
      "Spatial Index (SPT)",
      "Language Index (LAN)",
      "Memory Index (MEM)",
      "NAB Attention Index",
      "NAB Executive Functions Index",
      "NAB Total Index",
      "NAB Memory Index",
      "NAB Language Index",
      "NAB Spatial Index",
      "Attention Index",
      "RBANS Total Index",
      "Delayed Memory Index",
      "Immediate Memory Index",
      "Language Index",
      "Visuospatial/Constructional Index",
      "Full Scale (FSIQ)",
      "General Ability (GAI)",
      "Verbal Comprehension (VCI)",
      "Processing Speed (PSI)",
      "Perceptual Reasoning (PRI)",
      "Working Memory (WMI)",
      "Cognitive Proficiency (CPI)",
      "Fluid Reasoning (FRI)",
      "Visual Spatial (VSI)",
      "Vocabulary Acquisition (VAI)",
      "Nonverbal (NVI)"
    )

    # Process data more efficiently - avoid NA->0->NA conversion
    processed_data <- data |>
      dplyr::select(
        .data$test_name,
        .data$scale,
        .data$score,
        .data$percentile,
        .data$range
      ) |>
      dplyr::mutate(
        # Convert character columns efficiently
        test_name = as.character(.data$test_name),
        scale = as.character(.data$scale),
        # Handle zeros in numeric columns
        score = dplyr::if_else(.data$score == 0, NA_integer_, .data$score),
        percentile = dplyr::if_else(
          .data$percentile == 0,
          NA_integer_,
          .data$percentile
        )
      )

    # Create base gt table
    table <- processed_data |>
      gt::gt(
        rowname_col = "scale",
        groupname_col = "test_name",
        process_md = process_md,
        caption = caption,
        rownames_to_stub = FALSE,
        id = if (!is.null(pheno)) paste0("table_", pheno) else NULL
      ) |>
      gt::cols_label(
        test_name = gt::md("**Test**"),
        scale = gt::md("**Scale**"),
        score = gt::md("**Score**"),
        percentile = gt::md("**% Rank**"),
        range = gt::md("**Range**")
      ) |>
      gt::tab_header(title = title) |>
      gt::tab_stubhead(label = tab_stubhead) |>
      gt::sub_missing(missing_text = "--") |>
      # Indent non-index rows
      gt::tab_stub_indent(
        rows = !scale %in% INDEX_SCALES,
        indent = 2
      ) |>
      # Bold index rows
      gt::tab_style(
        style = gt::cell_text(
          weight = "bold",
          font = c(
            gt::google_font(name = "Roboto Slab"),
            gt::google_font(name = "IBM Plex Mono"),
            gt::default_fonts()
          )
        ),
        locations = gt::cells_stub(rows = scale %in% INDEX_SCALES)
      ) |>
      gt::cols_align(
        align = "center",
        columns = c("score", "percentile", "range")
      )

    # Add footnotes using helper function
    table <- add_footnotes(
      table,
      list(
        list(fn = fn_scaled_score, grp = grp_scaled_score),
        list(fn = fn_standard_score, grp = grp_standard_score),
        list(fn = fn_t_score, grp = grp_t_score),
        list(fn = fn_z_score, grp = grp_z_score),
        list(fn = fn_raw_score, grp = grp_raw_score)
      )
    )

    # Apply final styling
    table <- table |>
      gt::tab_style(
        style = gt::cell_text(size = "small"),
        locations = gt::cells_source_notes()
      ) |>
      gt::tab_source_note(source_note = source_note) |>
      gtExtras::gt_theme_538() |>
      gt::tab_options(
        row_group.font.weight = "bold",
        footnotes.multiline = multiline,
        footnotes.font.size = "small",
        footnotes.sep = "  "
      ) |>
      gt::opt_vertical_padding(scale = vertical_padding)

    # Conditionally save files
    if (save_files && !is.null(pheno)) {
      gt::gtsave(table, glue::glue("table_{pheno}.png"))
      gt::gtsave(table, glue::glue("table_{pheno}.pdf"))
    }

    return(table)
  }

#' Helper function to add footnotes efficiently
#' @param table A gt table object
#' @param footnote_list List of footnote configurations
#' @return Modified gt table object
#' @keywords internal
add_footnotes <- function(table, footnote_list) {
  for (fn_config in footnote_list) {
    if (!is.null(fn_config$fn) && !is.null(fn_config$grp)) {
      table <- table |>
        gt::tab_footnote(
          footnote = fn_config$fn,
          locations = gt::cells_row_groups(groups = fn_config$grp)
        )
    }
  }
  return(table)
}
