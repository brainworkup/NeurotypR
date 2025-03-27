#' @title Make Table Using gt Package for Neurocognitive Domains
#' @description Create a table of domain counts using dplyr and gt packages.
#' @importFrom dplyr across mutate group_by summarize arrange select if_else .data
#' @importFrom gt gt cols_label tab_stub_indent tab_header sub_missing tab_options cols_align tab_source_note gtsave tab_style tab_stubhead tab_caption tab_spanner cell_text cells_body cells_row_groups md tab_footnote opt_vertical_padding
#' @importFrom gtExtras gt_theme_538
#' @importFrom tidyr replace_na
#' @importFrom glue glue
#' @param data File or path to data.
#' @param pheno Phenotype name.
#' @param table_name Name of the table to be saved.
#' @param source_note Source note to be added to the table.
#' @param names Names of the columns.
#' @param title Title of the table.
#' @param tab_stubhead Stubhead of the table.
#' @param caption Caption of the table.
#' @param process_md Process markdown.
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
#' @param dynamic_grp Generalized grouping parameter.
#' @param vertical_padding Vertical padding.
#' @param multiline Multiline footnotes, Default = TRUE.
#' @param ... Additional arguments to be passed to the function.
#' @return A formatted table with domain counts.
#' @rdname tbl_gt2
#' @export
tbl_gt2 <-
  function(
    data,
    pheno = NULL,
    table_name = NULL,
    source_note = NULL,
    names = NULL,
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
    dynamic_grp = NULL,
    vertical_padding = NULL,
    multiline = TRUE,
    ...
  ) {
    # Create data counts
    data_counts <- data |>
      dplyr::select(
        .data$test_name,
        .data$scale,
        .data$score,
        .data$percentile,
        .data$range
      ) |>
      dplyr::mutate(across(
        c(.data$score, .data$percentile),
        ~ tidyr::replace_na(., replace = 0)
      ))

    # Create table
    table <- data_counts |>
      dplyr::mutate(
        score = dplyr::if_else(.data$score == 0, NA_integer_, .data$score),
        percentile = dplyr::if_else(
          .data$percentile == 0,
          NA_integer_,
          .data$percentile
        ),
        test_name = as.character(paste0(.data$test_name)),
        scale = as.character(.data$scale)
      ) |>
      gt::gt(
        rowname_col = "scale",
        groupname_col = "test_name",
        process_md = process_md,
        caption = caption,
        rownames_to_stub = FALSE,
        id = paste0("table_", pheno)
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
      # Indent rows except the main index row
      gt::tab_stub_indent(
        rows = !scale %in%
          c(
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
          ),
        indent = 2
      ) |>
      # Bold the index rows in the stub column
      gt::tab_style(
        style = gt::cell_text(
          weight = "bold",
          font = c(
            gt::google_font(name = "Roboto Slab"),
            gt::google_font(name = "IBM Plex Mono"),
            gt::default_fonts()
          )
        ),
        locations = gt::cells_stub(
          rows = scale %in%
            c(
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
        )
      ) |>
      gt::cols_align(
        align = "center",
        columns = c("score", "percentile", "range")
      ) # <-- End of pipe chain

    # Add footnotes individually
    if (!is.null(fn_scaled_score)) {
      table <- table |>
        gt::tab_footnote(
          footnote = fn_scaled_score,
          locations = gt::cells_row_groups(groups = grp_scaled_score)
        )
    }
    if (!is.null(fn_standard_score)) {
      table <- table |>
        gt::tab_footnote(
          footnote = fn_standard_score,
          locations = gt::cells_row_groups(groups = grp_standard_score)
        )
    }
    if (!is.null(fn_t_score)) {
      table <- table |>
        gt::tab_footnote(
          footnote = fn_t_score,
          locations = gt::cells_row_groups(groups = grp_t_score)
        )
    }

    # Adding source note and styling
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
        footnotes.sep = "  " # Adjust spacing between footnotes
      ) |>
      gt::opt_vertical_padding(scale = vertical_padding)

    # Save outputs
    gt::gtsave(table, glue::glue("table_{pheno}.png"))
    gt::gtsave(table, glue::glue("table_{pheno}.pdf"))

    return(table)
  }
