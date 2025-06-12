#' @title Make Enhanced Table Using gt Package for Neurocognitive Domains
#' @description Create a table of domain counts using dplyr and gt packages with advanced visualization features.
#' @importFrom dplyr across mutate group_by summarize arrange select if_else .data filter bind_rows n
#' @importFrom gt gt cols_label tab_stub_indent tab_header sub_missing tab_options cols_align tab_source_note gtsave tab_style tab_stubhead tab_caption tab_spanner cell_text cells_body cells_row_groups md tab_footnote opt_vertical_padding text_transform cols_hide cols_unhide
#' @importFrom gtExtras gt_theme_538 gt_plt_sparkline gt_plt_bullet gt_plt_dist gt_color_box
#' @importFrom tidyr replace_na separate
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom stringr str_detect str_extract str_replace_all
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_text theme_minimal theme element_text element_blank margin labs scale_color_gradient2 scale_y_continuous
#' @importFrom stats sd
#' @param data File path to CSV data or a data frame.
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
#' @param vertical_padding Vertical padding scale value (numeric), Default = 3.
#' @param multiline Multiline footnotes, Default = TRUE.
#' @param index_patterns Vector of regex patterns to identify index rows.
#' @param save_table Whether to save the table as PNG and PDF, Default = TRUE.
#' @param custom_index_rows Optional vector of scale names to be treated as index rows (overrides pattern matching).
#' @param highlight_scores Logical. If TRUE, highlights scores below certain thresholds, Default = FALSE.
#' @param low_score_threshold Score threshold for highlighting low scores, Default = 85 for standard scores.
#' @param very_low_score_threshold Score threshold for highlighting very low scores, Default = 70 for standard scores.
#' @param add_sparklines Logical. If TRUE, adds sparkline distribution graphs, Default = FALSE.
#' @param add_domain_summary Logical. If TRUE, adds domain summary rows, Default = FALSE.
#' @param add_ci_viz Logical. If TRUE, adds confidence interval visualizations, Default = FALSE.
#' @param ci_col Name of column containing confidence interval information, Default = "ci_95".
#' @param domain_col Name of column containing domain information, Default = "domain".
#' @param score_col Name of column containing score information, Default = "score".
#' @param percentile_col Name of column containing percentile information, Default = "percentile".
#' @param ... Additional arguments to be passed to the function.
#' @return A formatted table with domain counts and visualizations.
#' @rdname tbl_gt_enhanced
#' @export
tbl_gt_enhanced <- function(
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
    vertical_padding = 3,
    multiline = TRUE,
    index_patterns = c(
      "Index$",
      "\\(.*\\)$",
      "Scale$",
      "^Full Scale",
      "^General",
      "Composite$",
      "Total$",
      "^Processing",
      "Comprehension",
      "Memory$",
      "Reasoning",
      "Visual Spatial"
    ),
    save_table = TRUE,
    custom_index_rows = NULL,
    highlight_scores = FALSE,
    low_score_threshold = 85,
    very_low_score_threshold = 70,
    add_sparklines = FALSE,
    add_domain_summary = FALSE,
    add_ci_viz = FALSE,
    ci_col = "ci_95",
    domain_col = "domain",
    score_col = "score",
    percentile_col = "percentile",
    ...) {
  # Check if data is a path to a CSV file and import if necessary
  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    data <- readr::read_csv(data)
  }

  # Create data counts - handle case where some columns might be missing
  required_cols <- c("test_name", "scale", score_col, percentile_col, "range")

  # Add additional columns for advanced features
  if (add_domain_summary && domain_col %in% names(data)) {
    required_cols <- c(required_cols, domain_col)
  }
  if (add_ci_viz && ci_col %in% names(data)) {
    required_cols <- c(required_cols, ci_col)
  }

  # Check which columns exist in the data
  cols_to_select <- intersect(required_cols, names(data))

  # Start with base selection
  data_counts <- data |>
    dplyr::select(dplyr::all_of(cols_to_select))

  # Add missing columns if needed
  for (col in setdiff(required_cols, cols_to_select)) {
    data_counts[[col]] <- NA
  }

  # Replace NA values with 0 for score and percentile
  data_counts <- data_counts |>
    dplyr::mutate(across(
      c(score_col, percentile_col),
      ~ tidyr::replace_na(., replace = 0)
    ))

  # Add domain summary if requested
  if (add_domain_summary && domain_col %in% names(data_counts)) {
    # Create domain summaries
    domain_summaries <- data_counts |>
      dplyr::filter(!is.na(.data[[domain_col]])) |>
      dplyr::group_by(.data[[domain_col]]) |>
      dplyr::summarize(
        test_name = "Domain Summary",
        scale = paste0(.data[[domain_col]], " Average"),
        # Calculate mean score, only for non-zero scores
        "{score_col}" := mean(
          .data[[score_col]][.data[[score_col]] > 0],
          na.rm = TRUE
        ),
        "{percentile_col}" := mean(
          .data[[percentile_col]][.data[[percentile_col]] > 0],
          na.rm = TRUE
        ),
        range = ifelse(
          mean(.data[[score_col]][.data[[score_col]] > 0], na.rm = TRUE) <
            low_score_threshold,
          "Low Average",
          "Average"
        ),
        is_summary = TRUE,
        .groups = "drop"
      )

    # Add is_summary column to original data
    data_counts$is_summary <- FALSE

    # Combine with original data
    data_counts <- dplyr::bind_rows(data_counts, domain_summaries)
  } else {
    # Add is_summary column to original data
    data_counts$is_summary <- FALSE
  }

  # Detect index rows automatically or use custom list
  if (!is.null(custom_index_rows)) {
    # Use custom index rows if provided
    is_index_row <- function(scale_value) {
      return(scale_value %in% custom_index_rows)
    }
  } else {
    # Use pattern matching to identify index rows
    index_pattern <- paste(index_patterns, collapse = "|")
    is_index_row <- function(scale_value) {
      stringr::str_detect(scale_value, index_pattern)
    }
  }

  # Add an indicator column for index rows
  data_counts <- data_counts |>
    dplyr::mutate(
      is_index = sapply(.data$scale, is_index_row),
      "{score_col}" := dplyr::if_else(
        .data[[score_col]] == 0,
        NA_real_,
        .data[[score_col]]
      ),
      "{percentile_col}" := dplyr::if_else(
        .data[[percentile_col]] == 0,
        NA_real_,
        .data[[percentile_col]]
      ),
      test_name = as.character(paste0(.data$test_name)),
      scale = as.character(.data$scale)
    )

  # Extract the list of index rows for later use in styling
  index_rows <- data_counts |>
    dplyr::filter(is_index) |>
    dplyr::pull(scale)

  # Process confidence intervals if requested
  if (add_ci_viz && ci_col %in% names(data_counts)) {
    # Parse confidence intervals
    data_counts <- data_counts |>
      dplyr::mutate(
        ci_lower = as.numeric(stringr::str_extract(
          stringr::str_extract(.data[[ci_col]], "^\\d+"),
          "\\d+"
        )),
        ci_upper = as.numeric(stringr::str_extract(
          stringr::str_extract(.data[[ci_col]], "\\d+$"),
          "\\d+"
        ))
      )

    # Create CI plots
    create_ci_plot <- function(score, ci_lower, ci_upper) {
      if (is.na(score) || is.na(ci_lower) || is.na(ci_upper)) {
        return(NULL)
      }

      # Create a mini plot with confidence intervals
      plot_data <- data.frame(
        score = score,
        ci_lower = ci_lower,
        ci_upper = ci_upper
      )

      # Create the plot
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = 1, y = score)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
          width = 0.2
        ) +
        ggplot2::scale_y_continuous(
          limits = c(
            min(40, ci_lower - 10, na.rm = TRUE),
            max(160, ci_upper + 10, na.rm = TRUE)
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          plot.margin = ggplot2::margin(0, 0, 0, 0, "pt")
        )

      return(p)
    }

    # Add the plots to the data
    data_counts$ci_plot <- mapply(
      create_ci_plot,
      data_counts[[score_col]],
      data_counts$ci_lower,
      data_counts$ci_upper,
      SIMPLIFY = FALSE
    )
  }

  # Add 'score_normalized' column for sparklines
  if (add_sparklines) {
    # Function to normalize score based on type
    normalize_score <- function(score, test_name) {
      # Default normalization (standard score)
      mean_val <- 100
      sd_val <- 15

      # Check for T-scores
      if (any(grp_t_score == test_name)) {
        mean_val <- 50
        sd_val <- 10
      } else if (any(grp_scaled_score == test_name)) {
        # Check for scaled scores
        mean_val <- 10
        sd_val <- 3
      } else if (any(grp_z_score == test_name)) {
        # Check for Z-scores
        mean_val <- 0
        sd_val <- 1
      }

      # Calculate z-score
      z_score <- (score - mean_val) / sd_val
      return(z_score)
    }

    # Apply normalization
    data_counts$score_normalized <- mapply(
      normalize_score,
      data_counts[[score_col]],
      data_counts$test_name
    )

    # Create distribution for sparkline
    generate_norm_dist <- function(z_score) {
      if (is.na(z_score)) {
        return(NULL)
      }

      # Generate normal distribution
      x <- seq(-3, 3, length.out = 100)
      y <- dnorm(x)

      # Mark the position of the score
      data <- data.frame(x = x, y = y)

      return(data)
    }

    # Generate distributions
    data_counts$dist_data <- lapply(
      data_counts$score_normalized,
      generate_norm_dist
    )
  }

  # Ensure numeric scores for formatting
  data_counts[[score_col]] <- as.numeric(data_counts[[score_col]])
  data_counts[[percentile_col]] <- as.numeric(data_counts[[percentile_col]])

  # Create additional columns for visualization
  table_data <- data_counts

  # Create table
  table <- table_data |>
    dplyr::select(-is_index) |> # Remove the indicator column before creating the table
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
      "{score_col}" := gt::md("**Score**"),
      "{percentile_col}" := gt::md("**% Rank**"),
      range = gt::md("**Range**")
    ) |>
    # Add alternative row styling for better readability
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#f9f9f9")
      ),
      locations = gt::cells_body(rows = seq(1, nrow(table_data), 2))
    ) |>
    gt::tab_header(title = title) |>
    gt::tab_stubhead(label = tab_stubhead) |>
    gt::sub_missing(missing_text = "--") |>
    # Indent rows except the index rows
    gt::tab_stub_indent(
      rows = !scale %in% c(index_rows) & !is_summary,
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
        rows = scale %in% index_rows
      )
    ) |>
    # Style for domain summary rows
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#e6f3ff"),
        gt::cell_text(weight = "bold", style = "italic")
      ),
      locations = gt::cells_body(
        rows = is_summary
      )
    ) |>
    gt::cols_align(
      align = "center",
      columns = c(score_col, percentile_col, "range")
    ) |>
    # Color code the range column based on common neuropsychological classifications
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#f8d7da"),
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_body(
        columns = "range",
        rows = range %in%
          c("Extremely Low", "Very Low", "Impaired", "Severely Impaired")
      )
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#fff3cd")
      ),
      locations = gt::cells_body(
        columns = "range",
        rows = range %in%
          c("Low Average", "Borderline", "Mildly Impaired", "Below Average")
      )
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#d1e7dd")
      ),
      locations = gt::cells_body(
        columns = "range",
        rows = range %in%
          c(
            "Average",
            "High Average",
            "Superior",
            "Very Superior",
            "Above Average"
          )
      )
    )

  # Add score highlighting if requested
  if (highlight_scores) {
    table <- table |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#fff3cd"), # Light yellow for low scores
          gt::cell_text(weight = "bold")
        ),
        locations = gt::cells_body(
          columns = score_col,
          rows = .data[[score_col]] <= low_score_threshold &
            .data[[score_col]] > very_low_score_threshold
        )
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#f8d7da"), # Light red for very low scores
          gt::cell_text(weight = "bold")
        ),
        locations = gt::cells_body(
          columns = score_col,
          rows = .data[[score_col]] <= very_low_score_threshold
        )
      )
  }

  # Add confidence interval visualization if requested
  if (add_ci_viz && "ci_plot" %in% names(table_data)) {
    # Add a new column for CI visualization
    table <- table |>
      gt::cols_hide(ci_col) |> # Hide the original CI column
      gt::text_transform(
        locations = gt::cells_body(columns = score_col),
        fn = function(x, data) {
          # Get row indices
          row_indices <- seq_len(nrow(data))

          # Process each row
          mapply(
            function(i, score) {
              ci_plot <- table_data$ci_plot[[i]]
              if (is.null(ci_plot)) {
                return(as.character(score))
              } else {
                # Create a combo of score and plot
                plot_html <- gt::ggplot_image(ci_plot, height = 40)
                return(paste0(score, "<br>", plot_html))
              }
            },
            row_indices,
            x,
            SIMPLIFY = TRUE
          )
        }
      )
  }

  # Add sparklines if requested
  if (add_sparklines && "dist_data" %in% names(table_data)) {
    # Add sparkline column
    table <- table |>
      gt::cols_add(
        sparkline = "Distribution"
      ) |>
      gt::cols_label(
        sparkline = gt::md("**Distribution**")
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = "sparkline"),
        fn = function(x, data) {
          # Get row indices
          row_indices <- seq_len(nrow(data))

          # Process each row
          sapply(row_indices, function(i) {
            dist_data <- table_data$dist_data[[i]]
            score <- table_data[[score_col]][i]

            if (is.null(dist_data) || is.na(score)) {
              return("--")
            } else {
              # Create a sparkline
              spark <- gtExtras::gt_plt_dist(
                dist_data,
                x_col = "x",
                y_col = "y",
                spread_type = "Cust",
                palette = c("#e74c3c", "#3498db"),
                fig_dim = c(7, 1)
              )
              return(spark)
            }
          })
        }
      )
  }

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
  if (!is.null(fn_z_score)) {
    table <- table |>
      gt::tab_footnote(
        footnote = fn_z_score,
        locations = gt::cells_row_groups(groups = grp_z_score)
      )
  }
  if (!is.null(fn_raw_score)) {
    table <- table |>
      gt::tab_footnote(
        footnote = fn_raw_score,
        locations = gt::cells_row_groups(groups = grp_raw_score)
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
    )

  # Only add vertical padding if it's not NULL
  if (!is.null(vertical_padding) && is.numeric(vertical_padding)) {
    table <- table |>
      gt::opt_vertical_padding(scale = vertical_padding)
  }

  # Save outputs if requested
  if (save_table && !is.null(pheno)) {
    gt::gtsave(table, glue::glue("table_{pheno}.png"))
    gt::gtsave(table, glue::glue("table_{pheno}.pdf"))
  }

  return(table)
}
