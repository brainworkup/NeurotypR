#' Extract and Process RBANS Data
#'
#' This function processes RBANS (Repeatable Battery for the Assessment of Neuropsychological Status) data
#' from a CSV file exported from Q-interactive. It extracts raw scores, scaled scores, completion times,
#' and composite scores, then combines them into a single dataset with appropriate metadata.
#'
#' @param input_file_path Path to the CSV file containing RBANS data
#' @param test_name_prefix Prefix used in the CSV file for test names (e.g., "RBANS Update Form A ")
#' @param test The test code to use in the output (e.g., "rbans", "rbans_a")
#' @param test_name The full test name to use in the output (e.g., "RBANS", "RBANS Update Form A")
#' @param patient Patient identifier or name
#' @param output_file_path Optional path to save the processed data. If NULL, data is returned but not saved.
#' @importFrom readr read_csv write_excel_csv write_csv locale
#' @import dplyr
#' @import stringr
#'
#' @return A data frame containing the processed RBANS data
#' @export
#'
#' @examples
#' \dontrun{
#' process_rbans_data(
#'   input_file_path = "data/rbans_export.csv",
#'   test_name_prefix = "RBANS Update Form A ",
#'   test = "rbans_a",
#'   test_name = "RBANS Update Form A",
#'   patient = "Patient001",
#'   output_file_path = "data/processed_rbans.csv"
#' )
#' }
process_rbans_data <- function(input_file_path,
                               test_name_prefix,
                               test,
                               test_name,
                               patient,
                               output_file_path = NULL) {
  # Validate input file
  if (!file.exists(input_file_path)) {
    stop("Input file does not exist: ", input_file_path)
  }

  # Function to extract raw scores
  pluck_rbans_raw <- function(input_file_path, test_name_prefix, output_file_path = NULL) {
    df <- readr::read_csv(
      input_file_path,
      col_names = FALSE,
      show_col_types = FALSE,
      locale = readr::locale(encoding = "UTF-16LE")
    )

    # Rename the columns - make sure we have the right number based on the actual data
    if (ncol(df) >= 3) {
      names(df)[1:3] <- c("Subtest", "NA", "Raw score")
      # Remove the second column
      df <- df |> dplyr::select(Subtest, `Raw score`)
    } else {
      # Handle the case where there might be fewer columns
      names(df) <- c("Subtest", "Raw score")[1:ncol(df)]
    }

    # Find the start of the "Raw Score" section - search the entire dataframe
    start_line <- which(df == "RAW SCORES", arr.ind = TRUE)
    if (length(start_line) > 0) {
      start_line <- start_line[1, "row"] + 1 # Take the first occurrence + 1
    } else {
      # Fallback if "RAW SCORES" not found
      start_line <- which(grepl("RAW SCORES", as.matrix(df), ignore.case = TRUE), arr.ind = TRUE)
      if (length(start_line) > 0) {
        start_line <- start_line[1, "row"] + 1
      } else {
        start_line <- 1 # Default to beginning if not found
      }
    }

    # Find the stop of the "Raw Score" section - similar approach
    stop_line <- which(df == "SCALED SCORES", arr.ind = TRUE)
    if (length(stop_line) > 0) {
      stop_line <- stop_line[1, "row"] - 1 # Take the first occurrence - 1
    } else {
      # Fallback if "SCALED SCORES" not found
      stop_line <- which(grepl("SCALED SCORES", as.matrix(df), ignore.case = TRUE), arr.ind = TRUE)
      if (length(stop_line) > 0) {
        stop_line <- stop_line[1, "row"] - 1
      } else {
        stop_line <- nrow(df) # Default to end if not found
      }
    }

    # Read from the "Raw Score" section
    df_raw <- df |>
      dplyr::slice(start_line:stop_line)

    # Keep only rows with the specified prefix in the first column
    df_raw <- df_raw |> dplyr::filter(stringr::str_starts(Subtest, test_name_prefix))

    # Rename columns - using setNames instead of rename_with to avoid the length error
    df_raw <- df_raw |> setNames(c("scale", "raw_score"))

    df_raw$scale <- as.character(df_raw$scale)
    df_raw$raw_score <- as.numeric(df_raw$raw_score)

    # Write to file if output path is provided
    if (!is.null(output_file_path)) {
      readr::write_csv_excel(df_raw, output_file_path)
    }

    return(df_raw)
  }

  # Function to extract scaled scores
  pluck_rbans_score <- function(input_file_path, test_name_prefix, output_file_path = NULL) {
    df <- readr::read_csv(
      input_file_path,
      col_names = FALSE,
      show_col_types = FALSE,
      locale = readr::locale(encoding = "UTF-16LE")
    )

    # Rename the columns - make sure we have the right number based on the actual data
    if (ncol(df) >= 3) {
      names(df)[1:3] <- c("Subtest", "NA", "Scaled score")
      # Remove the second column
      df <- df |> dplyr::select(Subtest, `Scaled score`)
    } else {
      # Handle the case where there might be fewer columns
      names(df) <- c("Subtest", "Scaled score")[1:ncol(df)]
    }

    # Find the start of the "Scaled Score" section - search the entire dataframe
    start_line <- which(df == "SCALED SCORES", arr.ind = TRUE)
    if (length(start_line) > 0) {
      start_line <- start_line[1, "row"] + 1 # Take the first occurrence + 1
    } else {
      # Fallback if "SCALED SCORES" not found
      start_line <- which(grepl("SCALED SCORES", as.matrix(df), ignore.case = TRUE), arr.ind = TRUE)
      if (length(start_line) > 0) {
        start_line <- start_line[1, "row"] + 1
      } else {
        start_line <- 1 # Default to beginning if not found
      }
    }

    # Find the stop of the "Scaled Score" section - similar approach
    stop_line <- which(df == "CONTEXTUAL EVENTS", arr.ind = TRUE)
    if (length(stop_line) > 0) {
      stop_line <- stop_line[1, "row"] - 1 # Take the first occurrence - 1
    } else {
      # Fallback if "CONTEXTUAL EVENTS" not found
      stop_line <- which(grepl("CONTEXTUAL EVENTS", as.matrix(df), ignore.case = TRUE), arr.ind = TRUE)
      if (length(stop_line) > 0) {
        stop_line <- stop_line[1, "row"] - 1
      } else {
        stop_line <- nrow(df) # Default to end if not found
      }
    }

    # Read from the "score" section
    df_score <- df |>
      dplyr::slice(start_line:stop_line)

    # Keep only rows with the specified prefix in the first column
    df_score <- df_score |> dplyr::filter(stringr::str_starts(Subtest, test_name_prefix))

    # Rename columns - using setNames instead of rename_with to avoid the length error
    df_score <- df_score |> setNames(c("scale", "score"))

    df_score$scale <- as.character(df_score$scale)
    df_score$score <- as.numeric(df_score$score)

    # Write to file if output path is provided
    if (!is.null(output_file_path)) {
      readr::write_csv_excel(df_score, output_file_path)
    }

    return(df_score)
  }

  # Function to extract completion times
  pluck_rbans_completion_times <- function(input_file_path, test_name_prefix, output_file_path = NULL) {
    df <- readr::read_csv(
      input_file_path,
      col_names = FALSE,
      show_col_types = FALSE,
      locale = readr::locale(encoding = "UTF-16LE")
    )

    # Rename the columns - make sure we have the right number based on the actual data
    if (ncol(df) >= 3) {
      names(df)[1:3] <- c("Subtest", "NA", "Completion Time (seconds)")
      # Remove the second column
      df <- df |> dplyr::select(Subtest, `Completion Time (seconds)`)
    } else {
      # Handle the case where there might be fewer columns
      names(df) <- c("Subtest", "Completion Time (seconds)")[1:ncol(df)]
    }

    # Find the start of the "Completion Times" section - search the entire dataframe
    start_line <- which(df == "SUBTEST COMPLETION TIMES", arr.ind = TRUE)
    if (length(start_line) > 0) {
      start_line <- start_line[1, "row"] + 1 # Take the first occurrence + 1
    } else {
      # Fallback if "SUBTEST COMPLETION TIMES" not found
      start_line <- which(grepl("SUBTEST COMPLETION TIMES", as.matrix(df), ignore.case = TRUE), arr.ind = TRUE)
      if (length(start_line) > 0) {
        start_line <- start_line[1, "row"] + 1
      } else {
        start_line <- 1 # Default to beginning if not found
      }
    }

    # Find the stop of the section - similar approach
    stop_line <- which(df == "RULES TRIGGERED", arr.ind = TRUE)
    if (length(stop_line) > 0) {
      stop_line <- stop_line[1, "row"] - 1 # Take the first occurrence - 1
    } else {
      # Fallback if "RULES TRIGGERED" not found
      stop_line <- which(grepl("RULES TRIGGERED", as.matrix(df), ignore.case = TRUE), arr.ind = TRUE)
      if (length(stop_line) > 0) {
        stop_line <- stop_line[1, "row"] - 1
      } else {
        stop_line <- nrow(df) # Default to end if not found
      }
    }

    # Read from the "Completion Time" section
    df_times <- df |>
      dplyr::slice(start_line:stop_line)

    # Keep only rows with the specified prefix in the first column
    df_times <- df_times |> dplyr::filter(stringr::str_starts(Subtest, test_name_prefix))

    # Rename columns - using setNames instead of rename_with to avoid the length error
    df_times <- df_times |> setNames(c("scale", "completion_time_seconds"))

    df_times$scale <- as.character(df_times$scale)
    df_times$completion_time_seconds <- as.numeric(df_times$completion_time_seconds)

    # Write to file if output path is provided
    if (!is.null(output_file_path)) {
      readr::write_csv_excel(df_times, output_file_path)
    }

    return(df_times)
  }

  # Function to extract composite scores
  pluck_rbans_composite <- function(input_file_path, test_name_prefix, output_file_path = NULL) {
    df <- readr::read_csv(
      input_file_path,
      col_names = FALSE,
      show_col_types = FALSE,
      locale = readr::locale(encoding = "UTF-16LE")
    )

    # Find the start of the "Composite Score" section with more robust approach
    start_line <- which(df == "Composite Score", arr.ind = TRUE)
    if (length(start_line) > 0) {
      start_line <- start_line[1, "row"] # Take the first occurrence
    } else {
      # Try looking for it in the X1 column specifically
      start_line <- which(df$X1 == "Composite Score")
      if (length(start_line) == 0) {
        # Fallback if "Composite Score" not found
        start_line <- which(grepl("Composite Score", as.matrix(df), ignore.case = TRUE), arr.ind = TRUE)
        if (length(start_line) > 0) {
          start_line <- start_line[1, "row"]
        } else {
          # If still not found, return empty data frame
          warning("Composite Score section not found in the file")
          return(data.frame(
            scale = character(),
            score = numeric(),
            percentile = numeric(),
            ci_95_lower = numeric(),
            ci_95_upper = numeric()
          ))
        }
      }
    }

    # Assuming there's no specific end line, use the end of the file
    stop_line <- nrow(df)

    # Safely extract the relevant section with error handling
    tryCatch(
      {
        df_composite <- df |>
          dplyr::slice((start_line + 1):stop_line) |>
          tidyr::separate(
            X3,
            sep = ",",
            into = c(
              "percentile",
              "ci_90_lo",
              "ci_90_up",
              "ci_95_lower",
              "ci_95_upper"
            ),
            fill = "right" # Handle cases with fewer than expected values
          ) |>
          dplyr::slice(-1) |>
          dplyr::rename(scale = X1, score = X2) |>
          # Filter based on the prefix
          dplyr::filter(stringr::str_starts(scale, test_name_prefix)) |>
          dplyr::select(-c(ci_90_lo, ci_90_up)) |>
          dplyr::mutate(
            scale = as.character(scale),
            score = as.numeric(score),
            percentile = as.numeric(percentile),
            ci_95_lower = as.numeric(ci_95_lower),
            ci_95_upper = as.numeric(ci_95_upper)
          )
      },
      error = function(e) {
        warning("Error processing composite scores: ", e$message)
        return(data.frame(
          scale = character(),
          score = numeric(),
          percentile = numeric(),
          ci_95_lower = numeric(),
          ci_95_upper = numeric()
        ))
      }
    )

    # Write to file if output path is provided
    if (!is.null(output_file_path) && !is.null(df_composite) && nrow(df_composite) > 0) {
      readr::write_csv_excel(df_composite, output_file_path)
    }

    return(df_composite)
  }

  # Extract data components - pass the output_file_path parameter to intermediate files if desired
  rbans_raw <- pluck_rbans_raw(input_file_path, test_name_prefix, output_file_path = NULL)
  rbans_score <- pluck_rbans_score(input_file_path, test_name_prefix, output_file_path = NULL)
  rbans_time <- pluck_rbans_completion_times(input_file_path, test_name_prefix, output_file_path = NULL)
  rbans_composite <- pluck_rbans_composite(input_file_path, test_name_prefix, output_file_path = NULL)

  # Join the data into one dataframe by the test name
  df <- dplyr::left_join(rbans_raw, rbans_score, by = "scale") |>
    dplyr::mutate(percentile = as.numeric(""), range = as.character("")) |>
    dplyr::left_join(rbans_time, by = "scale")

  # Recalculate percentiles based on score
  df <- df |>
    dplyr::mutate(z = ifelse(!is.na(score), (score - 10) / 3, NA)) |>
    dplyr::mutate(percentile = ifelse(is.na(percentile), trunc(stats::pnorm(z) * 100), percentile)) |>
    dplyr::select(-z)

  # Merge with composite scores
  df <- dplyr::bind_rows(df, rbans_composite) |>
    dplyr::relocate(completion_time_seconds, .after = ci_95_upper)

  # Test score ranges
  if (requireNamespace("bwu", quietly = TRUE)) {
    df <- bwu::gpluck_make_score_ranges(table = df, test_type = "npsych_test")
  } else {
    # Fallback if bwu package is not available
    df <- df |>
      dplyr::mutate(
        range = dplyr::case_when(
          percentile >= 98 ~ "Exceptionally High",
          percentile >= 91 ~ "Above Average",
          percentile >= 75 ~ "High Average",
          percentile >= 25 ~ "Average",
          percentile >= 9 ~ "Low Average",
          percentile >= 2 ~ "Below Average",
          TRUE ~ "Exceptionally Low"
        )
      )
  }

  # Remove prefix from scale names
  df <- df |>
    dplyr::mutate(scale = stringr::str_remove(scale, test_name_prefix))

  # Rename specific scales
  scales_to_rename <- c(
    "Digit Span" == "RBANS Digit Span",
    "Coding" == "RBANS Coding",
    "Immediate Memory Index (IMI)" = "Immediate Memory Index", # RBANS add
    "Visuospatial/ Constructional Index (VCI)" = "Visuospatial/Constructional Index",
    "Language Index (LGI)" = "Language Index",
    "Attention Index (ATI)" = "Attention Index",
    "Delayed Memory Index (DRI)" = "Delayed Memory Index",
    "Total Scale (TOT)" = "RBANS Total Index"
  )

  df$scale <- purrr::map_chr(df$scale, ~ ifelse(.x %in% names(scales_to_rename), scales_to_rename[.x], .x))

  # Add additional columns
  df <- add_rbans_metadata(df, test, test_name, patient)

  # Write the combined data to a CSV file if output_file_path is provided
  if (!is.null(output_file_path)) {
    readr::write_csv_excel(df, output_file_path)
  }

  return(df)
}

#' Add RBANS Metadata
#'
#' This function adds domain, subdomain, and other metadata to RBANS test results.
#'
#' @param df A data frame containing RBANS test results
#' @param test The test code (e.g., "rbans", "rbans_a")
#' @param test_name The full test name (e.g., "RBANS", "RBANS Update Form A")
#' @param patient Patient identifier or name
#'
#' @return A data frame with added metadata
#' @keywords internal
add_rbans_metadata <- function(df, test, test_name, patient) {
  # Add basic columns if they don't exist
  if (requireNamespace("bwu", quietly = TRUE)) {
    df <- bwu::gpluck_make_columns(
      data = df,
      test = test,
      test_name = test_name,
      ci_95 = ifelse(!is.na(df$ci_95_lower) & !is.na(df$ci_95_upper),
        paste0(df$ci_95_lower, "-", df$ci_95_upper), ""
      ),
      domain = "",
      subdomain = "",
      narrow = "",
      pass = "",
      verbal = "",
      timed = "",
      test_type = "npsych_test",
      score_type = "",
      description = "",
      result = ""
    )
  } else {
    # Fallback if bwu package is not available
    missing_cols <- setdiff(
      c(
        "test", "test_name", "ci_95", "domain", "subdomain", "narrow",
        "pass", "verbal", "timed", "test_type", "score_type", "description", "result"
      ),
      names(df)
    )

    for (col in missing_cols) {
      df[[col]] <- ""
    }

    df$test <- test
    df$test_name <- test_name
    df$ci_95 <- ifelse(!is.na(df$ci_95_lower) & !is.na(df$ci_95_upper),
      paste0(df$ci_95_lower, "-", df$ci_95_upper), ""
    )
    df$test_type <- "npsych_test"
  }

  # Add domain information
  df <- df |>
    dplyr::mutate(
      domain = dplyr::case_when(
        scale == "RBANS Total Index" ~ "General Cognitive Ability",
        scale == "Immediate Memory Index" ~ "Memory",
        scale == "List Learning" ~ "Memory",
        scale == "Story Memory" ~ "Memory",
        scale == "Visuospatial/Constructional Index" ~ "Visual Perception/Construction",
        scale == "Figure Copy" ~ "Visual Perception/Construction",
        scale == "Line Orientation" ~ "Visual Perception/Construction",
        scale == "Language Index" ~ "Verbal/Language",
        scale == "Picture Naming" ~ "Verbal/Language",
        scale == "Semantic Fluency" ~ "Verbal/Language",
        scale == "Attention Index" ~ "Attention/Executive",
        scale == "RBANS Digit Span" ~ "Attention/Executive",
        scale == "RBANS Coding" ~ "Attention/Executive",
        scale == "Delayed Memory Index" ~ "Memory",
        scale == "List Recall" ~ "Memory",
        scale == "List Recognition" ~ "Memory",
        scale == "Story Recall" ~ "Memory",
        scale == "Figure Recall" ~ "Memory",
        TRUE ~ domain
      )
    )

  # Add subdomain information
  df <- df |>
    dplyr::mutate(
      subdomain = dplyr::case_when(
        scale == "RBANS Total Index" ~ "Neuropsychological Functioning",
        scale == "Immediate Memory Index" ~ "Neuropsychological Functioning",
        scale == "List Learning" ~ "Learning Efficiency",
        scale == "Story Memory" ~ "Learning Efficiency",
        scale == "Visuospatial/Constructional Index" ~ "Neuropsychological Functioning",
        scale == "Figure Copy" ~ "Organization",
        scale == "Line Orientation" ~ "Perception",
        scale == "Language Index" ~ "Neuropsychological Functioning",
        scale == "Picture Naming" ~ "Retrieval",
        scale == "Semantic Fluency" ~ "Fluency",
        scale == "Attention Index" ~ "Neuropsychological Functioning",
        scale == "RBANS Digit Span" ~ "Attention",
        scale == "RBANS Coding" ~ "Processing Speed",
        scale == "Delayed Memory Index" ~ "Neuropsychological Functioning",
        scale == "List Recall" ~ "Delayed Recall",
        scale == "List Recognition" ~ "Recognition Memory",
        scale == "Story Recall" ~ "Delayed Recall",
        scale == "Figure Recall" ~ "Delayed Recall",
        TRUE ~ subdomain
      )
    )

  # Add narrow information
  df <- df |>
    dplyr::mutate(
      narrow = dplyr::case_when(
        scale == "RBANS Total Index" ~ "RBANS Total Index",
        scale == "Immediate Memory Index" ~ "RBANS Memory Index",
        scale == "List Learning" ~ "Word-List Learning",
        scale == "Story Memory" ~ "Story Memory",
        scale == "Visuospatial/Constructional Index" ~ "RBANS Visuospatial/Constructional Index",
        scale == "Figure Copy" ~ "Figure Copy",
        scale == "Line Orientation" ~ "Visual Perception",
        scale == "Language Index" ~ "RBANS Language Index",
        scale == "Picture Naming" ~ "Naming",
        scale == "Semantic Fluency" ~ "Semantic Fluency",
        scale == "Attention Index" ~ "RBANS Attention Index",
        scale == "RBANS Digit Span" ~ "Attention Span",
        scale == "RBANS Coding" ~ "Cognitive Efficiency",
        scale == "Delayed Memory Index" ~ "RBANS Memory Index",
        scale == "List Recall" ~ "Word-List Learning",
        scale == "List Recognition" ~ "Recognition Memory",
        scale == "Story Recall" ~ "Story Memory",
        scale == "Figure Recall" ~ "Visual Memory",
        TRUE ~ narrow
      )
    )

  # Add timed/untimed information
  df <- df |>
    dplyr::mutate(
      timed = dplyr::case_when(
        scale == "RBANS Total Index" ~ "",
        scale == "Immediate Memory Index" ~ "Untimed",
        scale == "List Learning" ~ "Untimed",
        scale == "Story Memory" ~ "Untimed",
        scale == "Visuospatial/Constructional Index" ~ "Untimed",
        scale == "Figure Copy" ~ "Untimed",
        scale == "Line Orientation" ~ "Untimed",
        scale == "Language Index" ~ "",
        scale == "Picture Naming" ~ "Untimed",
        scale == "Semantic Fluency" ~ "Timed",
        scale == "Attention Index" ~ "",
        scale == "RBANS Digit Span" ~ "Untimed",
        scale == "RBANS Coding" ~ "Timed",
        scale == "Delayed Memory Index" ~ "Untimed",
        scale == "List Recall" ~ "Untimed",
        scale == "List Recognition" ~ "Untimed",
        scale == "Story Recall" ~ "Untimed",
        scale == "Figure Recall" ~ "Untimed",
        TRUE ~ timed
      )
    )

  # Add verbal/nonverbal information
  df <- df |>
    dplyr::mutate(
      verbal = dplyr::case_when(
        scale == "RBANS Total Index" ~ "",
        scale == "Immediate Memory Index" ~ "Verbal",
        scale == "List Learning" ~ "Verbal",
        scale == "Story Memory" ~ "Verbal",
        scale == "Visuospatial/Constructional Index" ~ "Nonverbal",
        scale == "Figure Copy" ~ "Nonverbal",
        scale == "Line Orientation" ~ "Nonverbal",
        scale == "Language Index" ~ "Verbal",
        scale == "Picture Naming" ~ "Verbal",
        scale == "Semantic Fluency" ~ "Verbal",
        scale == "Attention Index" ~ "",
        scale == "RBANS Digit Span" ~ "Verbal",
        scale == "RBANS Coding" ~ "Nonverbal",
        scale == "Delayed Memory Index" ~ "",
        scale == "List Recall" ~ "Verbal",
        scale == "List Recognition" ~ "Verbal",
        scale == "Story Recall" ~ "Verbal",
        scale == "Figure Recall" ~ "Nonverbal",
        TRUE ~ verbal
      )
    )

  # Add PASS information
  df <- df |>
    dplyr::mutate(
      pass = dplyr::case_when(
        scale == "RBANS Total Index" ~ "",
        scale == "Immediate Memory Index" ~ "Sequential",
        scale == "List Learning" ~ "Sequential",
        scale == "Story Memory" ~ "Sequential",
        scale == "Visuospatial/Constructional Index" ~ "Simultaneous",
        scale == "Figure Copy" ~ "Simultaneous",
        scale == "Line Orientation" ~ "Simultaneous",
        scale == "Language Index" ~ "Sequential",
        scale == "Picture Naming" ~ "Knowledge",
        scale == "Semantic Fluency" ~ "Sequential",
        scale == "Attention Index" ~ "Attention",
        scale == "RBANS Digit Span" ~ "Attention",
        scale == "RBANS Coding" ~ "Planning",
        scale == "Delayed Memory Index" ~ "",
        scale == "List Recall" ~ "Sequential",
        scale == "List Recognition" ~ "Sequential",
        scale == "Story Recall" ~ "Sequential",
        scale == "Figure Recall" ~ "Simultaneous",
        TRUE ~ as.character(pass)
      )
    )

  # Add score type information
  df <- df |>
    dplyr::mutate(
      score_type = dplyr::case_when(
        scale == "RBANS Total Index" ~ "standard_score",
        scale == "Immediate Memory Index" ~ "standard_score",
        scale == "List Learning" ~ "scaled_score",
        scale == "Story Memory" ~ "scaled_score",
        scale == "Visuospatial/Constructional Index" ~ "standard_score",
        scale == "Figure Copy" ~ "scaled_score",
        scale == "Line Orientation" ~ "percentile",
        scale == "Language Index" ~ "standard_score",
        scale == "Picture Naming" ~ "percentile",
        scale == "Semantic Fluency" ~ "scaled_score",
        scale == "Attention Index" ~ "standard_score",
        scale == "RBANS Digit Span" ~ "scaled_score",
        scale == "RBANS Coding" ~ "scaled_score",
        scale == "Delayed Memory Index" ~ "standard_score",
        scale == "List Recall" ~ "percentile",
        scale == "List Recognition" ~ "percentile",
        scale == "Story Recall" ~ "scaled_score",
        scale == "Figure Recall" ~ "scaled_score",
        TRUE ~ as.character(score_type)
      )
    )

  # Add descriptions
  df <- df |>
    dplyr::mutate(
      description = dplyr::case_when(
        scale == "RBANS Total Index" ~ "composite indicator of general cognitive functioning",
        scale == "Immediate Memory Index" ~ "composite verbal learning of a word list and a logical story",
        scale == "List Learning" ~ "word list learning",
        scale == "Story Memory" ~ "expository story learning",
        scale == "Visuospatial/Constructional Index" ~ "broad visuospatial processing",
        scale == "Figure Copy" ~ "copy of a complex abstract figure",
        scale == "Line Orientation" ~ "basic perception of visual stimuli",
        scale == "Language Index" ~ "general language processing",
        scale == "Picture Naming" ~ "confrontation naming/expressive vocabulary",
        scale == "Semantic Fluency" ~ "semantic word fluency/generativity",
        scale == "Attention Index" ~ "general attentional and executive functioning",
        scale == "RBANS Digit Span" ~ "attention span and auditory attention",
        scale == "RBANS Coding" ~ "speed of information processing",
        scale == "Delayed Memory Index" ~ "long-term recall of verbal information",
        scale == "List Recall" ~ "long-term recall of a word list",
        scale == "List Recognition" ~ "delayed recognition of a word list",
        scale == "Story Recall" ~ "long-term recall of a detailed story",
        scale == "Figure Recall" ~ "long-term recall and reconstruction of a complex abstract figure",
        TRUE ~ as.character(description)
      )
    )

  # Add result text
  df <- df |>
    dplyr::mutate(
      result = glue::glue(
        "{patient}'s score on {scale} ({description}) was {range}."
      )
    )

  return(df)
}
