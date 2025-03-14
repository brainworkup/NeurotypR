#' Sort NAB scales in standard order
#'
#' @description
#' Sorts a data frame containing Neuropsychological Assessment Battery (NAB) scales
#' into the standard hierarchical order: Total Index followed by domain indices with
#' their respective subtests.
#'
#' @param data A data frame containing a column named "scale" with NAB scale names
#'
#' @return A data frame sorted by NAB scale names in standard order
#'
#' @export
#'
#' @examples
#' \dontrun{
#' nab_data <- tibble::tibble(
#'   scale = c("Memory Index (MEM)", "Attention Index (ATT)", "Dots"),
#'   score = c(100, 95, 8)
#' )
#' sort_nab_scales(nab_data)
#' }
sort_nab_scales <- function(data) {
  # Define standard NAB order
  nab_order <- c(
    "Total NAB Index (T-NAB)",
    # Attention domain
    "Attention Index (ATT)",
    "Orientation",
    "Orientation to Self",
    "Orientation to Time",
    "Orientation to Place",
    "Orientation to Situation",
    "Digits Forward",
    "Digits Forward Longest Span",
    "Digits Backward",
    "Digits Backward Longest Span",
    "Dots",
    "Numbers & Letters Part A Speed",
    "Numbers & Letters Part A Errors",
    "Numbers & Letters Part A Efficiency",
    "Numbers & Letters Part B Efficiency",
    "Numbers & Letters Part C Efficiency",
    "Numbers & Letters Part D Efficiency",
    "Numbers & Letters Part D Disruption",
    "Driving Scenes",
    # Language domain
    "Language Index (LAN)",
    "Oral Production",
    "Auditory Comprehension",
    "Auditory Comprehension Colors",
    "Auditory Comprehension Shapes",
    "Auditory Comprehension Colors/Shapes/Numbers",
    "Auditory Comprehension Pointing",
    "Auditory Comprehension Yes/No",
    "Auditory Comprehension Paper Folding",
    "Naming",
    "Naming Semantic Cuing",
    "Naming Phonemic Cuing",
    "Reading Comprehension",
    "Reading Comprehension Words",
    "Reading Comprehension Sentences",
    "Writing",
    "Writing Legibility",
    "Writing Spelling",
    "Writing Syntax",
    "Writing Conveyance",
    "Bill Payment",
    # Memory domain
    "Memory Index (MEM)",
    "List Learning List A Trial 1 Immediate Recall",
    "List Learning List A Trial 2 Immediate Recall",
    "List Learning List A Trial 3 Immediate Recall",
    "List Learning List A Immediate Recall",
    "List Learning List B Immediate Recall",
    "List Learning List A Short Delayed Recall",
    "List Learning List A Long Delayed Recall",
    "List Learning List A Percent Retention",
    "List Learning List A Long Delayed Forced-Choice Recognition",
    "List Learning List A Long Delayed Forced-Choice Recognition False Alarms",
    "List Learning List A Discriminability Index",
    "List Learning List A Recall vs. Recognition Index",
    "List Learning Semantic Clusters",
    "List Learning Perseverations",
    "List Learning Intrusions",
    "Shape Learning Trial 1 Immediate Recognition",
    "Shape Learning Trial 2 Immediate Recognition",
    "Shape Learning Trial 3 Immediate Recognition",
    "Shape Learning Immediate Recognition",
    "Shape Learning Delayed Recognition",
    "Shape Learning Percent Retention",
    "Shape Learning Delayed Forced-Choice Recognition",
    "Shape Learning Delayed Forced-Choice Recognition False Alarms",
    "Shape Learning Discriminability Index",
    "Story Learning Trial 1 Phrase Unit",
    "Story Learning Trial 2 Phrase Unit",
    "Story Learning Phrase Unit Immediate Recall",
    "Story Learning Thematic Unit Immediate Recall",
    "Story Learning Trial 1 Thematic Unit",
    "Story Learning Trial 2 Thematic Unit",
    "Story Learning Phrase Unit Delayed Recall",
    "Story Learning Thematic Unit Delayed Recall",
    "Story Learning Phrase Unit Percent Retention",
    "Daily Living Memory Immediate Recall",
    "Daily Living Memory Delayed Recall",
    "Daily Living Memory Retention",
    "Daily Living Memory Delayed Recognition",
    "Daily Living Memory Recall vs. Recognition",
    "Medication Instructions Immediate Recall",
    "Medication Instructions Delayed Recall",
    "Medication Instructions Delayed Recognition",
    "Name/Address/Phone Immediate Recall",
    "Name/Address/Phone Delayed Recall",
    "Name/Address/Phone Delayed Recognition",
    # Spatial domain
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
    "Map Reading",
    # Executive Functions domain
    "Executive Functions Index (EXE)",
    "Mazes",
    "Judgment",
    "Categories",
    "Word Generation",
    "Word Generation Perseverations"
  )

  # Sort the data frame using the predefined order
  data %>%
    dplyr::mutate(scale = factor(scale, levels = nab_order)) %>%
    dplyr::arrange(scale)
}
