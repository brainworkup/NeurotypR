# NeurotypR

<!-- badges: start -->
![R-package](https://img.shields.io/badge/R-package-blue.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Development](https://img.shields.io/badge/Development-Active-brightgreen.svg)
<!-- badges: end -->

## Overview

NeurotypR is an R package that provides standardized R Markdown templates and functions for clinical and forensic neuropsychological report writing and data analysis. It streamlines the process of creating comprehensive neuropsychological reports by automating common tasks and providing consistent formatting.

## Features

- **Standardized Templates**: Ready-to-use R Markdown templates for neuropsychological reports
- **Data Processing Tools**: Functions for scoring, normalization, and interpretation of common neuropsychological tests
- **Workflow Management**: Support for the full neuropsychological evaluation workflow including:
  - Neurobehavioral Status Exam (NSE)
  - Neuropsychological Testing (NT)
  - Summary/Interpretation/Report/Feedback (SIRF)
- **Test Integration**: Support for various neuropsychological assessment tools including:
  - WAIS-IV/WAIS-V (Wechsler Adult Intelligence Scale)
  - WISC-V (Wechsler Intelligence Scale for Children)
  - RBANS (Repeatable Battery for the Assessment of Neuropsychological Status)
  - BASC-3 (Behavior Assessment System for Children)
  - CVLT-3 (California Verbal Learning Test)
  - D-KEFS (Delis-Kaplan Executive Function System)
  - And many more
- **Visualization**: Tools for creating standardized plots and tables of neuropsychological test results
- **Data Extraction**: Functions for extracting test data from various formats

## Installation

```r
# Install from GitHub
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("brainworkup/NeurotypR")
```

## Usage

### Creating a new neuropsychological report

```r
library(NeurotypR)

# Create a new report template
neurotypr_report()
```

### Data Processing Workflow

```r
# Import test data
# Example for RBANS test data processing
rbans_data <- read_csv("path/to/rbans_data.csv")
rbans_processed <- process_rbans_data(rbans_data)

# Merge with lookup table
rbans_with_norms <- merge_lookup_table(rbans_processed, "path/to/lookup_table.csv")

# Calculate composite scores
composites <- compute_composites(rbans_with_norms)

# Generate standardized plots
dotplot(composites)
```

## Package Structure

- `R/`: Core functions for data processing, visualization, and report generation
- `inst/rmarkdown/templates/`: R Markdown templates for neuropsychological reports
- `rmd/`: R Markdown files for processing various neuropsychological tests
- `qmd/`: Quarto documents for specific report sections
- `data/`: Example datasets
- `man/`: Documentation files

## Workflow Support

NeurotypR supports the full neuropsychological evaluation workflow:

1. **Neurobehavioral Status Exam (NSE)**
   - Template support for conducting and documenting initial assessments
   - Integration with telehealth platforms (Zoom)

2. **Neuropsychological Testing (NT)**
   - Tools for test administration, scoring, and data organization
   - Support for both in-person and remote testing paradigms

3. **Summary/Interpretation/Report/Feedback (SIRF)**
   - Automated report generation with standardized sections
   - Visualization of test results
   - Integration of diagnostic considerations (DSM-5, ICD-10)

## Dependencies

NeurotypR relies on several R packages:

- **Data Manipulation**: dplyr, tibble, tidyr, purrr
- **Visualization**: ggplot2, gt, highcharter
- **Report Generation**: rmarkdown, kableExtra
- **Utility**: glue, here, lubridate, stringr

## License

This package is licensed under the MIT License - see the LICENSE file for details.

## Citation

If you use NeurotypR in your research, please cite it as:

```
Trampush, J. (2023). NeurotypR: R Markdown Template for Neuropsychological Report Writing. R package version 0.0.0.9002. https://github.com/brainworkup/NeurotypR
```

## Contributing

Contributions to NeurotypR are welcome. Please feel free to submit a pull request or open an issue.

## Contact

Joey Trampush - joey.trampush@brainworkup.org
