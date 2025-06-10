# install.R - One-time setup script
install_neuropsych_report <- function() {
  # Install required packages
  required_packages <- c(
    "quarto",
    "knitr",
    "tidyverse",
    "gt",
    "here",
    "bwu",
    "NeurotypR"
  )

  # Check and install missing packages
  missing <- required_packages[
    !required_packages %in%
      installed.packages()[, "Package"]
  ]

  if (length(missing) > 0) {
    message("Installing required packages...")
    install.packages(missing)
  }

  # Install the package itself
  if (!"NeurotypR" %in% installed.packages()[, "Package"]) {
    pak::pak("brainworkup/NeurotypR")
  }

  # Verify Quarto installation
  if (!quarto::quarto_version()) {
    stop("Quarto is not installed. Please install from quarto.org")
  }

  message("Installation complete!")
}
