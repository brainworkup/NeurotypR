# Load the knitr package
library(quarto)

# Predefined list of input files
file_list <- c(
  "_02-01_iq.qmd",
  "_02-02_academics.qmd",
  "_02-03_verbal.qmd",
  "_02-04_spatial.qmd",
  "_02-05_memory.qmd",
  "_02-06_executive.qmd",
  "_02-07_motor.qmd",
  # "_02-08_social.qmd",
  # "_02-09_adhd_adult.qmd",
  # "_02-10_emotion_adult.qmd",
  "_02-09_adhd_child.qmd",
  "_02-10_emotion_child.qmd",
  # "_02-11_adaptive.qmd",
  "_02-12_daily_living.qmd",
  "_03-00_sirf.qmd"
)

# Loop through the list to knit each file
for (file in file_list) {
  cat("Knitting file:", file, "\n")
  quarto::quarto_render(file, output_format = "typst")
}
