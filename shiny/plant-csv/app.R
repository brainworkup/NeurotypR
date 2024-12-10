# I'll help you convert the RMarkdown code into a Shiny app.
# Here's how we can structure it:

# app.R
library(shiny)
library(here)
library(knitr)
library(magrittr)
library(readr)
library(rmarkdown)
library(snakecase)
library(glue)
library(tibble)
library(tidyr)
library(dplyr)
library(tidytable)
library(purrr)
library(rlang)
library(bwu)

# Helper functions (from your RMarkdown)
scale_description <- function(data, scale, description) {
  # Your existing scale_description function code
}

compute_percentile_range <- function(data, score, score_type, percentile, range_type) {
  # Your existing compute_percentile_range function code
}

glue_result <- function(data, scale, description, range, result) {
  # Your existing glue_result function code
}

# UI
ui <- fluidPage(
  titlePanel("Neurocognitive Test Data Entry"),
  sidebarLayout(
    sidebarPanel(
      textInput("patient", "Patient First Name:", ""),
      selectInput("test_name", "Full Name of Test/Test Battery:",
        choices = c(
          "NIH EXAMINER", "Rey Complex Figure", "Test of Premorbid Functioning",
          "Trail Making Test", "NAB", "ACS Social Cognition"
        )
      ),
      selectInput("test", "Test/Measure File Name:",
        choices = c("examiner", "rocft", "topf", "tmt", "nab", "social_cognition")
      ),
      selectInput("scale", "Scale/Subtest:",
      choices = list(
        "Examiner" = c("Unstructured Task", "Letter Fluency", "Category Fluency"),
        "Rey Complex Figure" = c("ROCFT Copy", "ROCFT Delayed Recall"),
        "Trail Making Test" = c("TMT, Part A", "TMT, Part B"),
        "ACS" = c("TOPF Standard Score", "ACS Word Choice", "Affect Naming"),
        "NAB" = c("Mazes", "Judgment", "Categories", "Word Generation", "Dots",
                 "Numbers & Letters Part A Efficiency", "Numbers & Letters Part B Efficiency", "Numbers & Letters Part C Efficiency", "Numbers & Letters Part D Efficiency", "Driving Scenes"),
        "NEPSY-2" = c("Comprehension of Instructions", "Visuomotor Precision", "Word Generation-Semantic", "Word Generation-Initial Letter", "Semantic vs. Initial Letter", "Narrative Memory Free and Cued Recall",
                     "Narrative Memory Free Recall", "Narrative Memory Recognition", "List Memory"),
        "DKEFS" = c("D-KEFS Color Naming", "D-KEFS Word Reading", "D-KEFS Inhibition",
                   "D-KEFS Switching", "D-KEFS Inhibition Total Errors", "D-KEFS Switching Total Errors"),
        "WMS-IV" = c("Symbol Span", "Spatial Addition"),
        "Grooved Pegboard" = c("Dominant Hand Time", "Nondominant Hand Time"),
        "KTEA-3" = c("Decoding Fluency", "Nonsense Word Decoding", "Reading Comprehension"),
        "Cognitive Estimation" = "Deviation Score",
        "Ravens 2" = "Raven's 2 Index Score"
      ),
      numericInput("raw_score", "Raw score:", 10),
      numericInput("score", "Standardized score:", 50),
      radioButtons("score_type", "Type of Test Score:",
        choices = c("z_score", "scaled_score", "t_score", "standard_score")
      ),
      selectInput("domain", "Domain:",
        choices = c(
          "General Cognitive Ability", "Intelligence/General Ability",
          "Academic Skills", "Verbal/Language"
        )
      ),
      selectInput("subdomain", "Subdomain:",
        choices = c("Select Subdomain")
      ),
      selectInput("narrow", "Narrow Subdomain:",
        choices = c("Select Narrow Subdomain")
      ),
      radioButtons("pass", "PASS:",
        choices = c("Planning", "Attention", "Sequential", "Simultaneous")
      ),
      radioButtons("verbal", "Verbal or Nonverbal Test:",
        choices = c("Verbal", "Nonverbal")
      ),
      radioButtons("timed", "Timed or Untimed Test:",
        choices = c("Timed", "Untimed")
      ),
      numericInput("mean", "Mean:", 50),
      numericInput("stdev", "Standard Deviation:", 10),
      sliderInput("reliability", "Reliability:",
        min = 0, max = 1, value = 0.81, step = 0.01
      ),
      actionButton("submit", "Submit Data")
    ),
    mainPanel(
      tableOutput("results_table"),
      verbatimTextOutput("file_status")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store the processed data
  rv <- reactiveValues(data = NULL)

  # Update subdomain choices based on domain selection
  observeEvent(input$domain, {
    # Add your subdomain choices logic here
  })

  # Update scale choices based on test selection
  observeEvent(input$test_name, {
    # Add your scale choices logic here
  })

  # Process data when submit button is clicked
  observeEvent(input$submit, {
    # Create data frame
    data <- data.frame(
      test = input$test,
      test_name = input$test_name,
      test_type = "npsych_test",
      scale = input$scale,
      raw_score = input$raw_score,
      score = input$score,
      percentile = NA,
      true_score = NA,
      ci_95 = NA,
      ci_lo = NA,
      ci_hi = NA,
      range = NA,
      range_lower = NA,
      range_upper = NA,
      score_type = input$score_type,
      domain = input$domain,
      subdomain = input$subdomain,
      narrow = input$narrow,
      pass = input$pass,
      verbal = input$verbal,
      timed = input$timed,
      absort = paste0(tolower(input$test), "_", tolower(input$scale), "_", seq_len(1)),
      description = NA,
      result = NA
    )

    # Apply your processing functions
    data <- scale_description(data, input$scale, "description")
    data <- compute_percentile_range(data, input$score, input$score_type, "percentile", "range")
    data <- glue_result(data, data$scale, data$description, data$range, data$result)

    # Calculate CI 95%
    ci_values <- bwu::calc_ci_95(
      ability_score = input$score,
      mean = input$mean,
      standard_deviation = input$stdev,
      reliability = input$reliability
    )

    data$true_score <- ci_values["true_score"]
    data$ci_lo <- ci_values["lower_ci_95"]
    data$ci_hi <- ci_values["upper_ci_95"]
    data$ci_95 <- paste0(data$ci_lo, " - ", data$ci_hi)

    # Store processed data
    rv$data <- data

    # Write CSV files
    test <- data$test
    scale <- snakecase::to_snake_case(data$scale)

    # Write to pre_csv
    file_path_pre <- here::here("pre_csv", paste0(test, "_", scale, ".csv"))
    write_excel_csv(data, file_path_pre, append = FALSE)

    # Write to csv
    file_path_test <- here::here("csv", paste0(test, ".csv"))
    write_excel_csv(data, file_path_test,
      append = TRUE,
      col_names = !file.exists(file_path_test)
    )

    # Write to g3.csv
    file_path_g <- here::here("csv", "g3.csv")
    write_excel_csv(data, file_path_g,
      append = TRUE,
      col_names = !file.exists(file_path_g)
    )
  })

  # Display results
  output$results_table <- renderTable({
    req(rv$data)
    rv$data
  })

  output$file_status <- renderText({
    req(rv$data)
    "Data successfully saved to CSV files"
  })
}

# Run the app
shinyApp(ui = ui, server = server)
