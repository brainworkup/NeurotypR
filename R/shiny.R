#' Launch Neurotypical Data Entry Shiny App
#'
#' This function launches a Shiny app for entering and managing neuropsychological data.
#' The app allows users to input data, save it to a CSV file, and generate reports.
#'
#' @return A Shiny app object
#' @export
#'
#' @examples
#' \dontrun{
#' run_data_entry_app()
#' }
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel dateInput textInput actionButton downloadButton tableOutput reactiveValues observeEvent renderTable downloadHandler shinyApp
#' @importFrom rmarkdown render
#' @importFrom tinytex tinytex_root
#' @importFrom utils write.csv
run_data_entry_app <- function() {
  # Define UI
  ui <- fluidPage(
    titlePanel("Data Entry App"),
    sidebarLayout(
      sidebarPanel(
        dateInput("date_input", "Select a Date:", value = Sys.Date()),
        textInput("text_input", "Enter Text:"),
        actionButton("save_button", "Save Data"),
        br(),
        br(),
        downloadButton("download_csv", "Download CSV"),
        downloadButton("download_pdf", "Download PDF")
      ),
      mainPanel(
        tableOutput("data_table")
      )
    )
  )

  # Define Server
  server <- function(input, output, session) {
    # Reactive Values to Store Data
    rv <- reactiveValues(data = data.frame(
      Date = as.Date(character()),
      Text = character(),
      stringsAsFactors = FALSE
    ))

    # Observe Save Button Click
    observeEvent(input$save_button, {
      # Create a new data frame from inputs
      new_data <- data.frame(
        Date = input$date_input,
        Text = input$text_input,
        stringsAsFactors = FALSE
      )

      # Append new data to existing data
      rv$data <- rbind(rv$data, new_data)

      # Write data to local CSV file
      write.csv(rv$data, "user_data.csv", row.names = FALSE)
    })

    # Render Table of Data
    output$data_table <- renderTable({
      rv$data
    })

    # Download Handler for CSV
    output$download_csv <- downloadHandler(
      filename = function() {
        paste("user_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(rv$data, file, row.names = FALSE)
      }
    )

    # Download Handler for PDF
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste("user_data_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Create a temporary R Markdown file
        tempReport <- file.path(tempdir(), "report.Rmd")

        # Get the path to the template file in the package
        template_path <- system.file("rmarkdown/templates/neurotypr/skeleton/skeleton.Rmd",
          package = "NeurotypR"
        )

        # If the template doesn't exist in the package, use a basic template
        if (template_path == "") {
          template_content <- paste(
            "---",
            "title: \"Neuropsychological Data Report\"",
            "date: \"`r format(Sys.Date(), '%B %d, %Y')`\"",
            "output: pdf_document",
            "params:",
            "  data: NULL",
            "---",
            "",
            "## Data Summary",
            "",
            "```{r, echo=FALSE}",
            "knitr::kable(params$data)",
            "```",
            sep = "\n"
          )
          writeLines(template_content, tempReport)
        } else {
          file.copy(template_path, tempReport, overwrite = TRUE)
        }

        # Set up parameters to pass to Rmd document
        params <- list(data = rv$data)

        # Render the PDF
        rmarkdown::render(tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
  }

  # Return the Shiny app object
  shiny::shinyApp(ui = ui, server = server)
}
