# R Shiny App Code

# Load required libraries
library(shiny)
library(rmarkdown) # For rendering PDF reports
library(tinytex) # For PDF generation (if not already installed)

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
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

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

# Run the Application
shinyApp(ui = ui, server = server)
