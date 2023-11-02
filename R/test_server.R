server <- function(input, output, session) {
  
  # Load required libraries within the server function
  library(shiny)
  library(RMySQL)
  library(XML)
  library(DT)
  library(dplyr)
  library(purrr)
  library(foam)

  # End session
  session$onSessionEnded(kill_shiny)

  # User initiated quit
  observeEvent(input$Quit, kill_shiny)

  # Initial output setup
  output$foo <- wup::clean_selections()

  observeEvent(input$goButton, {
    output$MSG <- renderText("Munging Data...")

    if (length(input$goButton$name) > 0) {
      fls <- input$goButton$datapath  # Get uploaded file paths

      # Process files into foam objects and then into a dataframe
      procd <- purrr::map(fls, foam::new)
      DATA <- purrr::map(procd, wup::format_kraken)

      # Bind the data rows together and display in a table
      output$foo2 <- DT::renderDataTable(dplyr::bind_rows(DATA))
      
      # Create summary table
      sum_tbl <- create_summary_table(procd)

      # React to row selection in the DT table
      observeEvent(input$foo_rows_selected, {
        update_selection_in_sum_table(sum_tbl, input$foo_rows_selected)
      })
      
      # React to deselection
      observeEvent(input$desel, {
        reset_selection_in_sum_table(sum_tbl)
      })
      
      # Define export functionality
      output$exprt <- create_export_handler(DATA, input$foo_rows_selected)
      
      # Upload functionality
      observeEvent(input$upload, {
        handle_upload(DATA, input$foo_rows_selected)
        # Clear variables
        DATA <- NULL
        sum_tbl <- NULL
      })
    } else {
      output$MSG <- renderText("Please select a directory.")
    }
  })
}

# Define helper functions here
# ...

# Define kill_shiny function if it doesn't exist elsewhere
kill_shiny <- function() {
  stopApp()
}
