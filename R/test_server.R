server2 <- function(){

  # Load necessary libraries
  library(shiny)
  library(RMySQL)
  library(XML)
  library(DT)
  library(dplyr)
  library(foam)
  
function(input, output, session) {


  # Define a reactive value to store logs
  log_messages <- reactiveValues(logs = "")

  # A function to add messages to the log
  add_log <- function(message) {
    log_messages$logs <- paste0(log_messages$logs, "\n", Sys.time(), ": ", message)
  }

  # Function to safely call add_log within reactive contexts
  safe_log <- function(message) {
    tryCatch({
      add_log(message)
    }, error = function(e) {
      # handle error gracefully
    })
  }

  # Initialize the session
  session$onSessionEnded(function() {
    safe_log("Session ended.")
  })

  observeEvent(input$Quit, {
    safe_log("Quit button pressed.")
    stopApp()
  })

  output$foo <- renderDT({ wup::clean_selections() })

  observeEvent(input$goButton, {
    safe_log("Go button pressed.")
    # Add more reactive code here...
  })

  # Observe log changes and send to UI
  output$logs <- renderText({
    log_messages$logs
  })

  # ... rest of your server function ...
}
  }
