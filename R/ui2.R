# UI Function
ui <-function(){
fluidPage(
  # Load necessary libraries for UI
  library(shiny),
  library(shinyjs),

  headerPanel("Upload WetQC Data"),
  sidebarPanel(
    fileInput(
      "goButton",
      "Select wave files",
      accept = c(".asyr", ".ASYR"),
      multiple = TRUE
    )
  ),
  mainPanel(
    actionButton("Quit", "Exit", icon = icon("times-circle")),
    downloadButton("exprt", "Download"),
    actionButton("upload", "Database Upload"),
    br(),
    strong("Highlighted runs are not uploaded to the database"),
    DTOutput("foo"),
    actionButton("desel", "Deselect All Rows", icon = icon("undo")),
    # Add a box to display logs
    verbatimTextOutput("logs")
  )
)
}
