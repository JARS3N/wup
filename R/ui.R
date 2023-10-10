ui <-
  function ()
  {
    require(shiny)
    require(shinyjs)
    shinyUI(pageWithSidebar(
      headerPanel("Upload WetQC Data"),
      sidebarPanel(
        fileInput(
          "goButton",
          "select wave files",
          accept = c("asyr", ".asyr"),
          multiple = T
        ),
        width = 1.5
      ),
      mainPanel(
        actionButton("Quit", "Exit", icon = icon("times-circle")),
        shiny::downloadButton("exprt", "download"),
        actionButton("upload",
                     "Database Upload"),
        br(),
        strong("Highlighted runs are not uploaded to the database"),
        tabsetPanel(tabPanel(
          "Selection & Validation",
          DT::dataTableOutput("foo"),
          actionButton("desel", "Deselect All Rows", icon = icon("undo"))
        ))
      )
    ))
  }
