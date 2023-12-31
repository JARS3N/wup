server <- function () {
  require(shiny)
  require(RMySQL)
  require(XML)
  require(DT)
  require(dplyr)
  require(foam)

  shinyServer(function(input, output, session) {
    session$onSessionEnded(function() {
      kill_shiny()
    })
    observeEvent(input$Quit, {
      kill_shiny()
    })

    output$foo <- wup::clean_selections()



    observeEvent(input$goButton, {
      output$MSG <- renderText("Ready")
      if (length(input$goButton$name) > 0) {
        output$MSG <- renderText("Select Directory")
        output$MSG <- renderText("Munging Data...")
        # point to files uploaded to temp directory
        fls <- input$goButton$datapath
        # process them into foam objects
        message("debug: map foam::new")
        procd <- purrr::map(fls, foam::new)
        # process foam objects into dataframe for kraken
        message("debug: map foam::new")
        DATA <- purrr::map(procd, wup::format_kraken)
        message("data munged")

        output$foo2 <- DT::renderDataTable(dplyr::bind_rows(DATA))
        sum_tbl <- purrr::map_df(procd, wup::sum_table_row) %>%
          arrange(sn)

        if (exists("sum_tbl")) {
          sum_tbl$use <- T
          output$foo <- DT::renderDataTable(
            sum_tbl,
            selection = list(selected = which(sum_tbl$valid ==
                                                FALSE)),
            server = F,
            options = list(dom = "t",
                           pageLength = nrow(sum_tbl)),
            rownames = F
          )
        }
        observeEvent(input$foo_rows_selected, {
          sum_tbl
          sum_tbl$use <- T
          sum_tbl$use[input$foo_rows_selected] <- F
          output$foo <- DT::renderDataTable(
            sum_tbl,
            selection = list(selected = input$foo_rows_selected),
            server = F,
            options = list(dom = "t", pageLength = nrow(sum_tbl)),
            rownames = FALSE
          )
          last <- input$foo_rows_selected
          print(last)
        })
        observeEvent(input$desel, {
          sum_tbl$use <- T
          output$foo <-
            output$foo <- DT::renderDataTable(
              sum_tbl,
              selection = list(selected = NULL),
              server = F,
              options = list(dom = "t", pageLength = nrow(sum_tbl)),
              rownames = FALSE
            )
        })
        output$exprt <-
          downloadHandler(filename <- function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
          }, content <- function(file) {
            OUT <- DATA %>%
              wup::remove_deselected(., input$foo_rows_selected) %>%
              dplyr::bind_rows() %>%
              arrange(., sn, Well)

            write.csv(OUT, file, row.names = F)
          })
        observeEvent(input$upload, {
          # wup::upload_all(remove_deselected(DATA,input$foo_rows_selected))
          AA <<-
            wup::remove_deselected(DATA, input$foo_rows_selected)
          AAA <<- lapply(AA, upload_if_new)
          message("upload sequence completed.")
           output$foo <- wup::clean_selections()
          DATA<-NULL
          OUT<-NULL
          sum_tbl<-NULL
        })
      }
    })
  })
}
