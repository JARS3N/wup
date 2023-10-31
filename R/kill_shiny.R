kill_shiny<-function(){
  message("Exiting application.")
  shiny::stopApp(returnValue = invisible())
}
