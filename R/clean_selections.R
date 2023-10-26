clean_selections<-function (){
    DT::renderDataTable(NULL, selection = list(selected = NULL), 
        server = F, options = list(dom = "t"), rownames = FALSE)
}
