get_cartid<-function(u){
  db<-adminKraken::con_dplyr()
  out<-suppressWarnings(tbl(db,"cartridgetype") %>%
                          filter(typename==u) %>%
                          select(cartridgetypeid) %>%
                          collect() %>%
                          unlist(.,use.names = F))
  rm(db)
  return(out)
}
