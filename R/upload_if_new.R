upload_if_new <- function(u) {
  # fix format
  w <- format_wetqc(u)
  #check if already uploaded
  w2 <- select(w, Lot, sn) %>% distinct()
  sample <- paste0("Lot:", w2$Lot, " , sn:", w2$sn)
  up <- check_wetqc(w)
  if (up) {
    msg <- paste0(sample, "already exists in database")
    message(msg)
  } else{
    msg <- paste0(sample, "upload to database")
    message(msg)
    DB <- adminKraken::con_mysql()
    dbWriteTable(
      DB,
      value = w,
      name = "wetqc",
      append = TRUE ,
      row.names = FALSE
    )
    RMySQL::dbDisconnect(DB)
  }
  return(up)
}
