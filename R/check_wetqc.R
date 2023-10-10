check_wetqc <- function(u) {
  x <- select(u, Inst, Lot, sn) %>% distinct()
  find_sn_lot(x$Lot,x$Inst,x$sn) > 0
}
