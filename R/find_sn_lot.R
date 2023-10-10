find_sn_lot <- function(x, y, z) {
  #x=Lot,y=Inst,z=sn
  db <- adminKraken::con_dplyr()
  n <- suppressWarnings(
    tbl(db, "wetqc_cartridge") %>%
      filter(Lot == local(x)) %>%
      filter(Inst == local(y)) %>%
      filter(sn == local(z)) %>%
      count() %>%
      collect() %>%
      .$n
  )
  rm(db)
  return(n)
}
