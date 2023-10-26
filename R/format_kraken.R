format_kraken<-function (u) 
{
    out <- mutate(u$summary, Lot = paste0(u$type, u$lot), sn = u$sn, 
        Inst = u$Inst) %>% select(-contains("delta"))
    setNames(out, gsub("pH_target", "Target", names(out)))
    out
}
