format_wetqc<-function(u){
  u<-ungroup(u)
  names(u)<-gsub("[.]","",names(u)) %>% gsub("KSV","ksv",.)

  select(u,Lot) %>%
    distinct() %>%
    mutate(CartridgetypeID=get_cartid(substr(Lot,1,1))) %>%
    left_join(u,.) %>%
    mutate(
      Lot = as.integer(substr(Lot,2,6)),
      O2Status = as.integer(O2Status=="Good"),
      pHStatus = as.integer(pHStatus=="Good"),
      sn=as.integer(sn),
      Inst=as.numeric(gsub("[A-Z]","",Inst))
    ) %>%
    select(-any_of("pH_target"))
}
