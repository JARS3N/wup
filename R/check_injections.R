check_injections<-function (u) 
{
    if (u$assay == "gain") {
        return(TRUE)
    }
    F0hat <- ((u$O2_coefs$Ksv * pp::O2(37)) + 1) * u$O2_coefs$target
    F0per00dif <- 100 * ((u$summary$F0 - F0hat)/F0hat)
    inj <- sum(abs(F0per00dif >= 10))
    (100 * (inj/length(F0per00dif))) <= 5
}
