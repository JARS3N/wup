check_positive<-function (vec) 
{
    if (all(is.na(vec))) {
        return(TRUE)
    }
    med <- median(vec, na.rm = T)
    if (med <= 0) {
        return(FALSE)
    }
    else {
        return(TRUE)
    }
}
