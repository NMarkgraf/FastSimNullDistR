fastSimNullDistR_work <- function(fkt, dta, n) {
    counter <- 0
    
    N <- length(dta)
    N1 <- sum(fkt)
    N2 <- N - N1
    
    total <- sum(dta)
    retVec <- vector("numeric", n)
    
    fkt_cur <- fkt
    
    while(counter < n) {
        counter <- counter + 1
        total_n1 <- sum(fkt_cur * dta)
        total_n2 <- total - total_n1
        retVec[counter] <- (total_n1 / N1 - total_n2 / N2)
        #        fkt_cur <- shuffle(fkt)  # use mosiac::shuffle
        #        fkt_cur <- shuffle(fkt_cur)  # use mosiac::shuffle (???)
        fkt_cur <- base::sample(fkt)  # use base::sample
        #       fkt_cur <- base::sample(fkt_cur)  # use base::sample (???)
    }
    retVec
}

