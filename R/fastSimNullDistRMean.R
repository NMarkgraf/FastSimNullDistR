#' fastNullDistRMean implements a fast(er) way to create
#' a simulated null distibution for the differances of means.
#'
#'@author Norman Markgraf
#'
#'@examples
#'
#'   y <- c(rnorm(100, mean=-1, sd=2), rnorm(100, mean=1, sd=2))
#'   x <- c(rep("m", 100), rep("f", 100))
#' 
#'   nulldist <- fastNullDistRMean(y ~ x, data=df, n=10000)
#' 
#' ## is a subsitution for
#' 
#'   library(mosiac)
#'   nulldist <- do(10000) * diffmean(y ~ shuffle(x), data=df)
#'   
#' @export

fastSimNullDistRMean <- function(formula, data=NULL, n=10000) {
    # todo:
    # check if right hand side (rhs) is a factor with to levels
    form_lhs <- lhs(formula)
    form_rhs <- rhs(formula)
    chr_lhs <- as.character(lhs(formula))
    chr_rhs <- as.character(rhs(formula))
    #
    if (!is.null(data) && (chr_rhs %in% names(data))) {
        fkts <- as.factor(data[,chr_rhs])
    } else {
        fkts <- as.factor(eval(form_rhs))
    }
    # We need exactly 2 levels!
    stopifnot(length(levels(fkts)) == 2)
    
    fkt <- as.integer(fkts) -1
    if (!is.null(data) && (chr_lhs %in% names(data))) {
        dta <- data[,chr_lhs]
    } else {
        dta <- eval(form_lhs)
    }
    # We need numeric data!
    stopifnot(is.numeric(dta))

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
    data.frame(diffmean = retVec)
}