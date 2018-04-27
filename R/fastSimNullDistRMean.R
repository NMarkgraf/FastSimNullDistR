#' fastNullDistRMean implements a fast(er) way to create
#' a simulated null distibution for the differances of means.
#'
#'@author Norman Markgraf
#'
#'@examples
#'
#'  df <- data.frame(
#'     y = c(rnorm(100, mean=-1, sd=2), rnorm(100, mean=1, sd=2))
#'     x = c(rep("m", 100), rep("f", 100))
#'  )
#'  
#'   nulldist <- fastNullDistRMean(y ~ x, data=df, n=10000)
#' 
#' ## is a subsitution for
#' 
#'   library(mosiac)
#'   nulldist <- do(10000) * diffmean(y ~ shuffle(x), data=df)
#'   
#' @export

fastSimNullDistRMean <- function(formula, data = parent.frame(), only.2=TRUE, n=10000) {
    chr_lhs <- force(as.character(lhs(formula)))
    chr_rhs <- force(as.character(rhs(formula)))
    if (is.environment(data)) {
        assign("chr_lhs", chr_lhs, data)
        assign("chr_rhs", chr_rhs, data)
    }
    fkts <- as.factor(with(data, get(chr_rhs)))
    # currently not supported!
    stopifnot(only.2)
    # We need exactly 2 levels!
    stopifnot(length(levels(fkts)) == 2)
    
    fkt <- as.integer(fkts) - 1
    
    dta <- with(data, get(chr_lhs))

    # We need numeric data!
    stopifnot(is.numeric(dta))

    data.frame(diffmean = fastSimNullDistR_work(fkt, dta, n))

}