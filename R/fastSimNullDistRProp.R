#' fastNullDistRProp implements a fast(er) way to create
#' a simulated null distibution for the differances of proportions.
#'
#'@author Norman Markgraf
#'
#'@examples
#' 
#'  df <- data.frame(
#'     y = rbinom(200, size=1, prob=0.4),
#'     x = c(rep("m", 100), rep("f", 100))
#'  )
#'   
#'   nulldist <- fastNullDistRMean(y ~ x, success="1", data=df, n=10000)
#' 
#' ## is a subsitution for
#' 
#'   library(mosiac)
#'   nulldist <- do(10000) * diffprop(y ~ shuffle(x), success="1", data=df)
#'   
#' @export

fastSimNullDistRProp <- function(formula, success=NULL, data = parent.frame(), only.2=TRUE, n=10000) {
    stopifnot(!is.null(success))
    chr_lhs <- force(as.character(lhs(formula)))
    chr_rhs <- force(as.character(rhs(formula)))
    if (is.environment(data)) {
        assign("chr_lhs", chr_lhs, data)
        assign("chr_rhs", chr_rhs, data)
    }
    fkts <- as.factor(with(data, get(chr_rhs)))
    # We need exactly 2 levels!
    stopifnot(length(levels(fkts)) == 2)
    
    fkt <- as.integer(fkts) - 1
    dtas <- with(data, get(chr_lhs))
    # We need numeric data!
    #stopifnot(!is.numeric(dtas))

    dtaf <- as.factor(dtas)

    stopifnot(success %in% levels(dtaf))

    dta <- as.integer(dtaf == success)

    data.frame(diffprop = fastSimNullDistR_work(fkt, dta, n))
}