# FastSimNullDistR

Fast simulated Null Distributions in R, faster than using `do() * diffmean()` or `do() * diffprop()`. 

## Function fastSimNullDistRMean

This function is a (nearly) drop-in replacement for `do() * diffmean()`, like the following example will show:

```
  df <- data.frame(
     y = c(rnorm(100, mean=-1, sd=2), rnorm(100, mean=1, sd=2))
     x = c(rep("m", 100), rep("f", 100))
  )
  
   # Using Mosaic:
   NullDistMosaic <- do(10000) * diffmean(y ~ shuffle(x), data=df)
  
   # Using fastNullDistR:
   NullDistFNDR <- fastNullDistRMean(y ~ x, data=df, n=10000)
```

## Function fastSimNullDistRProp

This function is a (nearly) drop-in replacement for `do() * diffrop()`, like the following example will show:
```
    library(mosaic)
    library(mosaicCore)

    df <- data.frame(
        y = rbinom(200, size=1, prob=0.4),
        x = c(rep("m", 100), rep("f", 100))
    )
     
    # Using Mosaic:
    NullDistMosaic <- do(10000) * diffprop(y ~ x, success="1")

    # Using fastNullDistR:
    NullDistFNDR <- fastNullDistRProp(y ~ x, success="1", data=df, n=10000)
```
