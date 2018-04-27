# Test
library(mosaic, quietly = TRUE)
library(mosaicCore, quietly = TRUE)

source("R/fastSimNullDistR_work.R")
source("R/fastSimNullDistRMean.R")

TheSeed <- 1848

# Herunterladen
if (!file.exists("tips.csv")) {
    download.file("https://goo.gl/whKjnl", destfile = "tips.csv")
}

# Einlesen in R
tips <- read.csv2("tips.csv")

# Mosaic
set.seed(TheSeed)
system.time(
    NullDistMosaic <- do(10000) * diffmean(total_bill ~ shuffle(time), data=tips)
)

# fastSNDRM
set.seed(TheSeed)
system.time(
    NullDistFSNDRM <- fastSimNullDistRMean(total_bill ~ time, data=tips)
)

qqplot(NullDistFSNDRM$diffmean, NullDistMosaic$diffmean)