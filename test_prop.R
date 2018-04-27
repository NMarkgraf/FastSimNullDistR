# Test
library(mosaic, quietly = TRUE)
library(mosaicCore, quietly = TRUE)

source("R/fastSimNullDistR_work.R")
source("R/fastSimNullDistRProp.R")

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
    NullDistMosaic <- do(10000) * diffprop(smoker ~ shuffle(time), success="No", data=tips)
)

# fastSNDRM
set.seed(TheSeed)
system.time(
    NullDistFSNDRM <- fastSimNullDistRProp(smoker ~ time, success="No", data=tips)
)

qqplot(NullDistFSNDRM$diffprop, NullDistMosaic$diffprop)

gf_histogram( ~ diffprop, color="red", nint=100, data=NullDistFSNDRM) %>%
gf_histogram( ~ diffprop, fill = "skyblue", nint=100, data=NullDistMosaic)