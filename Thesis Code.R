# Before all of this, clean the data sets so that each data set has matching country names. Country names should be matched to the GTD.

# Set your goddamn working directory. Load the required packages.
setwd("C:/Users/lc1976/Desktop")
install.packages(c("xlsx", "reshape2"))
library(xlsx); library(reshape2)

# Recording whether or not there was a terrorist attack for a country-year, and if so, the number. Be sure to read in GTD data first.
no.doubt.terrorism = subset(terrorism, doubtterr == 0 & iyear > 1991)
no.doubt.terrorism = table(no.doubt.terrorism$country_txt, no.doubt.terrorism$iyear)
no.doubt.terrorism = melt(no.doubt.terrorism)

terroristattack = rep(NA, nrow(no.doubt.terrorism))
terroristattack = ifelse(no.doubt.terrorism[,3] != 0, 1, 0)

no.doubt.terrorism = cbind(no.doubt.terrorism[,1:2], terroristattack, no.doubt.terrorism[,3])
colnames(no.doubt.terrorism) = c("country", "year", "terroristattack", "nattacks")

# Cleaning the SWIID data.
load("SWIIDv5_0.RData")

swiid_data = lapply(swiid, function(x) x[x$year>=1970,, drop=F])
swiid_data = as.data.frame(swiid_data)
swiid_data = subset(swiid_data, is.na(gini_net) == F, select = c(country, year, gini_net))

first.merge = merge(no.doubt.terrorism, swiid_data, by = c("country", "year"))

# Load in and clean the PRS data. 
ethnic.tensions = read.xlsx("CountryData.xlsx", 1)
ethnic.tensions = ethnic.tensions[ ,-2]
ethnic.tensions = na.omit(ethnic.tensions)

byapply <- function(x, by, fun, ...)
{
    # Create index list
    if (length(by) == 1)
    {
        nc <- ncol(x)
        split.index <- rep(1:ceiling(nc / by), each = by, length.out = nc)
    } else # 'by' is a vector of groups
    {
        nc <- length(by)
        split.index <- by
    }
    index.list <- split(seq(from = 1, to = nc), split.index)

    # Pass index list to fun using sapply() and return object
    sapply(index.list, function(i)
            {
                do.call(fun, list(x[, i], ...))
            })
}

ethnic.tensions = byapply(ethnic.tensions[,3:ncol(ethnic.tensions)], 12, rowMeans)
colnames(ethnic.tensions) = c("country", c(1984:2013))
ethnic.tensions = melt(ethnic.tensions)
colnames(ethnic.tensions) = c("country", "year", "ethnictension")

second.merge = merge(first.merge, ethnic.tensions, by = c("country", "year"))

# Load in GDP per capita data from the World Bank.
gdppc = gdppc[ , -c(2:4, 59)]
gdppc = melt(gdppc)
