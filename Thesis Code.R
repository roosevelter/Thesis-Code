# Before all of this, clean the data sets so that each data set has matching country names. They don't all include country codes.

# Set your goddamn working directory. Load the required packages.
setwd("C:/Users/lc1976/Desktop")
install.packages(c("xlsx", "reshape2"))
library(xlsx); library(reshape2)

# Summing up fatalities by country-year. Be sure to read in GTD data first.

no.doubt.terrorism = subset(terrorism, doubtterr == 0 & is.na(nkill) == F, select = c(country_txt, nkill, iyear, country))
possible.solution = aggregate(x = no.doubt.terrorism, by = list(no.doubt.terrorism$country_txt, no.doubt.terrorism$iyear), FUN = function(x) sum(as.numeric(x)))

attack = vector("numeric", 3075)
possible.solution = cbind(possible.solution, attack)

for (i in 1:length(possible.solution[,7])) {
  if (possible.solution$nkill[i] == 0) {
    possible.solution[i,7] = 0
  }
  if (possible.solution$nkill[i] > 0) {
    possible.solution[i,7] = 1
  }
}


possible.solution = possible.solution[,-c(3, 5)]
colnames(possible.solution) = c("country", "year", "victims", "country code", "terrorist attack")

# Cleaning the SWIID data.
load("SWIIDv5_0.RData")

swiid_data = lapply(swiid, function(x) x[x$year>=1970,, drop=F])
swiid_data = as.data.frame(swiid_data)
swiid_data = subset(swiid_data, is.na(gini_net) == F, select = c(country, year, gini_net))

first.merge = merge(possible.solution, swiid_data, by = c("country", "year"))

# Load in the PRS data. 
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

ethnic.tensions = byapply(ethnic.tensions[,3:ncol(ethnic.tensions) 12, rowMeans)
colnames(ethnic.tensions) = c(country, c(1984:2013))
