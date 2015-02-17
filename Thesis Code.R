# Before all of this, clean the data sets so that each data set has matching country names. Country names should be matched to the GTD.

# Set your goddamn working directory. Load the required packages.
setwd("C:/Users/lc1976/Desktop")
install.packages(c("xlsx", "reshape2", "dplyr", "tidyr"))
library(xlsx); library(reshape2); library(tidyr); library(tidyr)

# Recording whether or not there was a terrorist attack for a country-year, and if so, the number. Be sure to read in GTD data first.
no.doubt.terrorism = subset(terrorism, doubtterr == 0 & iyear > 1991)
no.doubt.terrorism = table(no.doubt.terrorism$country_txt, no.doubt.terrorism$iyear)
no.doubt.terrorism = melt(no.doubt.terrorism)

terroristattack = rep(NA, nrow(no.doubt.terrorism))
terroristattack = ifelse(no.doubt.terrorism[,3] != 0, 1, 0)

no.doubt.terrorism = cbind(no.doubt.terrorism[,1:2], terroristattack, no.doubt.terrorism[,3])
colnames(no.doubt.terrorism) = c("country", "year", "terroristattack", "nattacks")

# Cleaning the matched SWIID data.
load("SWIIDv5_0.RData")

swiid_data = lapply(swiid, function(x) x[x$year>=1970,, drop=F])
swiid_data = as.data.frame(swiid_data)
swiid_data = subset(swiid_data, is.na(gini_net) == F, select = c(country, year, gini_net))

first.merge = merge(no.doubt.terrorism, swiid_data, by = c("country", "year"))

# Load in and clean the matched PRS data. 
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

# Load in matched GDP per capita data from the World Bank.
gdppc = gdppc[ , -c(2:4, 59)]
gdppc = melt(gdppc)
colnames(gdppc) = c("country" , "year", "gdppc")
gdppc$year = substring(gdppc$year, 2)

thirdmerge = merge(secondmerge, gdppc, by = c("country", "year"))
thirdmerge = thirdmerge[,-3]

# Load in matched Freedom House data. Remember that the Freedom House data should be converted to .xlsx format first.
liberties = read.xlsx("liberties.xlsx", 1)
liberties = liberties[1:205, c(1, 59:ncol(liberties))]
liberties = liberties[,1:67]
liberties = na.omit(liberties)
country = liberties[,1]
country = factor(country)
country = as.character(country)
liberties = liberties[,2:ncol(liberties)]
Nth.delete = function(dataframe, n) dataframe[,-(seq(n,to=ncol(dataframe),by=n))]
liberties = Nth.delete(liberties, 3)
liberties = byapply(liberties, 2, rowMeans)
liberties = cbind(country, liberties)
colnames(liberties) = c("country", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")

liberties = as.data.frame(liberties)
liberties = melt(liberties, id.vars = "country")
colnames(liberties) = c("country", "year", "polliberties")

fourthmerge = merge(thirdmerge, liberties, by = c("country", "year")
fourthmerge = fourthmerge[,-3]

# Clean and load in the trade openness data from the World Bank. This is being used as a measure of globalization.
colnames(tradeopenness)[1] = "country" ; tradeopenness = tradeopenness[,-c(2:4, 59)]
tradeopenness = melt(tradeopenness)
colnames(tradeopenness)[2:3] = c("year" , "tradeopenness")
tradeopenness$year = substring(tradeopenness$year, 2)

fifthmerge = merge(fourthmerge, tradeopenness, by = c("country", "year")

# Load in the population data from the World Bank.
colnames(lnpopulation)[1] = "country" ; lnpopulation = lnpopulation[,-c(2:4, 59)]
lnpopulation = melt(lnpopulation)
colnames(lnpopulation)[2:3] = c("year" , "lnpopulation")
lnpopulation$year = substring(lnpopulation$year, 2)
lnpopulation$lnpopulation = log(lnpopulation$lnpopulation)

sixthmerge = merge(fifthmerge, lnpopulation, by = c("country", "year"))

# Clean and load in regime durability variable (durable) from Polity IV.
polity4 = subset(polity4, year > 1991, select = c(country, year, durable))
rownames(polity4) = NULL

seventhmerge = merge(sixthmerge, polity4, by = c("country", "year"))

# Load in the Ethnic Power Relations 2014 data. Calculate ELF proposed by Alesina et al. (2003) from it.
epr$year = mapply(seq, epr$from, epr$to, SIMPLIFY = F)

epr = epr %>% 
  unnest(year) %>% 
  select(-from,-to)
  
epr = subset(epr, year > 1991 & year < 2014, select = c(statename, year, gwgroupid, size))

elf = by(epr[,"size"], epr[,c("statename", "year")], function(x) 1-sum(x^2))
