# Set your goddamn working directory.
setwd("C:/Users/lc1976/Desktop")

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

testMerge = merge(possible.solution, swiid_data, by = c("country", "year"))
