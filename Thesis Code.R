# Summing up fatalities by country-year. Be sure to read in GTD data first.

no.doubt.terrorism = subset(terrorism, doubtterr == 0 & is.na(nkill) == F, select = c(country_txt, nkill, iyear, country))
no.doubt.terrorism = aggregate(x = no.doubt.terrorism, by = list(no.doubt.terrorism$country_txt, no.doubt.terrorism$iyear), FUN = function(x) sum(as.numeric(x)))

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
colnames(possible.solution) = c("Country", "Year", "Victims", "Country Code", "Terrorist Attack")

# Cleaning the SWIID data.

swiid_data = lapply(swiid, function(x) x[x$year>=1970,, drop=F])
