# Summing up the number of fatalities for each year in the GTD data. But first, some cleaning.

no.doubt.terrorism = subset(terrorism, region == 10 & doubtterr == 0)

foo = c(rep(NA, length(unique(no.doubt.terrorism$iyear))))
fatality.years = c(rep(NA, length(unique(no.doubt.terrorism$iyear))))

for (i in 1:length(fatality.years) {
  foo[i] = subset(no.doubt.terrorism, iyear == unique(no.doubt.terrorism$iyear)[i], select = nkill)
  fatality.years[i] = colSums(foo[i], na.rm = T) 
}

no.doubt.terrorism = cbind(no.doubt.terrorism, fatality.years)
