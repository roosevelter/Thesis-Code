# Summing up the number of fatalities for each year in the GTD data. But first, some cleaning.

no.doubt.terrorism = subset(terrorism, region == 10 & doubtterr == 0)

fatality.years = rep(NA, length(unique(no.doubt.terrorism$iyear)))

for (i in 1:length(fatality.years)) {
  fatality.years[i] = colSums(subset(no.doubt.terrorism, iyear == unique(no.doubt.terrorism$iyear)[i], select = nkill))
}

no.doubt.terrorism = cbind(no.doubt.terrorism, fatality.years)
