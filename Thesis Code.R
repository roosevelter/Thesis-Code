# Summing up fatalities by country-year. Be sure to read in GTD data first.

no.doubt.terrorism = subset(terrorism, doubtterr == 0 & is.na(nkill) == F, select = c(country_txt, nkill, iyear, country))
no.doubt.terrorism = aggregate(x = setup, by = list(setup$country_txt, setup$iyear), FUN = function(x) sum(as.numeric(x)))


