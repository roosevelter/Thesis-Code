# Summing up fatalities by country-year. Be sure to read in GTD data first.

no.doubt.terrorism = subset(terrorism, doubtterr == 0 & is.na(nkill) == F, select = c(country_txt, nkill, iyear, country))
no.doubt.terrorism = aggregate(x = setup, by = list(setup$country_txt, setup$iyear), FUN = function(x) sum(as.numeric(x)))


Try to get the number of terrorist attacks for a given country-year.

terrorist.attack.number = rep(NA, 43)


for (j in unique(year)) {
      for (i in 1:43) {
          terrorist.attack.number[i] = nrow(no.doubt.terrorism[no.doubt.terrorism$iyear == j & no.doubt.terrorism$nkill != 0,])
    }
}


  for (i in unique(year)) {
    terrorist.attack.number[seq_along(i)] = nrow(no.doubt.terrorism[no.doubt.terrorism$iyear == i & no.doubt.terrorism$nkill != 0,])
  }


tapply(k, l, function (x) nrow)
