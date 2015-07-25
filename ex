train3$bit = do.call(paste0, train3[,3:ncol(train3)])
train3 = train3[,c(1,2,131)]
