library(gdata)
library(plyr)

mfunds <- read.xls(xls = "mfunds (1).xls", sheet = 2)
sds <- sapply(mfunds, sd)
means <- sapply(mfunds, mean)
