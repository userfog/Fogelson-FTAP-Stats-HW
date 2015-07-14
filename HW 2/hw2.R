library(gdata)
library(plyr)
countries <- read.xls(xls = "country portfolios.xlsx", sheet = 1)
countries <- rename(x = countries, c("Global.Index.Key...Index.Monthly"="key", "Index.Price...Close.Monthly"="price", "Data.Date...Index.Monthly"="date", "Index.Name"="name"))

indexReturns <- data.frame(seq(1:184))

for(i in unique(countries$name)){
  
  considering <- countries[countries["name"] == i,]
  diff <- considering$price[-1] - considering$price[-length(considering$price)]
  basicReturn <- diff / considering$price[-length(considering$price)]
  indexReturns <- cbind(indexReturns, basicReturn)
  indexReturns <- rename(x = indexReturns, c("basicReturn"=i))
}

drops <- c("seq.1.184.")
indexReturns <- indexReturns[, !(names(indexReturns) %in% drops)]

varCov <- var(indexReturns, y=indexReturns)
corMat <- cor(indexReturns, indexReturns)


# The correlation matrix contains more interpretable information regarding the degree of 
# co-movement in each pair of series because it is normalized based on the standard deviations

barplot(as.matrix(corMat), beside = T, las = 2, col = terrain.colors(length(names(corMat[,1]))))
legend(1,c(names(corMat[,1])), cex=0.6, fill=terrain.colors(10))


# Clearly from the plot one can see that the cac40 and the FTSE 100 and the Composite DAX index are highly
# correlated, which makes sense given that we saw in class that European economic performance is highly
# correlated.