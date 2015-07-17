data <- read.csv("c:/citadel2015/data/housing.csv")
require(stats)

Price<-data$Price
Size<-data$Size

#Estimate Simple Linear Regression
lm.out = lm(Price ~ Size)
lm.out

summary(lm.out)

plot(Price ~ Size, main="Housing Price")
abline(lm.out, col="red")

anova(lm.out)

# Build prediction and confidence intervals

newdata = data.frame(Size=2)
predict(lm.out,newdata,interval="predict")

predict(lm.out,newdata,interval="confidence")