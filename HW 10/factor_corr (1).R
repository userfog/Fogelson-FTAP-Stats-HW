data<-read.csv("c:/citadel2014/data/factor.csv")
require(fGarch)
date <- as.Date(data$Date, format="%m/%d/%Y")

appl<-data$appl
csco<-data$csco

mappl<-lm(data$appl~data$sp500)
mcsco<-lm(data$csco~data$sp500)
summary(mappl)
summary(mcsco)
ca<-coef(mappl)
cc<-coef(mcsco)

residappl<-residuals(mappl)
sappl<-mean(residappl^2)
residcsco<-residuals(mcsco)
scsco<-mean(residcsco^2)

sappl
scsco

garch <- garchFit(~garch(2,2),data=data$sp500)
vol<-volatility(garch)
plot(vol, type="l")

cov=ca[2]*cc[2]*vol^2
vara=ca[2]^2*vol^2+sappl
varc=cc[2]^2*vol^2+scsco
prod=appl*csco
ma <- filter(prod,rep(1/252,100), sides=1)
sda<-filter(appl^2,rep(1/252,100), sides=1)
sdc<-filter(csco^2,rep(1/252,100), sides=1)
corrma<-ma/sqrt(sda*sdc)
corr=cov/sqrt(vara*varc)
plot(date,corrma,type="l", col="blue")
plot(date,corr,type="l", col="blue")
points(date,corrma,type="l",col="red")
