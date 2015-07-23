data <- read.csv("c:/citadel2014/data/tbill.csv")
require(timeSeries)
require(tseries)
require(stats)
require(forecast)
year<-data$Year
tbill<-data$tbill
plot(tbill[1:80], tbill[2:81])

plot(year,tbill, type="l")

acf(tbill,lag.max=20)
pacf(tbill,lag.max=20)

ar<-arma(tbill,order=c(1,0))
summary(ar)
ar1<-arima(tbill,order=c(1,0,1))
ar1
fitted(ar1)
plot(tbill,col="red")
lines(fitted(ar1),col="blue")



ar1lm <- lm(tbill[2:81] ~ tbill[1:80])
summary(ar1lm)
e<-resid(ar1)
#e<-as.timeSeries((e))
plot(e)

acf(e[2:81])

p<-predict(ar1,n.ahead=10)
summary(p)

armam<-arima(tbill,order=c(5,0,0))
armam
e<-resid(armam)
ts.plot(e)
acf(e)
pacf(tbill)

fcast<-p$pred
upper<-p$pred+2*p$se
lower<-p$pred-2*p$se

ts.plot(cbind(fcast,upper,lower))

arma<-arima(tbill,order=c(1,0,1))
e2<-resid(arma)
acf(e2[2:81])

#ADF Test
adf.test(tbill)