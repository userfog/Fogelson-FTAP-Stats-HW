data<-read.csv("c:/citadel2015/data/sp500_daily.csv")
require(fGarch)
require(timeSeries)
require(tseries)

r<-data$ret
date <- as.Date(data$Date, format="%m/%d/%Y")
r2=252*r*r
ar=sqrt(252)*r
plot(date,r,type="l")

plot(date,sqrt(r2),type="l")

nrow(data)
hist(r)
skew=mean(r^3)/(mean(r^2)^(3/2))
skew

kurt=mean(r^4)/(mean(r^2)^2)
kurt

#summary stats
acf(r2,400)

absr=abs(r)
acf(absr,400)

#MA
ma <- sqrt(filter(r2,rep(1/100,100), sides=1))

plot(date,abs(ar),type="l")
points(date,ma,type="l",col="red")

#exp_smoother
rmvar <- matrix(0,nrow=length(ar),ncol=1)
rmvar[1,1] <- var(ar)
for (i in 2:length(rmvar))
rmvar[i,1] <- (0.97*rmvar[i-1,1])+(0.03*ar[i-1]^2)
expsmooth<-sqrt(rmvar)
plot(date,abs(ar),type="l", col="grey")
points(date,ma,type="l",col="red")
points(date,expsmooth,type="l",col="blue")

#GARCH

garch.model <- garchFit(~garch(2,2),data=ar)
summary(garch.model)
garchvol<-volatility(garch.model)
garchres<-residuals(garch.model)
tail(garchvol,n=1)
plot(date,abs(ar),type="l", col="grey")
points(date,ma,type="l",col="red")
points(date,expsmooth,type="l",col="blue")
points(date,garchvol,type="l",col="green")

stdres<-garchres/garchvol
acf(stdres)
stdres2=stdres^2
acf(stdres2)
hist(stdres)
skew=mean(stdres^3)/(mean(stdres^2)^(3/2))
skew

kurt=mean(stdres^4)/(mean(stdres^2)^2)
kurt


plot(date,abs(ar),type="l", col="grey")
points(date,ma,type="l",col="red")
points(date,expsmooth,type="l",col="blue")
points(date,garchvol,type="l",col="green")


#predict garch
forecast<-predict(garch.model,n.ahead=500)
ts.plot(forecast$standardDeviation,type="l")
forecast<-predict(garch.model,n.ahead=1)
forecast
cc<-qnorm(.010,0,1)
cc
valrisk_norm<-cc*(.1489/sqrt(252))
valrisk_norm
#GARCH with t

tgarch.1.model <- garchFit(~garch(1,1),data=ar, cond.dist=c("std"))
summary(tgarch.1.model)
p<-coef(tgarch.1.model)

t_q<-qt(.01,6.82)/sqrt(p[5]/(p[5]-2))
t_q
valrisk_t<-t_q*(.1489/sqrt(252))
valrisk_t

#Semi parametric
qq=quantile(stdres,.01)
qq
valrisk_sp<-qq*(.1489/sqrt(252))
valrisk_sp





