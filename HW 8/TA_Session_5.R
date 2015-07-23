
########################################################
# Ben Charoenwong
# TA_Session_5.R
# Description: tseries and forecast package
# Code mainly from: http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
########################################################

## Forecasting Example with "forecast" Library
# Maybe a good idea to work at home. Not sure if you have this library
# Exponential Smoother (HoltWinters)
library(forecast);
rain = scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1);

# Set up time series. I'm not a big fan of this though.

rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries,main=paste("Rainfall Data from 1813 to ",1813+length(rain),sep=""),ylab="Rainfall (inches)");
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts = HoltWinters(rainseries);

plot(rainseriesforecasts);

rainseriesforecasts2 = forecast.HoltWinters(rainseriesforecasts, h=8)
plot.forecast(rainseriesforecasts2,ylab="Rainfall (inches)");

# Seasonality
births = scan("http://robjhyndman.com/tsdldata/data/nybirths.dat");
birthstimeseries = ts(births, frequency=12, start=c(1946,1));
ts.plot(birthstimeseries,main="Birthrate Across time");

l=length(births);
l=12;

birthstimeseries_yearly = ts(births[seq(from=1,to=length(births),by=12)], frequency=12, start=c(1946,1));
plot.ts(birthstimeseries_yearly,main="Birthrate Across time");
# Note: plot.ts or ts.plot are the same! :O!


## Fit a GARCH model
library(fGarch);

temp=garchFit(formula = ~ garch(1, 1),data=birthstimeseries);
# Feed in a time series type data.






