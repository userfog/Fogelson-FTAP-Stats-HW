
########################################################
# Ben Charoenwong
# TA_Session_4.R
# Description: PCA
########################################################

getwd();
setwd("/Users/bencharoenwong/Dropbox/Citadel/2015/TA Session 4/");
getwd();

## PCA
# PCA on the X's usually. But can be on all the data
pca=princomp(data);
names(pca);
plot(pca$sdev,main="Scree Plot",xlab="Factors",ylab="SD",type="l",col="red");
# But that's somewhat meaningless. Let's look at proportion of SD explained

plot(pca$sdev/sum(pca$sdev),main="Scree Plot",xlab="Factors",ylab="% of SD Explained",type="l",col="blue");

dim(data);
dim(pca$loadings);
factors=as.matrix(data)%*%as.matrix(pca$loadings);

# Check:
sd_gen=sqrt(apply(factors,2,var));
par(mfrow=c(1,2));
plot(sd_gen,main="Scree Plot-Generated",xlab="Factors",ylab="SD Explained",type="l",col="red");
plot(pca$sdev,main="Scree Plot-princomp",xlab="Factors",ylab="SD Explained",type="l",col="blue");

## Time Series

genAR_1 = function(beta0,beta1,n,sigma) {

true_mean= beta0/(1-beta1);
eps=rnorm(n)*sigma;
y=matrix(NA,nrow=n,ncol=1);
y[1]=eps[1];
for (i in 2:n) {	y[i]=beta0+beta1*y[i-1]+eps[i];};

return(y)
}

q1=matrix(NA,nrow=200,ncol=5);

q1[,1]=genAR_1(0,0,200,5);

par(mfrow=c(2,1));

pdf("out.pdf");
plot(q1[,1],type='l');
dev.off();

# Example of ACF and PACF, and estimating AR(1) and MA(1)

data = read.table("TB3MS.txt", sep = ",");
names(data) = c("date", "rf");
data$date = as.Date(data$date);

pdf("out.pdf");
plot(data$date, data$rf, main = "3 Month T-Bills", ylab = "%", xlab = "time", type = "l");
dev.off();

pdf("out1.pdf");
acf(data$rf);
dev.off();

pdf("out.pdf");
acf(data$rf,type = "partial");
dev.off();

fit1 = arima(data$rf, order = c(3,0,0));
fit2 = arima(data$rf, order = c(0,0,1));