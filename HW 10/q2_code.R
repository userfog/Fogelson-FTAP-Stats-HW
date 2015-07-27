# Question 2
ge = read.csv("GE_daily.csv")
ge = ge[nrow(ge):1,]
ge$returns = c(0,diff(log(ge$Adj.Close)))
amzn = read.csv("AMZN_daily.csv")
amzn = amzn[nrow(amzn):1,]
amzn$returns = c(0,diff(log(amzn$Adj.Close)))
sp500 = read.csv("S&P500_daily.csv")
sp500 = sp500[nrow(sp500):1,]
sp500$returns = c(0,diff(log(amzn$Adj.Close)))
ibm = read.csv("IBM_daily.csv")
ibm = ibm[nrow(ibm):1,]
ibm$returns = c(0,diff(log(ibm$Adj.Close)))
yhoo = read.csv("YHOO_daily.csv")
yhoo = yhoo[nrow(yhoo):1,]
yhoo$returns = c(0,diff(log(yhoo$Adj.Close)))
qcom = read.csv("QCOM_daily.csv")
qcom = qcom[nrow(qcom):1,]
qcom$returns = c(0,diff(log(qcom$Adj.Close)))

lm.ge = lm(ge$returns ~ sp500$returns)
lm.amzn = lm(amzn$returns ~ sp500$returns)
lm.ibm = lm(ibm$returns ~ sp500$returns)
lm.yhoo = lm(yhoo$returns ~ sp500$returns)
lm.qcom = lm(qcom$returns ~ sp500$returns)

varF = var(sp500$returns)
betas = c(lm.ge$coefficients[["sp500$returns"]],lm.amzn$coefficients[["sp500$returns"]],
          lm.ibm$coefficients[["sp500$returns"]],lm.yhoo$coefficients[["sp500$returns"]],lm.qcom$coefficients[["sp500$returns"]])
idioVar = c(var(lm.ge$residuals),var(lm.amzn$residuals),var(lm.ibm$residuals),var(lm.yhoo$residuals),var(lm.qcom$residuals))
idioVar = diag(5)*idioVar
omega = outer(betas,betas)*varF + idioVar