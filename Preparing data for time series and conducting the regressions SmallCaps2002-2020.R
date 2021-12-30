attach(OMXSPI_SMALL_CAP_DEC_2002_JAN_2020)
library(MASS)
library(tseries)
library(forecast)
library(dynlm)


lnCLose=log(Close[1:206])
lnCLose
plot(lnCLose,type='l')


####ACF and PACF tests to ln format ####
acf(lnCLose,lag.max = 50)
pacf(lnCLose,lag.max = 50)
adf.test(lnCLose)


difflnClose=diff(lnCLose,1)
difflnClose
plot(difflnClose,type = 'l')


hist(difflnClose, prob = TRUE)

curve(dnorm(x,mean(difflnClose),sd(difflnClose)),add = TRUE, col="darkred")
skewness(difflnClose)
kurtosis(difflnClose)


acf(difflnClose,lag.max = 50)
pacf(difflnClose,lag.max = 50)

adf.test(difflnClose)
jarque.bera.test(difflnClose)



Closearima<-ts(lnCLose,start = c(2003,1), end = c(2020,1), frequency = 12)
fitClosearima<-auto.arima(difflnClose)
fitClosearima




auto.arima(difflnClose, xreg = januarSC)

library(moments)



