

#### ALL CAPS DEC 2002- JANUARY 2020, to compare with SMALL CAP DATA ####
attach(OMXSPI_ALL_FIRMS_2002_2020)
lnPriceAC=log(Price[1:206])
lnPriceAC
plot(lnPriceAC, type = 'l')

#### ACF and PACF of LnpriceAC ####
acf(lnPriceAC, lag.max = 50)
pacf(lnPriceAC, lag.max = 50)

#### we differentiate lnPrice for stationarity , 1 time ####
difflnPriceAC=diff(lnPriceAC,1)
difflnPriceAC
plot(difflnPriceAC,type = 'l')
skewness(lnPriceAC)
kurtosis(lnPriceAC)
skewness(difflnPriceAC)
kurtosis(difflnPriceAC)

acf(difflnPriceAC, lag.max = 50)
pacf(difflnPriceAC, lag.max = 50)

#### we test for stationarity with ADT test ####
adf.test(lnPriceAC)
jarque.bera.test(difflnPriceAC)
#### we found for LnPrice is not stionary so we do differentiate ####
adf.test(difflnPriceAC)

#### now that stationarity is achieved, we continue with time series and ARIMA ####
ACarima <-ts(lnPriceAC, start = c(2003,1),end = c(2020,1), frequency = 12)
fitlnPriceAC<-auto.arima(ACarima)
fitlnPriceAC
plot(ACarima, type = 'l')
title('ALL FIRMS 2003-2020')


fitdifflnPriceAC<-auto.arima(difflnPriceAC)
fitdifflnPriceAC
modelAC<-auto.arima(difflnPriceAC, xreg = januaryAC)
modelAC
coeftest(modelAC)
#### now we controll with OLS regression ####

RtAC<- diff(log(Price))
#### Ols regression and Robust standard errors####

ResultAC<- lm(formula = RtAC ~ januaryAC )

summary(ResultAC)

coeftest(ResultAC,vcov = vcovHC(ResultAC, type = "HC0"))





