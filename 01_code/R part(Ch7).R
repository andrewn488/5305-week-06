# Part I: simulation AR timeseriese with and without seasonality
y=numeric(200) #generate a vector of 200 zeros
y[2:200]=arima.sim(n=199,list(ar= .4),innov=rnorm(199,1,.5)) #generate AR(1) (leave the first obs as zero)
plot.ts(y)

y=numeric(200) #generate a vector of 200 zeros
y[3:200]=arima.sim(n=198, list(ar=c(1, -.5)),innov=rnorm(198,1,1)) 
plot.ts(y)

y=numeric(84) #generate a vector of 84 zeros
y[5:84]=arima.sim(n=80, list(ar=c(0,0,0,.8)),innov=rnorm(80,0,.5)) 
plot.ts(y)

y2=numeric(84) #generate a vector of 84 zeros
y2[9:84]=arima.sim(n=76, list(ar=c(0,0,0,.5,0,0,0,.3)),innov=rnorm(76,0,.5)) 
plot.ts(y2)

y3=2+arima.sim(n=84, list(ma=c(0,0,0,.9)),innov=rnorm(84,0,.5)) #generate S-MA(1)
plot.ts(y3)

y4=2+arima.sim(n=84, list(ma=c(0,0,0,-1,0,0,0,.25)),innov=rnorm(84,0,.5)) #generate S-MA(2)
plot.ts(y4)


# Part II: estimation 

library(readxl)
data<-read_excel("Figure7_11_inflation.xls")
CPI_GR<-ts(data$CPI_Growth, frequency=1, start=c(1913)) #declare CPI_GR as (yearly) time series (1913-2016)
plot(CPI_GR)

CPI_GRr<-ts(data$CPI_Growth[1:85], frequency=1, start=c(1913)) #restrict the sample of CPI_GR up to 90th obs (i.e. 2003 year)
plot(CPI_GRr)

model<-arima(CPI_GRr, order=c(2,0,0)) #in-sample AR(2) estimation (1913-2003)
model #see the estimation output

# Part III: Forecasting
library(forecast) #use package forecast
fCPI_GRr<-forecast(model, h=6) #dynamic forecast for 6 steps ahead
summary(fCPI_GRr) #forecast summary
plot(fCPI_GRr, include=10) #plot of the forecast
lines(CPI_GR, col="red") #add the actual series to the plot (in magenta color)


# Part IV: estimation of timeseries with seasonality
data<-read_excel("Figure7_19_constructionchanges.xls")
change_CONST<-ts(data$ConstructionChanges, frequency=12, start=c(2002,1)) #declare change_CONST as (monthly) time series
plot(change_CONST)
model<-arima(change_CONST, order=c(1,0,0), seasonal=list(order=c(1,0,0), period=12)) #AR(1)&S-AR(1) estimation
model #see the estimation output
AIC(model) #Akaike IC
BIC(model) #Bayesian IC

# Part V: Forecasting of timeseries with seasonality
library(forecast) #use package forecast
fchange_CONST<-forecast(model, h=12) #dynamic forecast for 12 steps ahead
summary(fchange_CONST) #forecast summary
plot(fchange_CONST, include =24) #plot of the forecast
