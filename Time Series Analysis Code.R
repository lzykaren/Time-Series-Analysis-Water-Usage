# Time Series Analysis Water Usage
# Ziyan Lin

library(TSA)

###
source(file = "H:\\Documents\\6560 Applied Time Series Analysis\\Project\\Water\\arimax-txt2.R")
###

# Read data
MyData<- read.csv(file="H:\\Documents\\6560 Applied Time Series Analysis\\Project\\Water\\water.csv", header=TRUE, sep=",")
water = ts(MyData$WaterUsage, start = 1967-01, frequency = 12)

# Plot time series plot: Monthly Water usage in London
plot(water, type = "b", main='Monthly water usage in London from 1966 to 1988')


# Plot with Monthly Symbols
plot(window(water,start=c(1966,1)),ylab='Monthly Water Usage(mL/day)', main='Fig 1 Monthly water usage in London from 1966 to 1988')
month = c('J','F','M','A','M','J','J','A','S','O','N','D')
points(window(water,start=c(1966,1)),pch=month)

plot(window(water,start=c(1980,1)),ylab='Monthly Water Usage(mL/day)', main='Fig 2 Monthly water usage in London from 1980 to 1988')
month = c('J','F','M','A','M','J','J','A','S','O','N','D')
points(window(water,start=c(1980,1)),pch=month)



# Power Transformations
title(main = "Fig 3 Power Transformation")
BoxCox.ar(water)
Water = 1/(water^(4/5))
Water=1/water
BoxCox.ar(Water)


# Sample ACF
acf(as.vector(water), lag.max = 100)
title(main = "Fig 4 Sample ACF on Reciprocal Transformation")
acf(as.vector(Water), lag.max = 100, main = "Fig 4 Sample ACF on Reciprocal Transformation")


# Time Series Plot of the First Differences of Water Levels
plot(diff(water),ylab='First Difference of Water',xlab='Time')
plot(diff(Water),ylab='First Difference of Water',xlab='Time', main = "Fig 5 Time Series Plot of the First Differences of Monthly Water Usage")


# Sample ACF of First Differences of Water Levels
acf(as.vector(diff(water)), lag.max = 100)
acf(as.vector(diff(Water)), lag.max = 100, main = "Fig 6 Sample ACF of First Differences of Monthly Water Usage")



# Time Series Plot of First and Seasonal Differences of Water
plot(diff(diff(water),lag=12),xlab='Time',ylab='First and Seasonal Difference of Water')
plot(diff(diff(Water),lag=12),xlab='Time',ylab='First and Seasonal Difference of Monthly Water Usage', main = 'Fig 7 Time Series Plot of First and Seasonal Differences of Monthly Water Usage')


# Sample ACF of First and Seasonal Differences of Water
acf(as.vector(diff(diff(water),lag=12)),lag.max=36,ci.type='ma')
acf(as.vector(diff(diff(Water),lag=12)),lag.max=60,ci.type='ma', main='Fig 8 Sample ACF of First and Seasonal Differences of Monthly Water Usage')
pacf(as.vector(diff(diff(Water),lag=12)),lag.max=60)



# Model Fitting
model=arima(Water,order=c(0,1,4),seasonal=list(order=c(0,1,1), period=12))
model


# Diagnostic Checking
# Residuals from the ARIMA(0,1,1)?0,1,1)12 Model
plot(window(rstandard(model)),ylab='Standardized Residuals',type='o')
abline(h=0)


# ACF of Residuals from the ARIMA(0,1,1)?0,1,1)12 Model
acf(as.vector(window(rstandard(model))), lag.max=36)


# Histogram
hist(window(rstandard(model)),xlab='Standardized Residuals')


# Normal QQ Plot
qqnorm(window(rstandard(model)))
qqline(window(rstandard(model)))


# Three of our diagnostic tools
tsdiag(model)
title(main = 'Fig 10 Diagnostic Display for ARIMA(0,1,4)*(0,1,1)')
detectAO(model)
model=arima(Water,order=c(0,1,4),seasonal=list(order=c(0,1,1),period=12))
detectIO(model2)



# New model
model2=arima(Water,order=c(0,1,4),seasonal=list(order=c(0,1,1),period=12),io=c(19,20, 44,55,56, 80, 127,151,198))
model2
plot(window(rstandard(model2)),ylab='Standardized Residuals',type='o')
abline(h=0)
acf(as.vector(window(rstandard(model2))), lag.max=36)
hist(window(rstandard(model2)),xlab='Standardized Residuals')
qqnorm(window(rstandard(model2)))
qqline(window(rstandard(model2)))
tsdiag(model2,omit.initial=F)

detectAO(model2)
detectIO(model2)
plot(model ,n1=c(1966,1),n.ahead=48,xlab='Year',type='b',ylab='Water Usage')





model=arima(Water,order=c(0,1,4),seasonal=list(order=c(0,1,1), period=12))

xreg=data.frame(IO.19=seq(Water)==19,IO.44=seq(Water)==44,IO.56=seq(Water)==56,IO.55=seq(Water)==55,IO.80=seq(Water)==80,IO.127=seq(Water)==127,IO.151=seq(Water)==151,IO.198=seq(Water)==198)
n.ahead=48
plot(model,xreg=data.frame(IO.19=rep(0,n.ahead),IO.44=rep(0,n.ahead),IO.55=rep(0,n.ahead),IO.56=rep(0,n.ahead),IO.80=rep(0,n.ahead),IO.127=rep(0,n.ahead),IO.151=rep(0,n.ahead),IO.198=rep(0,n.ahead)),n.ahead=n.ahead, transf=, xlab='Year',type='b',ylab='Water Usage')
title(main = "Fig 16 Forecasts and Forecast Limits for the Monthly Water Usage")
plot(model,xreg=xreg,n.ahead=n.ahead)




xreg=data.frame(IO.19=seq(water)==19)
xreg
n.ahead=24
plot(model,newxreg=data.frame(IO.19=rep(0,n.ahead)),n.ahead=n.ahead)








model3 = arima(Water, order=c(0,1,4), seasonal=list(order=c(0,1,1), period=12),fixed = c(NA, NA, 0, rep(NA, 10)),io=c(19,44,55,56,80,127,151,198))
detectAO(model3)
detectIO(model3)
tsdiag(model3)
title(main = 'Fig 13 Diagnostic Display for ARIMA(0,1,4)*(0,1,1) with io=19,44,55,56,80,127,151,198 and ma3 = 0')
model3

model4 = arimax(Water,order=c(1,1,2),seasonal=list(order=c(0,1,1),period=12),io=c(19,127,198))
detectIO(model4)