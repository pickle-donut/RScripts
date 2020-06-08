#############################################
#=============Read in Libraries=============#
#############################################
install.packages("tseries")
install.packages("forecast")
install.packages("lmtest")

options(digits = 4) #Useability by way of rounding
library(tseries)
library(forecast)
library(lmtest)


#########################################################
#==============Setup the Working Directory==============#
#########################################################
workingdirectory = "C:\\Users\\jonmc\\Documents\\git\\RScripts\\ml\\data\\general\\tsData"
setwd(workingdirectory)


#############################################
#===============Read in data================#
#############################################
temptable = paste(workingdirectory, "\\global.txt", sep = "")
global_data = read.table(temptable, sep = '\t', header = TRUE)
colnames(global_data)
global_month_ts = ts(global_data$globe_data, start = c(1856, 1), freq = 12)

global_month_ts
plot(global_month_ts)


#########################################################
#=========Assess Time Series for Stationarity===========#                    
# 1) Compare means of trend-model to non-trend-model    #
# 2) Augmented Dickey�Fuller (ADF) t-test               #
#########################################################
global_month_ts_dc = decompose(global_month_ts)
plot(global_month_ts_dc)


#Remove season; assess mean for trend data
global_ts_trend = global_month_ts - global_month_ts_dc$seasonal
mean(global_ts_trend)
var(global_ts_trend)

#Remove trend and season; assess mean for data without trend
global_ts_rand = global_month_ts - global_month_ts_dc$seasonal - global_month_ts_dc$trend
mean(na.omit(global_ts_rand))
var(na.omit(global_ts_rand))

layout(1:2)
plot(global_ts_trend)
plot(global_ts_rand)

#ADF T-Test
kpss.test(global_month_ts)


#########################################################
#================Assess Autocorrelation=================#
#########################################################
#Autocorrelation assessment on entire time series.
#Decreasing trend, no seasonality.
acf(global_month_ts)

#Stationarity assessment.
adf.test(global_month_ts, k = 20, alternative = "stationary")
kpss.test(global_month_ts)

#Trend confirmed.
acf(global_ts_trend, lag.max = 20)

#No considerable difference in plots with seasonal component removed.
layout(1:2)
plot(global_ts_trend)
plot(global_month_ts)


#########################################################
#================Remove Trend Component=================#
#########################################################
#Remove any trend components.
global_ts_diff1 = diff(global_ts_trend, differences = 1)
plot(global_ts_diff1)

#Stationarity achieved; ADF is significant, KPSS test is non-significant.
adf.test(global_ts_diff1, k = 20, alternative = "stationary")
kpss.test(global_ts_diff1)

layout(1:2)
acf(global_ts_diff1, lag.max = 20)
pacf(global_ts_diff1, lag.max = 20)


#########################################################
#===============Estimation of Parameters================#
#########################################################
#========================
# Obtain AIC for Models
#========================
global_arima1 = arima(global_ts_diff1, order = c(1, 1, 1), method = "ML")
global_arima1

global_arima2 = arima(global_ts_diff1, order = c(2, 1, 0), method = "ML")
global_arima2

global_arima3 = arima(global_ts_diff1, order = c(2, 1, 1), method = "ML")
global_arima3


#============================
# Obtain p-values for Models
#============================
coeftest(global_arima1)
coeftest(global_arima3)


#========================
# Obtain BIC for Models
#========================
global_arima1_bic = AIC(global_arima1, k = log(length(global_ts_diff1)))
global_arima2_bic = AIC(global_arima2, k = log(length(global_ts_diff1)))
global_arima3_bic = AIC(global_arima3, k = log(length(global_ts_diff1)))


global_arima1_bic
global_arima2_bic
global_arima3_bic


#########################################################
#====================Forecast Errors====================#
#########################################################
#ARIMA(1,1,1)
global_arima1_fore = forecast(global_arima1, h = 20)

#ARIMA(2,1,0)
global_arima2_fore = forecast(global_arima2, h = 20)

#ARIMA(2,1,1)
global_arima3_fore = forecast(global_arima3, h = 20)

accuracy(global_arima1_fore)
accuracy(global_arima2_fore)
accuracy(global_arima3_fore)
