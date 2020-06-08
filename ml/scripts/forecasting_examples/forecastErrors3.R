#############################################
#=============Read in Libraries=============#
# Read in the necessary libraries. Note,    #
# the commented code for installing		    #
# packages. Remove the "#" and run the	    #
# script to install them. You must be	    #
# connected to the internet to download	    #
# them.						                #
#############################################

options(digits = 4) #Useability by way of rounding
library(tseries)
library(forecast)
library(lmtest)


#########################################################
#==============Setup the Working Directory==============#
# Set the working directory to the project folder by    #
# running the appropriate script below. Note, you can 	#
# run the data off of your OneDrive or DropBox.		    #
#########################################################

workingdirectory = "C:\\Users\\jonmc\\Documents\\git\\RScripts\\ml\\data\\general\\tsData"
setwd(workingdirectory)


#############################################
#===============Read in data================#
# Read in the data.	                        #
#############################################

#NYC Birth Data
nycbirths_data = read.table('NYC_births.txt', sep = '\t', header = TRUE)
births_ts = ts(nycbirths_data, frequency = 12, start = 1946)

births_ts

plot(births_ts)

#Unemployment data from Maine 1996 to August 2006
temptable = paste(workingdirectory, "\\Maine.dat", sep = "")
maine_data = read.table(temptable, sep = '\t', header = TRUE)
maine_month_ts = ts(maine_data$unemploy, start = c(1996, 1), freq = 12)


#########################################################
#=========Assess Time Series for Stationarity===========#
# 1) Determine if the trend component exists by         #
#    regressing trend onto time.                        #
# 2) Compare means of trend-model to non-trend-model    #
# 3) Augmented Dickey�Fuller (ADF) t-test               #
# 4) Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test      #
#########################################################

births_ts_dc = decompose(births_ts)

plot(births_ts_dc)

#========================================
# Use regression to assess stationarity 
# by regressing trend onto time
#========================================
births_trendcomp = births_ts_dc$trend

births_trend_data = data.frame(trend = c(births_trendcomp), time = c(time(births_trendcomp)))

births_trend_reg = lm(births_trend_data$trend ~ births_trend_data$time)
summary(births_trend_reg)

#=======================================
# Compare the means of the time series
# with and without the trend
#=======================================
#Remove season; assess mean for trend data
births_ts_trend = births_ts - births_ts_dc$seasonal
mean(births_ts_trend)
var(births_ts_trend)

#Remove trend and season; assess mean for data without trend
births_ts_rand = births_ts - births_ts_dc$seasonal - births_ts_dc$trend
mean(na.omit(births_ts_rand))
var(na.omit(births_ts_rand))

#=======================================
# Augmented Dickey�Fuller (ADF) t-test
#=======================================
adf.test(births_ts_trend, k = 20, alternative = "stationary")
#### Result: Not significant, indicating non-stationarity

#================================================
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
#================================================
kpss.test(births_ts_trend)
#### Result: Significant, indicating non-stationarity


#########################################################
#================Assess Autocorrelation=================#
# 1) Autocorrelation Function                           #
# 2) Partial Correlation Function                       #
#########################################################

#=================================================
# Autocorrelation Function for Unemployment data
# with seasonal and trend effects
#=================================================

acf(maine_month_ts)
#Seasonal effects due to the ocillations shown in the plot

adf.test(maine_month_ts, k = 20, alternative = "stationary")
kpss.test(maine_month_ts)
#### Result: Not stationary

#=================================================
# Autocorrelation Function for Unemployment data
# with trend effects
#=================================================

#Model without the seasonal component
maine_ts_dc = decompose(maine_month_ts)
maine_ts_trend = maine_month_ts - maine_ts_dc$seasonal

acf(maine_ts_trend, lag.max = 20)
#Exhibits trend because the values start out high and
#steadily decrease, but never truly reach zero

plot(maine_ts_trend)

#=================================================
# Autocorrelation Function for Unemployment data
# with no seasonal or trend effects
#=================================================
maine_ts_diff1 = diff(maine_ts_trend, differences = 1)
plot(maine_ts_diff1)

adf.test(maine_ts_diff1, k = 20, alternative = "stationary")
kpss.test(maine_ts_diff1)
#### Result: ADF: Non-stationary; KPSS: Stationary

#Use differencing of 2 to further eliminate trend effect
maine_ts_diff2 = diff(maine_ts_trend, differences = 2)
plot(maine_ts_diff2)

adf.test(maine_ts_diff2, k = 20, alternative = "stationary")
kpss.test(maine_ts_diff2)
#### Result: Stationary

acf(maine_ts_diff2, lag.max = 20)
#Exhibits stationarity because short-term correlations are shown
#(usually only the first 2 to 3 extend beyond the 95% interval), 
#then drop off to zero for the remainder

#================================================
# Partial Correlation Function for Unemployment
# data with 2 differences
#================================================
pacf(maine_ts_diff2, lag.max = 20)

#==================================================
# Simulation of ARIMA(2, 0, 0) for 50 data points
#==================================================
sim_ts1 = arima.sim(model = list(ar = c(0.9, -0.2)), n = 50)

plot(sim_ts1, main="Simulation 1: Plot")
acf(sim_ts1, lag.max = 20, main="Simulation 1: ACF")
pacf(sim_ts1, lag.max=20, main = "Simulation 1: PACF")

#==================================================
# Simulation of ARIMA(0, 0, 2) for 50 data points
#==================================================
sim_ts2 = arima.sim(model = list(ma = c(0.7, -0.4)), n = 50)

plot(sim_ts2, main = "Simulation 2: Plot")
acf(sim_ts2, lag.max = 20, main = "Simulation 2: ACF")
pacf(sim_ts2, lag.max = 20, main = "Simulation 2: PACF")

#==================================================
# Simulation of ARIMA(2, 0, 2) for 50 data points
#==================================================
sim_ts3 = arima.sim(model = list(ar = c(0.9, -0.2), ma = c(0.7, -0.4)), n = 50)

plot(sim_ts3, main = "Simulation 3: Plot")
acf(sim_ts3, lag.max = 20, main = "Simulation 3: ACF")
pacf(sim_ts3, lag.max = 20, main = "Simulation 3: PACF")


#########################################################
#===============Estimation of Parameters================#
# Use Maximum Likelihood to estimate the parameters of  #
# the model and assess the p-values.                    #
#########################################################

maine_arima1 = arima(maine_ts_diff2, order = c(1, 0, 1), method = "ML")
maine_arima1
#### Result: AIC -69.90

maine_arima2 = arima(maine_ts_diff2, order = c(2, 0, 0), method = "ML")
maine_arima2
#### Result: AIC -50.04

maine_arima3 = arima(maine_ts_diff2, order = c(2, 0, 1), method = "ML")
maine_arima3
#### Result: AIC -72.00

maine_arima4 = arima(maine_ts_diff2, order = c(3, 0, 1), method = "ML")
maine_arima4
#### Result: AIC -70.55

coeftest(maine_arima3)

acf(maine_arima3)

#======================================
# Original data prior to differencing
#======================================
maine_arima5 = arima(maine_ts_trend, order = c(2, 2, 1), method = "ML")
maine_arima5
#### Result: AIC -72.39

#========================
# Obtain BIC for Models
#========================
maine_arima1_bic = AIC(maine_arima1, k = log(length(maine_ts_diff2)))
maine_arima2_bic = AIC(maine_arima2, k = log(length(maine_ts_diff2)))
maine_arima3_bic = AIC(maine_arima3, k = log(length(maine_ts_diff2)))
maine_arima4_bic = AIC(maine_arima4, k = log(length(maine_ts_diff2)))

maine_arima1_bic
maine_arima2_bic
maine_arima3_bic
maine_arima4_bic


#########################################################
#====================Forecast Errors====================#
# 1) Mean Forecast Error (MFE)                          #
# 2) Mean Absolute Error (MAE)                          #
# 3) Root Mean-Squared Error (RMSE)                     #
# 4) Mean Absolute Percent Error (MAPE)                 #
#########################################################

# ARMA(1,1)
maine_arima1_fore = forecast(maine_arima1, h = 20)

# AR(2)
maine_arima2_fore = forecast(maine_arima2, h = 20)

# ARMA(2, 1)
maine_arima3_fore = forecast(maine_arima3, h = 20)

# ARMA(3, 1)
maine_arima4_fore = forecast(maine_arima4, h = 20)

accuracy(maine_arima1_fore)
accuracy(maine_arima2_fore)
accuracy(maine_arima3_fore)
accuracy(maine_arima4_fore)