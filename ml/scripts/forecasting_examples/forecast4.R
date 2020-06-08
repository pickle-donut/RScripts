#############################################
#=============Read in Libraries=============#
#############################################
install.packages("forecast")

options(digits=4)
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
temptable = paste(workingdirectory, '\\CaliforniaHospital_FinancialData.csv', sep="")
origHospitalData = read.table(temptable , sep = ',', header = TRUE)
colnames(origHospitalData)
origHospitalData

# Display column names, display/convert datatypes, and create new dataframe.
names(origHospitalData)		
str(origHospitalData)
origHospitalData$GRIP_TOT = as.numeric(origHospitalData$GRIP_TOT)
origHospitalData$GROP_TOT = as.numeric(origHospitalData$GROP_TOT)
origHospitalData$NET_TOT = as.numeric(origHospitalData$NET_TOT)
origHospitalData$TOT_OP_EXP = as.numeric(origHospitalData$TOT_OP_EXP)
origHospitalData$NONOP_REV = as.numeric(origHospitalData$NONOP_REV)
str(origHospitalData)


#############################################
#====Convert data to time series object=====#
#############################################
NET_TOT_ts = ts(origHospitalData$NET_TOT, start = c(2007,1), end = c(2016,3), frequency = 4)
TOT_OP_EXP_ts = ts(origHospitalData$TOT_OP_EXP, start = c(2007,1), end = c(2016,3), frequency = 4)
NONOP_REV_ts = ts(origHospitalData$NONOP_REV, start = c(2007,1), end = c(2016,3), frequency = 4)

NET_TOT_ts
TOT_OP_EXP_ts
NONOP_REV_ts


#########################################################
#=========Assess Time Series for Stationarity===========#                    
#########################################################
NET_TOT_ts_dc = decompose(NET_TOT_ts)
plot(NET_TOT_ts_dc)

TOT_OP_EXP_ts_dc = decompose(TOT_OP_EXP_ts)
plot(TOT_OP_EXP_ts_dc)

NONOP_REV_ts_dc = decompose(NONOP_REV_ts)
plot(NONOP_REV_ts_dc)

#========================================
# Use regression to assess stationarity 
# by regressing trend onto time
#========================================
# Regression for NET_TOT.
NET_TOT_trendcomp =NET_TOT_ts_dc$trend
NET_TOT_trend_data = data.frame(trend = c(NET_TOT_trendcomp), time = c(time(NET_TOT_trendcomp)))
NET_TOT_trend_reg = lm(NET_TOT_trend_data$trend ~ NET_TOT_trend_data$time)
summary(NET_TOT_trend_reg)

# Regression for TOT_OP_EXP.
TOT_OP_EXP_trendcomp =TOT_OP_EXP_ts_dc$trend
TOT_OP_EXP_trend_data = data.frame(trend = c(TOT_OP_EXP_trendcomp), time = c(time(TOT_OP_EXP_trendcomp)))
TOT_OP_EXP_trend_reg = lm(TOT_OP_EXP_trend_data$trend ~ TOT_OP_EXP_trend_data$time)
summary(TOT_OP_EXP_trend_reg)

# Regression for NONOP_REV.
NONOP_REV_trendcomp =NONOP_REV_ts_dc$trend
NONOP_REV_trend_data = data.frame(trend = c(NONOP_REV_trendcomp), time = c(time(NONOP_REV_trendcomp)))
NONOP_REV_trend_reg = lm(NONOP_REV_trend_data$trend ~ NONOP_REV_trend_data$time)
summary(NONOP_REV_trend_reg)

#=======================================
# Augmented Dickey�Fuller (ADF) t-test
#=======================================
# ADF for NET_TOT.
NET_TOT_ts_trend = NET_TOT_ts - NET_TOT_ts_dc$seasonal
adf.test(NET_TOT_ts_trend, k = 0, alternative = "stationary")

# ADF for TOT_OP_EXP.
TOT_OP_EXP_ts_trend = TOT_OP_EXP_ts - TOT_OP_EXP_ts_dc$seasonal
adf.test(TOT_OP_EXP_ts_trend, k = 0, alternative = "stationary")

# ADF for NONOP_REV.
NONOP_REV_ts_trend = NONOP_REV_ts - NONOP_REV_ts_dc$seasonal
adf.test(NONOP_REV_ts_trend, k = 0, alternative = "stationary")

#================================================
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
#================================================
# KPSS for NET_TOT.
kpss.test(NET_TOT_ts_trend)

# KPSS for TOT_OP_EXP.
kpss.test(TOT_OP_EXP_ts_trend)

# KPSS for NONOP_REV.
kpss.test(NONOP_REV_ts_trend)


#########################################################
#================Assess Autocorrelation=================#
#########################################################
# Autocorrelation function for NET_TOT with seasonal and trend affects.
acf(NET_TOT_ts)
adf.test(NET_TOT_ts, k = 0, alternative = "stationary")
kpss.test(NET_TOT_ts)
# Trend; no oscillations exhibiting seasonality present.

# Autocorrelation function for TOT_OP_EXP with seasonal and trend affects.
acf(TOT_OP_EXP_ts)
adf.test(TOT_OP_EXP_ts, k = 0, alternative = "stationary")
kpss.test(TOT_OP_EXP_ts)
# Trend; no oscillations exhibiting seasonality present.

# Autocorrelation function for NONOP_REV with seasonal and trend affects.
acf(NONOP_REV_ts)
adf.test(NONOP_REV_ts, k = 0, alternative = "stationary")
kpss.test(NONOP_REV_ts)
# Trend and oscillations exhibiting seasonality present.

# Autocorrelation without seasonal component.
acf(NONOP_REV_ts_trend, lag.max = 20)
plot(NONOP_REV_ts_trend)
# Trend and oscillations still present.


#########################################################
#================Determine Differencing=================#
#########################################################
# Differencing for trend component in NET_TOT.
NET_TOT_ts_diff1 = diff(NET_TOT_ts, differences = 1)
plot(NET_TOT_ts_diff1)
adf.test(NET_TOT_ts_diff1, k = 0, alternative = "stationary")
kpss.test(NET_TOT_ts_diff1)
#### Result: ADF: Stationary; KPSS: Stationary

# Differencing for trend component in TOT_OP_EXP.
TOT_OP_EXP_ts_diff1 = diff(TOT_OP_EXP_ts, differences = 1)
plot(TOT_OP_EXP_ts_diff1)
adf.test(TOT_OP_EXP_ts_diff1, k = 0, alternative = "stationary")
kpss.test(TOT_OP_EXP_ts_diff1)
#### Result: ADF: Stationary; KPSS: Stationary

# Differencing for trend component in NONOP_REV.
NONOP_REV_ts_diff1 = diff(NONOP_REV_ts_trend, differences = 1)
plot(NONOP_REV_ts_diff1)
adf.test(NONOP_REV_ts_diff1, k = 0, alternative = "stationary")
kpss.test(NONOP_REV_ts_diff1)
#### Result: ADF: Stationary; KPSS: Stationary

#===========================
# Compare ACF correlograms.
#===========================
layout(1:2)
acf(NET_TOT_ts)
acf(NET_TOT_ts_diff1)

layout(1:2)
acf(TOT_OP_EXP_ts)
acf(TOT_OP_EXP_ts_diff1)

layout(1:2)
acf(NONOP_REV_ts_trend, lag.max = 20)
acf(NONOP_REV_ts_diff1)


#########################################################
#==================Build ARMA Models====================#
#########################################################
#====================================
# Compare ACF and PACF Correlograms.
#====================================
layout(1:2)
pacf(NET_TOT_ts_diff1)
acf(NET_TOT_ts_diff1)

layout(1:2)
pacf(TOT_OP_EXP_ts_diff1)
acf(TOT_OP_EXP_ts_diff1)

layout(1:2)
pacf(NONOP_REV_ts_diff1)
acf(NONOP_REV_ts_diff1)

       



