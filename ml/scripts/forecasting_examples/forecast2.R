#############################################
#=============Read in Libraries=============#
#############################################
install.packages("forecast")

options(digits=4)
library(forecast)
library(graphics)


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
GRIP_TOT_ts = ts(origHospitalData$GRIP_TOT, start = c(2007,1), end = c(2016,3), frequency = 4)
GROP_TOT_ts = ts(origHospitalData$GROP_TOT, start = c(2007,1), end = c(2016,3), frequency = 4)
NET_TOT_ts = ts(origHospitalData$NET_TOT, start = c(2007,1), end = c(2016,3), frequency = 4)
TOT_OP_EXP_ts = ts(origHospitalData$TOT_OP_EXP, start = c(2007,1), end = c(2016,3), frequency = 4)
NONOP_REV_ts = ts(origHospitalData$NONOP_REV, start = c(2007,1), end = c(2016,3), frequency = 4)

GRIP_TOT_ts
GROP_TOT_ts
NET_TOT_ts
TOT_OP_EXP_ts
NONOP_REV_ts


#################################################
#=============Descriptive Analysis==============#
#################################################
# Describe GRIP_TOT_ts time series.
start(GRIP_TOT_ts)
end(GRIP_TOT_ts)
frequency(GRIP_TOT_ts)
plot(GRIP_TOT_ts,  ylab="GRIP_TOT")

# Describe GROP_TOT_ts time series.
start(GROP_TOT_ts)
end(GROP_TOT_ts)
frequency(GROP_TOT_ts)
plot(GROP_TOT_ts,  ylab="GROP_TOT_ts")

# Describe NET_TOT_ts time series.
start(NET_TOT_ts)
end(NET_TOT_ts)
frequency(NET_TOT_ts)
plot(NET_TOT_ts,  ylab="NET_TOT_ts")

# Describe TOT_OP_EXP_ts time series.
start(TOT_OP_EXP_ts)
end(TOT_OP_EXP_ts)
frequency(TOT_OP_EXP_ts)
plot(TOT_OP_EXP_ts,  ylab="TOT_OP_EXP_ts")

# Describe NONOP_REV_ts time series.
start(NONOP_REV_ts)
end(NONOP_REV_ts)
frequency(NONOP_REV_ts)
plot(NONOP_REV_ts,  ylab="NONOP_REV_ts")


################################################################
#=============Create Seasonal Assessment Box Plot==============#
################################################################
par(mfrow=c(2,3))
# Create boxplot for GRIP_TOT_ts time series.
boxplot(GRIP_TOT_ts ~ cycle(GRIP_TOT_ts), main = "GRIP_TOT Boxplot")

# Create boxplot for GROP_TOT_ts time series.
boxplot(GROP_TOT_ts ~ cycle(GROP_TOT_ts), main = "GRIP_TOT Boxplot")

# Create boxplot for NET_TOT_ts time series.
boxplot(NET_TOT_ts ~ cycle(NET_TOT_ts), main = "NET_TOT Boxplot")

# Create boxplot for TOT_OP_EXP_ts time series.
boxplot(TOT_OP_EXP_ts ~ cycle(TOT_OP_EXP_ts), main = "TOT_OP_EXP Boxplot")

# Create boxplot for NONOP_REV_ts time series.
boxplot(NONOP_REV_ts ~ cycle(NONOP_REV_ts), main = "NONOP_REV_ts Boxplot")


#################################################
#===========Decompose Time Series===============#
#################################################
# Decompose and plot time series object for GRIP_TOT_ts set.
GRIP_TOT_ts_dc = decompose(GRIP_TOT_ts)
plot(GRIP_TOT_ts_dc)

# Decompose and plot time series object for GROP_TOT_ts set.
GROP_TOT_ts_dc = decompose(GROP_TOT_ts)
plot(GROP_TOT_ts_dc)

# Decompose and plot time series object for NET_TOT_ts set.
NET_TOT_ts_dc = decompose(NET_TOT_ts)
plot(NET_TOT_ts_dc)

# Decompose and plot time series object for TOT_OP_EXP_ts set.
TOT_OP_EXP_ts_dc = decompose(TOT_OP_EXP_ts)
plot(TOT_OP_EXP_ts_dc)

# Decompose and plot time series object for NONOP_REV_ts set.
NONOP_REV_ts_dc = decompose(NONOP_REV_ts)
plot(NONOP_REV_ts_dc)


#############################################################################
#===========Function for Homoscedasticity/Normality Assessment==============#
#############################################################################
plotForecastErrors = function(forecasterrors,forecasttitle) {
    #Function provided by Avril Coghlan
    forecasterrors = na.omit(forecasterrors)
    # make a histogram of the forecast errors:
    mybinsize = IQR(forecasterrors) / 4
    mysd = sd(forecasterrors)
    mymin = min(forecasterrors) - mysd * 5
    mymax = max(forecasterrors) + mysd * 3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(10000, mean = 0, sd = mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins, main=forecasttitle)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}


##############################################################
#===========Holt-Winters Exponential Smoothing===============#
##############################################################
par(mfrow = c(2,3))
#### Pre-determined smoothing parameters.
# Create Holt-Winters for GRIP_TOT_ts set.
GRIP_TOT_ts_es = HoltWinters(GRIP_TOT_ts, alpha = 0.2, beta = 0.2, gamma = 0.7) 
GRIP_TOT_ts_es
plot(GRIP_TOT_ts_es, main = "GRIP_TOT_ts Set")

# Create Holt-Winters for GROP_TOT_ts set.
GROP_TOT_ts_es = HoltWinters(GROP_TOT_ts, alpha = 0.2, beta = 0.2, gamma = 0.7) 
GROP_TOT_ts_es
plot(GRIP_TOT_ts_es, main = "GROP_TOT_ts Set")

# Create Holt-Winters for NET_TOT_ts set.
NET_TOT_ts_es = HoltWinters(NET_TOT_ts, alpha = 0.2, beta = 0.2, gamma = 0.7) 
NET_TOT_ts_es
plot(NET_TOT_ts_es, main = "NET_TOT_ts Set")

# Create Holt-Winters for TOT_OP_EXP_ts set.
TOT_OP_EXP_ts_es = HoltWinters(TOT_OP_EXP_ts, alpha = 0.2, beta = 0.2, gamma = 0.7) 
TOT_OP_EXP_ts_es
plot(TOT_OP_EXP_ts_es, main = "TOT_OP_EXP_ts Set")

# Create Holt-Winters for NONOP_REV_ts set.
NONOP_REV_ts_es = HoltWinters(NONOP_REV_ts, alpha = 0.2, beta = 0.2, gamma = 0.7) 
NONOP_REV_ts_es
plot(NONOP_REV_ts_es, main = "NONOP_REV_ts Set")

par(mfrow = c(2,3))
#### Let R estimate smoothing parameters.
# Create Holt-Winters for GRIP_TOT_ts set.
R_GRIP_TOT_ts_es = HoltWinters(GRIP_TOT_ts) 
R_GRIP_TOT_ts_es
plot(R_GRIP_TOT_ts_es, main = "GRIP_TOT_ts Set") #Exponential Smoothing

# Create Holt-Winters for GROP_TOT_ts set.
R_GROP_TOT_ts_es = HoltWinters(GROP_TOT_ts) 
R_GROP_TOT_ts_es
plot(R_GROP_TOT_ts_es, main = "GROP_TOT_ts Set") #Trend-Adjusted Smoothing

# Create Holt-Winters for NET_TOT_ts set.
R_NET_TOT_ts_es = HoltWinters(NET_TOT_ts) 
R_NET_TOT_ts_es
plot(R_NET_TOT_ts_es, main = "NET_TOT_ts Set") #Exponential Smoothing

# Create Holt-Winters for TOT_OP_EXP_ts set.
R_TOT_OP_EXP_ts_es = HoltWinters(TOT_OP_EXP_ts) 
R_TOT_OP_EXP_ts_es
plot(R_TOT_OP_EXP_ts_es, main = "TOT_OP_EXP_ts Set") #Holt-Winters

# Create Holt-Winters for NONOP_REV_ts set.
R_NONOP_REV_ts_es = HoltWinters(NONOP_REV_ts) 
R_NONOP_REV_ts_es
plot(R_NONOP_REV_ts_es, main = "NONOP_REV_ts Set") #Holt-Winters


#################################################
#===========Assess Models Above=================#
#################################################
#### Predetermined Smoothing.
par(mfrow = c(3,1))
#Create forecast for GRIP_TOT_ts_es.
GRIP_TOT_ts_es_fore = forecast.HoltWinters(GRIP_TOT_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
GRIP_TOT_ts_es_fore
#Look at forecasted values
plot(GRIP_TOT_ts_es_fore)
#Assess constant variance
plot(GRIP_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(GRIP_TOT_ts_es_fore$residuals,'GRIP_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for GROP_TOT_ts_es.
GROP_TOT_ts_es_fore = forecast.HoltWinters(GROP_TOT_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
GROP_TOT_ts_es_fore
#Look at forecasted values
plot(GROP_TOT_ts_es_fore)
#Assess constant variance
plot(GROP_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(GROP_TOT_ts_es_fore$residuals,'GROP_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for NET_TOT_ts_es.
NET_TOT_ts_es_fore = forecast.HoltWinters(NET_TOT_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
NET_TOT_ts_es_fore
#Look at forecasted values
plot(NET_TOT_ts_es_fore)
#Assess constant variance
plot(NET_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(NET_TOT_ts_es_fore$residuals,'NET_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for TOT_OP_EXP_ts_es.
TOT_OP_EXP_ts_es_fore = forecast.HoltWinters(TOT_OP_EXP_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
TOT_OP_EXP_ts_es_fore
#Look at forecasted values
plot(TOT_OP_EXP_ts_es_fore)
#Assess constant variance
plot(TOT_OP_EXP_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(TOT_OP_EXP_ts_es_fore$residuals,'TOT_OP_EXP_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for NONOP_REV_ts_es.
NONOP_REV_ts_es_fore = forecast.HoltWinters(NONOP_REV_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
NONOP_REV_ts_es_fore
#Look at forecasted values
plot(NONOP_REV_ts_es_fore)
#Assess constant variance
plot(NONOP_REV_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(NONOP_REV_ts_es_fore$residuals,'NONOP_REV_ts_es_fore Normality Assessment')




#### R Determined Smoothing.
par(mfrow = c(3,1))
#Create forecast for R_GRIP_TOT_ts_es.
R_GRIP_TOT_ts_es_fore = forecast.HoltWinters(R_GRIP_TOT_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
R_GRIP_TOT_ts_es_fore
#Look at forecasted values
plot(R_GRIP_TOT_ts_es_fore)
#Assess constant variance
plot(R_GRIP_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_GRIP_TOT_ts_es_fore$residuals,'R_GRIP_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for R_GROP_TOT_ts_es.
R_GROP_TOT_ts_es_fore = forecast.HoltWinters(R_GROP_TOT_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
R_GROP_TOT_ts_es_fore
#Look at forecasted values
plot(R_GROP_TOT_ts_es_fore)
#Assess constant variance
plot(R_GROP_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_GROP_TOT_ts_es_fore$residuals,'R_GROP_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for R_NET_TOT_ts_es.
R_NET_TOT_ts_es_fore = forecast.HoltWinters(R_NET_TOT_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
R_NET_TOT_ts_es_fore
#Look at forecasted values
plot(R_NET_TOT_ts_es_fore)
#Assess constant variance
plot(R_NET_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_NET_TOT_ts_es_fore$residuals,'R_NET_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for R_TOT_OP_EXP_ts_es.
R_TOT_OP_EXP_ts_es_fore = forecast.HoltWinters(R_TOT_OP_EXP_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
R_TOT_OP_EXP_ts_es_fore
#Look at forecasted values
plot(R_TOT_OP_EXP_ts_es_fore)
#Assess constant variance
plot(R_TOT_OP_EXP_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_TOT_OP_EXP_ts_es_fore$residuals,'R_TOT_OP_EXP_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for R_NONOP_REV_ts_es.
R_NONOP_REV_ts_es_fore = forecast.HoltWinters(R_NONOP_REV_ts_es, h = 1)
#Forecasts with 80% and 95% intervals
R_NONOP_REV_ts_es_fore
#Look at forecasted values
plot(R_NONOP_REV_ts_es_fore)
#Assess constant variance
plot(R_NONOP_REV_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_NONOP_REV_ts_es_fore$residuals,'R_NONOP_REV_ts_es_fore Normality Assessment')


##############################################################
#===========Holt-Winters Forecast (40 periods)===============#
##############################################################
#### Predetermined Smoothing.
par(mfrow = c(3,1))
#Create forecast for GRIP_TOT_ts_es.
GRIP_TOT_ts_es_fore = forecast.HoltWinters(GRIP_TOT_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
GRIP_TOT_ts_es_fore
#Look at forecasted values
plot(GRIP_TOT_ts_es_fore)
#Assess constant variance
plot(GRIP_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(GRIP_TOT_ts_es_fore$residuals,'GRIP_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for GROP_TOT_ts_es.
GROP_TOT_ts_es_fore = forecast.HoltWinters(GROP_TOT_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
GROP_TOT_ts_es_fore
#Look at forecasted values
plot(GROP_TOT_ts_es_fore)
#Assess constant variance
plot(GROP_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(GROP_TOT_ts_es_fore$residuals,'GROP_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for NET_TOT_ts_es.
NET_TOT_ts_es_fore = forecast.HoltWinters(NET_TOT_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
NET_TOT_ts_es_fore
#Look at forecasted values
plot(NET_TOT_ts_es_fore)
#Assess constant variance
plot(NET_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(NET_TOT_ts_es_fore$residuals,'NET_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for TOT_OP_EXP_ts_es.
TOT_OP_EXP_ts_es_fore = forecast.HoltWinters(TOT_OP_EXP_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
TOT_OP_EXP_ts_es_fore
#Look at forecasted values
plot(TOT_OP_EXP_ts_es_fore)
#Assess constant variance
plot(TOT_OP_EXP_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(TOT_OP_EXP_ts_es_fore$residuals,'TOT_OP_EXP_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for NONOP_REV_ts_es.
NONOP_REV_ts_es_fore = forecast.HoltWinters(NONOP_REV_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
NONOP_REV_ts_es_fore
#Look at forecasted values
plot(NONOP_REV_ts_es_fore)
#Assess constant variance
plot(NONOP_REV_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(NONOP_REV_ts_es_fore$residuals,'NONOP_REV_ts_es_fore Normality Assessment')




#### R Determined Smoothing.
par(mfrow = c(3,1))
#Create forecast for R_GRIP_TOT_ts_es.
R_GRIP_TOT_ts_es_fore = forecast.HoltWinters(R_GRIP_TOT_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
R_GRIP_TOT_ts_es_fore
#Look at forecasted values
plot(R_GRIP_TOT_ts_es_fore)
#Assess constant variance
plot(R_GRIP_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_GRIP_TOT_ts_es_fore$residuals,'R_GRIP_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for R_GROP_TOT_ts_es.
R_GROP_TOT_ts_es_fore = forecast.HoltWinters(R_GROP_TOT_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
R_GROP_TOT_ts_es_fore
#Look at forecasted values
plot(R_GROP_TOT_ts_es_fore)
#Assess constant variance
plot(R_GROP_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_GROP_TOT_ts_es_fore$residuals,'R_GROP_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for R_NET_TOT_ts_es.
R_NET_TOT_ts_es_fore = forecast.HoltWinters(R_NET_TOT_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
R_NET_TOT_ts_es_fore
#Look at forecasted values
plot(R_NET_TOT_ts_es_fore)
#Assess constant variance
plot(R_NET_TOT_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_NET_TOT_ts_es_fore$residuals,'R_NET_TOT_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for R_TOT_OP_EXP_ts_es.
R_TOT_OP_EXP_ts_es_fore = forecast.HoltWinters(R_TOT_OP_EXP_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
R_TOT_OP_EXP_ts_es_fore
#Look at forecasted values
plot(R_TOT_OP_EXP_ts_es_fore)
#Assess constant variance
plot(R_TOT_OP_EXP_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_TOT_OP_EXP_ts_es_fore$residuals,'R_TOT_OP_EXP_ts_es_fore Normality Assessment')


par(mfrow = c(3,1))
#Create forecast for R_NONOP_REV_ts_es.
R_NONOP_REV_ts_es_fore = forecast.HoltWinters(R_NONOP_REV_ts_es, h = 40)
#Forecasts with 80% and 95% intervals
R_NONOP_REV_ts_es_fore
#Look at forecasted values
plot(R_NONOP_REV_ts_es_fore)
#Assess constant variance
plot(R_NONOP_REV_ts_es_fore$residuals)
lines(c(2007, 2017), c(0, 0), col = 'red')
#Assess normality of residuals
plotForecastErrors(R_NONOP_REV_ts_es_fore$residuals,'R_NONOP_REV_ts_es_fore Normality Assessment')


