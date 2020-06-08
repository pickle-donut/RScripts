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
globaltemp_data = read.table('global.txt', sep = '\t', header = TRUE)
colnames(globaltemp_data)
globaltemp_data #Data frame object.

# Convert data frame into time series object.
globaltemp_ts = ts(globaltemp_data, frequency = 12, start = c(1856, 1))
globaltemp_ts


#################################################
#=============Descriptive Analysis==============#
#################################################
start(globaltemp_ts)

end(globaltemp_ts)

frequency(globaltemp_ts)

class(globaltemp_ts)

plot(globaltemp_ts,  ylab="Temperature")


#################################################
#===========Decompose Time Series===============#
#################################################
#Decompose and plot time series object.
globaltemp_ts_dc = decompose(globaltemp_ts)
plot(globaltemp_ts_dc)


#################################################
#===========Exponential Smoothing===============#
#################################################
#Exponential smoothing with alpha = 0.1.
globaltemp_es = HoltWinters(globaltemp_ts, beta = FALSE, gamma = FALSE, alpha = 0.1)

#Exponential smoothing with alpha = 0.9.
globaltemp_es2 = HoltWinters(globaltemp_ts, beta = FALSE, gamma = FALSE, alpha = 0.9)

#Obtain estimate of alpha; do not provide a value for alpha
globaltemp_es3 = HoltWinters(globaltemp_ts, beta = FALSE, gamma = FALSE)
globaltemp_es3

#Plot time series with differing alphas.
layout(1:3)
plot(globaltemp_es, main='alpha = 0.1')
plot(globaltemp_es2, main='alpha = 0.9')
plot(globaltemp_es3, main='predicted alpha = 0.42')

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

layout(1:3)
#Forecast the model beyond the known range of data.
#Forecasts with 80% and 95% intervals.
#Plot of forecasted values.
globaltemp_es3_fore = forecast.HoltWinters(globaltemp_es3, h = 10)
globaltemp_es3_fore
plot(globaltemp_es3_fore)

#Assess constant variance; fairly constant variance.
plot(globaltemp_es3_fore$residuals)
abline(h = 0.0, col = "red")

#Assess normality of residuals.
plotForecastErrors(globaltemp_es3_fore$residuals,'Normal Distribution Assessment')


#######################################################
#===========Trend Exponential Smoothing===============#
#######################################################
#Remove season to use Trend-Adjusted Exponential Smoothing
globaltemp_ts_trend = globaltemp_ts - globaltemp_ts_dc$seasonal

globaltemp_es4 = HoltWinters(globaltemp_ts_trend,
                        gamma = FALSE)

#Estimate.
globaltemp_es4

#Plot series less seasonal data.
plot(globaltemp_es4)

####For without seasonal component.
#Forecast the next 10 periods.
#Assess constant variance
#Assess normal distribution
par(mfrow = c(3, 1))
globaltemp_es_fore = forecast.HoltWinters(globaltemp_es4, h = 10)
plot(globaltemp_es_fore)
plot(globaltemp_es_fore$residuals, main='Global Temp: No Seasonal Component')
lines(c(1856, 2017), c(0, 0), col = 'red')
plotForecastErrors(globaltemp_es_fore$residuals,'Global Temp: No Seasonal Component')


##############################################################
#===========Holt-Winters Exponential Smoothinng==============#
##############################################################
#Leave season in the model.
globaltemp_es5 = HoltWinters(globaltemp_ts)

#Estimate.
globaltemp_es5

#Plot series with seasonal data.
plot(globaltemp_es5)

####For seasonal component.
#Forecast the next 10 periods.
#Assess constant variance
#Assess normal distribution
par(mfrow = c(3, 1))
globaltemp_es_fore2 = forecast.HoltWinters(globaltemp_es5, h = 10)
plot(globaltemp_es_fore2)
plot(globaltemp_es_fore2$residuals, main='Global Temp: Seasonal Component')
lines(c(1856, 2017), c(0, 0), col = 'red')
plotForecastErrors(globaltemp_es_fore2$residuals,'Global Temp: Seasonal Component')


################################################################
#=============Create Seasonal Assessment Box Plot==============#
################################################################
boxplot(globaltemp_ts ~ cycle(globaltemp_ts))


########################################################
#=============Compare Standard Deviations==============#
########################################################
# Compare standard deviations to see if seasonal effect does exist
sd(globaltemp_ts)

sd(globaltemp_ts - globaltemp_ts_dc$seasonal)

