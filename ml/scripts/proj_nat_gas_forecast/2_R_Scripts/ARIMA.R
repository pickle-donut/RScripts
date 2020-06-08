#Read in Libraries
library(tseries)
library(forecast)
library(lmtest)



#Setup the Working Directory
workingdirectory = "/Volumes/FILES/Homeworks/R&Py/Project/"
setwd(workingdirectory)


#Read in data
prices = read.csv('ProjectData.csv',  header = TRUE)
prices


#Reversing Order of Rows
for(i in 1:ncol(prices)) {prices[,i] = rev(prices[,i])}
prices

#Split for train/test 60/40
#total_rows = nrow(prices)
#train_rows = round(total_rows*0.6, digits = 0)

#prices_train = prices[1:train_rows,]
#prices_test = prices[(train_rows+1):total_rows,]

#check for split correctness
#nrow(prices_train)
#nrow(prices_test)
#nrow(prices)

#Creating time series object
gas_price_ts = ts(prices$Gas_Price, frequency = 12, start = 1989)
gas_price_ts

#Check for stationarity
# Augmented Dickey?Fuller (ADF) t-test
adf.test(gas_price_ts, k = 20, alternative = "stationary")

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(gas_price_ts)


#Assessment for seasonal component
  
  #Examine time series plot
  plot(gas_price_ts,  bty="n", col="blue", main="Natural Gas Prices 1989-2016", ylab="USD", xlab="", col.lab="grey" )
  

  #boxplot to determine seasonality
  boxplot(gas_price_ts ~ cycle(gas_price_ts), frame="F", col="lightblue", main="Natural gas prices aggregated by month", xlab="month", ylab="USD")
  
  
  #Decompose
  gas_price_dc = decompose(gas_price_ts)

  #Check for mean and variance with and without seasonal components
  gas_price_no_season = gas_price_ts - gas_price_dc$seasonal
  mean(gas_price_no_season)
  var(gas_price_no_season)

  #compare to original dataset
  mean(gas_price_ts)
  var(gas_price_ts)
  
  #check the acf plot
  acf(gas_price_ts, frame="F", col="red", main="Natural Gas Price")

  #remove seasonal component
  gas_price_ts = gas_price_ts - gas_price_dc$seasonal

  
  
#Assessment for trend component
  
  #plot the trend component
  plot(gas_price_dc$trend, bty="n", col="blue", main="NATURAL GAS PRICES TREND 1989-2016", ylab="USD")
  
  #check the acf plot
  acf(gas_price_ts)
  
  #regression
  gas_price_trendcomp = gas_price_dc$trend
  gas_price_trend_data = data.frame(trend = c(gas_price_trendcomp), time = c(time(gas_price_trendcomp)))
  gas_price_trend_reg = lm(gas_price_trend_data$trend ~ gas_price_trend_data$time)
  summary(gas_price_trend_reg)
  

    
#Final tests for stationarity
  
  # Augmented Dickey?Fuller (ADF) t-test
  adf.test(gas_price_ts, k = 30, alternative = "stationary")

  # Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
  kpss.test(gas_price_ts)


    
#Differencing to remove trend effects
gas_price_diff1 = diff(gas_price_ts, differences = 1)
plot(gas_price_diff1, bty="n", col="blue", ylab="USD", xlab="", main="Natural gas prices - differenced")



#Checking stationarity after differencing

  # Augmented Dickey?Fuller (ADF) t-test
  adf.test(gas_price_diff1, k = 30, alternative = "stationary")
  
  # Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
  kpss.test(gas_price_diff1)
  
  
#Identifying AR and MA components
  
  acf(gas_price_diff1, bty="n", col="red", main="Series Gas Price(differenced)")
  pacf(gas_price_diff1, lag.max = 30, bty="n", col="red", main="Series Gas Price(differenced)")

  #ACF Dies down dramatically which indicates MA component
  #PCF cutts off dramatically indicating AR component
  
  
#Building the models
  
  gas_price_arima1 = Arima(gas_price_ts, order = c(1, 1, 1), method = "ML")
  gas_price_arima2 = Arima(gas_price_ts, order = c(0, 2, 2), method = "ML")
  gas_price_arima3 = Arima(gas_price_ts, order = c(1, 2, 2), method = "ML")
  gas_price_arima4 = Arima(gas_price_ts, order = c(1, 2, 1), method = "ML")
  
#Assessing the models
  
  #AIC Values
  gas_price_arima1
  gas_price_arima2
  gas_price_arima3
  gas_price_arima4
  
  #BIC values
  gas_price_arima1_bic = AIC(gas_price_arima1, k = log(length(gas_price_diff1)))
  gas_price_arima2_bic = AIC(gas_price_arima2, k = log(length(gas_price_diff1)))
  gas_price_arima3_bic = AIC(gas_price_arima3, k = log(length(gas_price_diff1)))
  gas_price_arima4_bic = AIC(gas_price_arima4, k = log(length(gas_price_diff1)))
  
  gas_price_arima1_bic 
  gas_price_arima2_bic 
  gas_price_arima3_bic
  gas_price_arima4_bic
  
  #Significance of components
  coeftest(gas_price_arima1)
  coeftest(gas_price_arima2)
  coeftest(gas_price_arima3)
  #coeftest(gas_price_arima4)
  
  
#Building the forecast
  
  gas_price_arima1_fore = forecast(gas_price_arima1, h=5)
  gas_price_arima2_fore = forecast(gas_price_arima2, h=5)
  gas_price_arima3_fore = forecast(gas_price_arima3, h=5)
  #gas_price_arima4_fore = forecast(gas_price_arima4, h=5)

#Assessing the forecast
  
  options(digits = 4)
  accuracy(gas_price_arima1_fore)
  accuracy(gas_price_arima2_fore)
  accuracy(gas_price_arima3_fore)
  #accuracy(gas_price_arima4_fore)
  
    
#Alternative method holt winters(trend adjusted exponential smoothing)
  gas_price_ts_es = HoltWinters(gas_price_ts, beta = FALSE, gamma = FALSE, alpha = 0.9)
  gas_price_ts_es
  plot(gas_price_ts_es)
  fore = forecast.HoltWinters(gas_price_ts_es, h = 30)
  plot(fore)
  
#Plotting ARIMA model
  
  plot(gas_price_arima1$x,col="blue", main="", bty="n", ylab="USD", xlab="")
  lines(fitted(gas_price_arima1),col="red")
  


