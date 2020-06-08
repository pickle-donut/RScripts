#libraries
library(psych)
library(car) #Used for Durbin-Watson test, VIF scores, binning
library(dummies)

#working dir

setwd("/Volumes/FILES/Homeworks/R&Py/Project")

#reading file
gas_data = read.csv("ProjectData.csv", header = T)
str(gas_data)

#Reverse Order of Rows
for(i in 1:ncol(gas_data)) {gas_data[,i] = rev(gas_data[,i])}

#Time Series Objects creation
Gas = ts(gas_data$Gas_Price, start=c(1989, 1), freq=12)
Gas

gas_data

Oil = ts(gas_data$Crude_Price, start=c(1989, 1), freq=12)
Oil

Gold = ts(gas_data$Gold, start=c(1989, 1), freq=12)
Gold
gas_data

fit = stl(Gas, Oil, Gold, s.window="period")
plot(fit)

monthplot(Gas)


ts.plot(Gas, Oil, Gold, gpars=list(xlab="Year", ylab="Proice", bty="n"))
lines(Gas, type="l", col="blue")
lines(Oil, type="l", col="green")
lines(Gold, type="l", col="brown")

require(ggplot2)
require(scales)

acf(Oil)
pacf(Oil)

require(forecast)

oilBest  =auto.arima(x=Oil)
