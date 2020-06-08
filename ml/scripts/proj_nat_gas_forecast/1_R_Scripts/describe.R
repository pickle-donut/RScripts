#libraries
library(psych)

#working dir

setwd("/Volumes/FILES/Homeworks/R&Py/Project")

#reading file
gas_data = read.csv("ProjectData.csv", header = T)
str(gas_data)

#converting Reporting Date to date object
gas_data$Report_Date = strptime(as.character(gas_data$Report_Date), "%Y-%m-%d")
str(gas_data)

#summary
summary(gas_data)
describe(gas_data)

#identifying outliers
boxplot(gas_data)
boxplot(gas_data$Crude_Price)
boxplot(gas_data$Gas_Price, main="Natural Gas Price")
boxplot(gas_data$Gold_Price, main="Gold Price")

#time series plot
year = strftime(gas_data$Report_Date, "%Y")
gas_data$Year = year
plot(gas_data$Year, gas_data$Gas_Price)

#identifying linear realtionship
pairs(gas_data[,c(2,3,4)], panel = panel.smooth)

#histograms
hist(gas_data$Crude_Price, main="Crude Oil Price")
skew(gas_data$Crude_Price)
kurtosi(gas_data$Crude_Price)

hist(gas_data$Gas_Price, main="Natural Gas Price")
skew(gas_data$Gas_Price)
kurtosi(gas_data$Gas_Price)

hist(gas_data$Gold_Price, main="Gold Price")
skew(gas_data$Gold_Price)
kurtosi(gas_data$Gold_Price)

#Assessing normality

#Crude_Price
qqnorm(gas_data$Crude_Price, main="Crude Oil Price")
qqline(gas_data$Crude_Price, lty=2)
shapiro.test(gas_data$Crude_Price)

#Gas_Price
qqnorm(gas_data$Gas_Price, main="Gas Price")
qqline(gas_data$Gas_Price, lty=2)
shapiro.test(gas_data$Gas_Price)

#Gold_Price
qqnorm(gas_data$Gold_Price, main="Gold Price")
qqline(gas_data$Gold_Price, lty=2)
shapiro.test(gas_data$Gold_Price)

