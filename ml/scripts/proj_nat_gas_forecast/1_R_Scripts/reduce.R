#libraries
library(psych)
library(car) #Used for Durbin-Watson test, VIF scores, binning
library(dummies)

#working dir

setwd("/Volumes/FILES/Homeworks/R&Py/Project")

#reading file
gas_data = read.csv("ProjectData.csv", header = T)
str(gas_data)
head(gas_data)

#converting Reporting Date to date object
gas_data$Report_Date = strptime(as.character(gas_data$Report_Date), "%m/%d/%Y")

month = strftime(gas_data$Report_Date, "%m")
gas_data$Month = month

year = strftime(gas_data$Report_Date, "%Y")
gas_data$Year = year


gas_data$Year = as.integer(gas_data$Year)
range(gas_data$Year)


unique(gas_data$Month)
unique(gas_data$Year)
str(gas_data)

#new dframe for pca analysis
reduc_pca = gas_data[,c(2,3,4)]

#PCA analysis
pcamodel_reduc = princomp(reduc_pca, cor=FALSE)
pcamodel_reduc$sdev^2

pcamodel_reduc = princomp(reduc_pca, cor=FALSE)
plot(pcamodel_reduc, main = "Screeplot of Crude Oil, Gas and Gold prices")

#Factor analysis
gas_data_FA = factanal(~Crude_Price+Gas_Price+Gold_Price, factors=1, rotation = "varimax", scores = "none", data = gas_data)
gas_data_FA

#Cluster Analysis
km = kmeans(data.frame(gas_data$Crude_Price, gas_data$Gas_Price), 3)
plot(gas_data$Crude_Price, gas_data$Gas_Price, col=km[[1]])
table(km[[1]], gas_data$Decade)

km = kmeans(data.frame(gas_data$Crude_Price, gas_data$Gold_Price), 3)
plot(gas_data$Crude_Price, gas_data$Gold_Price, col=km[[1]])
table(km[[1]], gas_data$Decade)

km = kmeans(data.frame(gas_data$Gas_Price, gas_data$Gold_Price), 2)
plot(gas_data$Gas_Price, gas_data$Gold_Price, col=km[[1]])
table(km[[1]], gas_data$Decade)

#split data by decades
bin_interval = c(1989, 1990, 2000, 2010, 2017)
table(cut(gas_data$Year, bin_interval, right = FALSE))
decade_vector = recode(gas_data$Year, "1989='1980s'; 1990:1999 = '1990s'; 2000:2009='2000s'; 2010:2017='2010s'")
gas_data$Decade = decade_vector

gas_data[, c('Year', 'Decade')]
gas_data

#Cluster Analysis - decade based
km = kmeans(data.frame(gas_data$Crude_Price, gas_data$Gas_Price), 3)
plot(gas_data$Crude_Price, gas_data$Gas_Price, bty="n", xlab="Oil Price", ylab="Gas Price", col=km[[1]])
table(km[[1]], gas_data$Decade)

km = kmeans(data.frame(gas_data$Gold_Price, gas_data$Gas_Price), 3)
plot(gas_data$Gold_Price, gas_data$Gas_Price, bty="n", xlab="Gold Price", ylab="Gas Price", col=km[[1]])
table(km[[1]], gas_data$Decade)

km = kmeans(data.frame(gas_data$Gas_Price, gas_data$Gold_Price), 4)
plot(gas_data$Gas_Price, gas_data$Gold_Price, col=km[[1]])
table(km[[1]], gas_data$Decade)

gas_data
