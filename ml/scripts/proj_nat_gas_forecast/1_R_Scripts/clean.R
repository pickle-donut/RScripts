#libraries
library(psych)
require(ggplot2)

#working dir

setwd("/Volumes/FILES/Homeworks/R&Py/Project")

#reading file
gas_data = read.csv("ProjectData.csv", header = T)
str(gas_data)

#converting Reporting Date to date object
gas_data$Report_Date = strptime(as.character(gas_data$Report_Date), "%Y-%m-%d")
str(gas_data$Report_Date)

#identifying missing values
is.na(gas_data)

str(gas_data$Report_Date)



#GGPLOT PLAYGROUND

#Histogram
gplot(data=gas_data) + geom_histogram(aes(x=gas_data$Crude_Price))
ggplot(data=gas_data) + geom_histogram(aes(x=gas_data$Crude_Price, binwidth=200))

ggplot(data=gas_data) + geom_density(aes(x=gas_data$Crude_Price))
ggplot(data=gas_data) + geom_density(aes(x=gas_data$Gas_Price))
ggplot(data=gas_data) + geom_density(aes(x=gas_data$Gold_Price))

#Scatterplot
ggplot(gas_data, aes(x=Gas_Price, y=Gold_Price)) + geom_point()
g <- ggplot(gas_data, aes(x=Gas_Price, y=Gold_Price))
g + geom_point(aes(color=Crude_Price))

#boxplots and violin plot
ggplot(gas_data, aes(y=Crude_Price, x=0.5)) + geom_boxplot()
ggplot(gas_data, aes(y=Crude_Price, x=1)) + geom_boxplot()

ggplot(gas_data, aes(y=Crude_Price, x=1)) + geom_violin()
ggplot(gas_data, aes(y=Crude_Price, x=1))   + geom_violin() + geom_point()
ggplot(gas_data, aes(y=Crude_Price, x=1)) + geom_jitter() + geom_violin()


#Time series
