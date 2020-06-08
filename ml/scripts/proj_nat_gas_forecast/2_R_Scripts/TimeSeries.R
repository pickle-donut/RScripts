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
workingdirectory = "C:\\Users\\jonmc\\Desktop\\Gradz School\\Semester # 2\\OOP for Data Science\\Project\\Data-CSV"
setwd(workingdirectory)


#############################################
#===============Read in data================#
#############################################
projData = read.table(file.choose(), sep = ',', header = TRUE)
str(projData)
projData$Report_Date = strptime(as.character(projData$Report_Date), "%Y-%m-%d")

#Standardize variables.
projData$Crude_Price = ((projData$Crude_Price - mean(projData$Crude_Price)) / sd(projData$Crude_Price))
projData$Gas_Price = ((projData$Gas_Price - mean(projData$Gas_Price)) / sd(projData$Gas_Price))
projData$Gold_Price = ((projData$Gold_Price - mean(projData$Gold_Price)) / sd(projData$Gold_Price))
projData


#############################################
#====Convert data to time series object=====#
#############################################
projData_ts1 = ts(projData$Crude_Price, frequency = 12, start = c(1989, 1))
projData_ts1
plot(projData_ts1, ylab = "Crude Price", main = "Crude Price Time Series")

projData_ts2 = ts(projData$Gas_Price, frequency = 12, start = c(1989, 1))
projData_ts2
plot(projData_ts2, ylab = "Gas Price", main = "Gas Price Time Series")

projData_ts3 = ts(projData$Gold_Price, frequency = 12, start = c(1989, 1))
projData_ts3
plot(projData_ts3, ylab = "Gold Price", main = "Gold Price Time Series")



#################################################
#=============Descriptive Analysis==============#
#################################################
start(projData_ts1)
end(projData_ts1)
frequency(projData_ts1)

start(projData_ts2)
end(projData_ts2)
frequency(projData_ts2)

start(projData_ts3)
end(projData_ts3)
frequency(projData_ts3)


#################################################
#===========Decompose Time Series===============#
#################################################
#Decompose and plot time series object.
projData_ts1_dc = decompose(projData_ts1, "multiplicative")
plot(projData_ts1_dc)

projData_ts2_dc = decompose(projData_ts2, "multiplicative")
plot(projData_ts2_dc)

projData_ts3_dc = decompose(projData_ts3, "multiplicative")
plot(projData_ts3_dc)



