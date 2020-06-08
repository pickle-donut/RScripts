#===============
#PART 1                       
#===============
#Import libraries.
install.packages("psych")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("RODBC")

#Install packages necessary for functions.
library(foreign)
library(psych)
library(Hmisc)
library(ggplot2)

#Check for current working directory, initialize variable workingDirectory,
#and set it as the current working directory. Verify change in directory.
getwd()
workingDirectory = 'C:\\Users\\jonmc\\Documents\\git\\RScripts\\ml\\data\\general\\'
setwd(workingDirectory)
getwd()

#Read in hospital and personnel data, display column names, and join columns on primary key and foreign key.
orgHospitalData = read.csv(file.choose(), header = T)
orgPersonnelData = read.table(file.choose(), header = T, sep = "\t")
names(orgHospitalData)
names(orgPersonnelData)

#Combine both data tables. Display resulting table.
combinedData = merge(orgHospitalData, orgPersonnelData, by = "HospitalID")
combinedData

#Insert data into row 1.
str(combinedData)
combinedData[,c('Name', 'Zip', 'Website', 'FirstName', 'LastName')] <- sapply(combinedData[, c('Name', 'Zip', 'Website', 'FirstName', 'LastName')], as.character)
combinedData[1,1] = 11111
combinedData[1,2] = 'OU Medical'
combinedData[1,3] = '73034'
combinedData[1,4] = 'www.oumedical.org'
combinedData[1,5] = 'City/County'
combinedData[1,6] = 'Teaching'
combinedData[1,7] = 'Alumni'
combinedData[1,8] = 160
combinedData[1,9] = 57729.56
combinedData[1,10] = 10131251
combinedData[1,11] = 11391721
combinedData[1,12] = 21343697
combinedData[1,13] = -179275
combinedData[1,14] = 100
combinedData[1,15] = 123456
combinedData[1,16] = 'McCallum'
combinedData[1,17] = 'Jonathan'
combinedData[1,18] = 'F'
combinedData[1,19] = 4
combinedData[1,20] = 'Safety Inspection Member'
combinedData[1,21] = 23987
combinedData[1,22] = 2
#Change any date columns to date datatypes. Insert today's date.
StartDate = strptime(as.character(combinedData$StartDate), "%m/%d/%Y")
StartDate = as.data.frame(StartDate)
combinedData = data.frame(combinedData, StartDate)
combinedData[1,24] = '2017/2/2'
combinedData

#Remove primary key columns.
combinedData1 = subset(combinedData, select = -c(HospitalID, Work_ID, PositionID))
combinedData1
str(combinedData1)

#Provide summary of of the mean, median, minimum, and maximum value for each numeric variable.
summary(combinedData1[,sapply(combinedData1, is.numeric)])

#Write data to file.
write.table(combinedData1, file = 'Mccallum_Jonathan_export.txt', row.names = F, sep = "\t")

#===============
#PART 2                        
#===============
#Read in exported table.
orgData = read.table(file.choose(), header = T, sep = "\t")
orgData = subset(orgData, select = -c(StartDate.1))

#Convert appropriate factor columns to character columns. 
orgData[,c('Name', 'Zip', 'Website', 'FirstName', 'LastName')] = sapply(orgData[, c('Name', 'Zip', 'Website', 'FirstName', 'LastName')], as.character)

#Convert dates from factors to dates.
StartDate = strptime(as.character(orgData$StartDate), "%m/%d/%Y")
StartDate = as.data.frame(StartDate)
orgData = data.frame(orgData, StartDate)

#Check data types.
str(orgData)

#Summary of characters.
summary(orgData[,sapply(orgData, is.character)])

#Summary of factors.
summary(orgData[,sapply(orgData, is.factor)])

#Create histograms.
par(mfrow = c(3,3))
hist(orgData$NoFTE, main="Full Time Count Histogram", xlab = "Full Time Count")
hist(orgData$InOperExp, main="Inpatient Operating Expense Histogram", xlab = "Inpatient Operating Expense")
hist(orgData$OutOperExp, main="Outpatient Operating Expense Histogram", xlab = "Outpatient Operating Expense")
hist(orgData$OperRev, main="Operating Revenue Histogram", xlab = "Operating Revenue")
hist(orgData$OperInc, main="Operating Income Histogram", xlab = "Operating Income")
hist(orgData$AvlBeds, main="Available Beds Histogram", xlab = "Available Beds")
hist(orgData$NetPatRev, main="Net Patient Revenue Histogram", xlab = "Net Patient Revenue")

Create scatterplots.
#There appears to be a strong, positive (direct) linear trend in the data for all of these plots except
#for the relationship between Net Patient Revenue and Operating Income.
par(mfrow = c(3,2))
plot(orgData$NoFTE, orgData$NetPatRev, main = "Full Time Employee Count vs Net Patient Revenue", xlab = "Full Time Count", ylab = "Net Patient Revenue")
plot(orgData$InOperExp, orgData$NetPatRev, main = "Inpatient Operating Expense vs Net Patient Revenue", xlab = "Inpatient Operating Expense", ylab = "Net Patient Revenue")
plot(orgData$OutOperExp, orgData$NetPatRev, main = "Outpatient Operating Expense vs Net Patient Revenue", xlab = "Outpatient Operating Expense", ylab = "Net Patient Revenue")
plot(orgData$OperRev, orgData$NetPatRev, main = "Operating Revenue vs Net Patient Revenue", xlab = "Operating Revenue", ylab = "Net Patient Revenue")
plot(orgData$OperInc, orgData$NetPatRev, main = "Operating Income vs Net Patient Revenue", xlab = "Operating Income", ylab = "Net Patient Revenue")
plot(orgData$AvlBeds, orgData$NetPatRev, main = "Available Beds vs Net Patient Revenue", xlab = "Available Beds", ylab = "Net Patient Revenue")

#Create boxplots.
par(mfrow = c(3,3))
boxplot(orgData$NoFTE, main="Full Time Count Boxplot", xlab = "Full Time Count")
boxplot(orgData$InOperExp, main="Inpatient Operating Expense Boxplot", xlab = "Inpatient Operating Expense")
boxplot(orgData$OutOperExp, main="Outpatient Operating Expense Boxplot", xlab = "Outpatient Operating Expense")
boxplot(orgData$OperRev, main="Operating Revenue Boxplot", xlab = "Operating Revenue")
boxplot(orgData$OperInc, main="Operating Income Boxplot", xlab = "Operating Income")
boxplot(orgData$AvlBeds, main="Available Beds Boxplot", xlab = "Available Beds")
boxplot(orgData$NetPatRev, main="Net Patient Revenue Boxplot", xlab = "Net Patient Revenue")

#Draw QQ plots and lines.
par(mfrow = c(3,3))
qqnorm(orgData$NoFTE, main="Full Time Count QQ Plot")
qqline(orgData$NoFTE, lty = 2)
qqnorm(orgData$InOperExp, main="Inpatient Operating Expense QQ Plot")
qqline(orgData$InOperExp, lty = 2)
qqnorm(orgData$OutOperExp, main="Outpatient Operating Expense QQ Plot")
qqline(orgData$OutOperExp, lty = 2)
qqnorm(orgData$OperRev, main="Operating Revenue QQ Plot")
qqline(orgData$OperRev, lty = 2)
qqnorm(orgData$OperInc, main="Operating Income QQ Plot")
qqline(orgData$OperInc, lty = 2)
qqnorm(orgData$AvlBeds, main="Available Beds QQ Plot")
qqline(orgData$AvlBeds, lty = 2)
qqnorm(orgData$NetPatRev, main="Net Patient Revenue QQ Plot")
qqline(orgData$NetPatRev, lty = 2)
#Most of the data appears to not possess any sense of normality.

#Shapiro-Wilk tests.
shapiro.test(orgData$NoFTE)
shapiro.test(orgData$InOperExp)
shapiro.test(orgData$OutOperExp)
shapiro.test(orgData$OperRev)
shapiro.test(orgData$OperInc)
shapiro.test(orgData$AvlBeds)
shapiro.test(orgData$NetPatRev)
#All the p-values are less than 0.05, thus we reject the hypothesis that the�observed�distribution�fits�a normal�distribution.

#Create bar chart for non-normal variable.
counts = table(orgData$NoFTE)
barplot(counts, main="Employee Count Distribution", xlab="Employee Count") 
#Normality for the number of full time employees can be exploited by the fact that the size of the hospital may dictate
#how many employees work there. For example, whether the hospital operates in the city or a rural community will dictate
#very different needs in staff.

#Create categorical bar charts, 2 horizontal.
par(mfrow = c(2,2))
counts = table(orgData$TypeControl)
barplot(counts, main="Type Control Distribution", names.arg = c("City/County", "District", "Non Profit", "Investor"))
counts = table(orgData$PositionTitle)
barplot(counts, main="Position Distribution", names.arg = c("Safety Inspection Member", "Regional Representative", "Acting Director", "State Board Representative"))
counts = table(orgData$Teaching)
barplot(counts, main="Teaching Distribution", horiz=TRUE, names.arg = c("Teaching", "Small/Rural"))
counts = table(orgData$DonorType)
barplot(counts, main="Donor Distribution", horiz=TRUE, names.arg = c("Alumni", "Charity"))


