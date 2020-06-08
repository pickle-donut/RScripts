# Clear the Environment - Include this in all pograms
#
rm(list=ls())
#

#Set the working directory using setwd("") - Point to where you R program is
# 
setwd("C:\\Users\\jonmc\\Documents\\git\\RScripts\\inferential_stats\\data\\")
#
#Read csv file as a DataFrame; header = TRUE means your CSV files will have 
#variable names as headers
#
df <- read.table('ClassData.csv', 
					header = TRUE, sep = ',')

#Assign variable names to DataFrame Column objects varname <- dataframe$column-name
#
id <- df$ID
name <- df$Name
age <- df$Age
gender <-df$Gender
education <- df$Education
crediscore <-df$CreditScore
income <- df$Income
networth <-df$NetWorth
sales <-df$Sales
#
# Variable names in R-program will now be available for use 
#
# Test a column by obtaining summary information
summary(income)



