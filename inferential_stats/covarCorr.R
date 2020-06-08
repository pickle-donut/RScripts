# Clear the Environment
rm(list=ls())

# Read csv file as a DataFrame
#
setwd("C:\\Users\\sarathy\\Documents\\2019-Teaching\\Fall2019\\Fall2019-MSIS5503\\MSIS-5503-Data")
df <- read.table('ClassData.csv', 
                 header = TRUE, sep = ',')

#Assign variable names to DataFrame Column objects
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
# Create Matrix M using the variable vectors and the cbind() function
M <- cbind(age, income, networth, sales)
#
covar_mat <- cov(M, use="all.obs", method ="pearson")
print(covar_mat)
#
# We use the signif() function instead of round(), beccause round() will round to the nearest integer
corr_mat <- cor(M, method = "pearson")
print(signif(corr_mat),digits = 4)
#
#
z_age <- (age - mean(age))/sd(age)
z_income <- (income - mean(income))/sd(income) 
z_networth <- (networth - mean(networth))/sd(networth)
z_sales <- (sales - mean(sales))/sd(sales)
#
z_M <-  cbind(z_age, z_income, z_networth, z_sales)
#
covar_z_mat <- cov(z_M, use="all.obs", method ="pearson")
print(signif(covar_z_mat),digits = 4)
