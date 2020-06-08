# Clear the Environment
rm(list=ls())

# Read csv file as a DataFrame
#
setwd("C:\\Users\\jonmc\\Documents\\git\\RScripts\\inferential_stats\\data\\")
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
# Simple Regression of NetWorth on Age using the Linear Models function lm(y~x)
#
df1 <- data.frame(networth, age)
reg_NetWorth_Age <- lm(networth ~ age, data = df1)
print(reg_NetWorth_Age)
#
age_40 <- data.frame(age=40)
age_40_networth <- predict(reg_NetWorth_Age, age_40) 
print(paste("Predicted Net Worth at Age 40 = ", round(age_40_networth,2)," dollars"))
#
samp_means <- c(mean(df1$networth), mean(df1$age))
print(samp_means)
samp_sd <- c(sd(df1$networth), sd(df1$age))
print(round(samp_sd,3))
samp_cor <- cor(networth, age)
print(round(samp_cor,3))
#
# Beta slope coefficient = corr*sd(y)/sd(x)
#
beta_hat <- samp_cor*sd(df1$networth)/sd(df1$age)
print(round(beta_hat),3)
alpha_hat <- mean(df1$networth) - beta_hat*mean(df1$age)
print(round(alpha_hat,3))
#
# Generate and add predicted vaues to the Data Frame
df1$pred_networth <- predict(reg_NetWorth_Age)
print(df1)
#
#
# Plot of Networth vs Age
#
plot(df1$age,df1$networth/1000,col="red", 
     xlab="Age in Years", 
     ylab="Networth in 1000's of Dollars", 
     xlim = c(0, 80), ylim= c(0, 400))
text(df1$age, df1$networth/1000, round(df1$networth/1000, 2), cex=0.6, pos=3)
#
# Add Predicted Values to the Plot
# 
par(new=TRUE)
plot(df1$age,df1$pred_networth/1000,type="l",
     yaxt='n', ann=FALSE, col="blue",  xlim = c(0, 80), ylim= c(0, 400))
#
# Obtaining Residuals Manually
# 
df1$resid_networth = df1$networth - df1$pred_networth
print(df1)
# Sum of Squared Residuals
sum_sq_resid <- sum(df1$resid^2)
print (sum_sq_resid)
#
# Regression Model Summary
#
summary(reg_NetWorth_Age)
#
df1$sq_YfromMean <- (df1$networth - mean(df1$networth))^2
df1$sqpredFromMean <- (df1$pred_networth - mean(df1$networth))^2
df1$Sq_resid <- (df1$resid)^2
print(df1)
#
#
print(sum(df1$sqpredFromMean) + sum(df1$Sq_resid))
print(sum(df1$sq_YfromMean))
print(paste("R-Squared is ", sum(df1$sqpredFromMean)/sum(df1$sq_YfromMean)))
#
#
#
plot(df1$age,df1$networth/1000,col="red", 
     xlab="Age in Years", type="h",
     ylab="Networth in 1000's of Dollars", 
     xlim = c(0, 80), ylim= c(0, 400))
text(df1$age, df1$networth/1000, round(df1$networth/1000, 2), cex=0.6, pos=3)
#
#
# Variability Analysis - Using the anova() function
#
anova_networth_age <- anova(reg_NetWorth_Age)
print(round(anova_networth_age),12)
print((anova_networth_age))
#
#
# The Standardized Regression Model
#
z_networth <- (networth - mean(networth))/sd(networth)
z_age <- (age - mean(age))/sd(age)
std_model <- lm(z_networth ~ z_age)
summary(std_model)
#
# Regression Using categorical variable Gender
#
# Convert gender to a Dummy Variable
#
d_gender <- ifelse(gender=="M", 1,0)
#
gender_mod <- lm(income ~ d_gender)
summary(gender_mod)
#
# Compare with t-test
#
male_income <- c(66000, 82000, 111000, 218000, 38900, 434000)
female_income <- c(112000, 182000, 242000, 172000)
t.test(male_income, female_income, paired=FALSE, alternative = "two.sided")



