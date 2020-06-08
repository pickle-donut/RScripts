# Clear the Environment
rm(list=ls())

# Read csv file as a DataFrame
#
setwd("C:\\Users\\sarathy\\Documents\\2019-Teaching\\Fall2019\\Fall2019-MSIS5503\\MSIS-5503-Data")
df <- read.table('MR100.csv', 
                 header = TRUE, sep = ',')

#Assign variable names to DataFrame Column objects
id <- df$Obs
gender <-df$Gender
marital <- df$Marital_Status
age<-df$Age
home <- df$Home_Value
mortgage<- df$Mortgage_Balance
assets <-df$Assets
#
# Creating plots of Assets and Overlaying Prediction lines for each of the three predictors
#
par(mfrow=c(1,3))
plot(age,assets, main="Scatterplot of Assets vs. Age")
plot(mortgage,assets, main="Scatterplot of Assets vs. Mortgage Balance")
plot(home,assets, main="Scatterplot of Assets vs Home Value")
#
#
logMort <- log(mortgage)
logHome <- log(home + 0.0001)
#
par(mfrow=c(1,2))
plot(logMort,assets, main="Scatterplot of Assets vs. Log Mortgage Balance")
plot(logHome,assets, main="Scatterplot of Assets vs Log Home Value")
#
# Checking correlations before and after transformations
#
M1 <- cbind(assets, age, home, mortgage)
print(cor(M1))
M2 <- cbind(assets, age, logHome, logMort)
print(cor(M2))
#
mod1 <- lm(assets ~ age+mortgage) 
summary(mod1)
mod2 <- lm(assets ~ age+logHome+logMort)
summary(mod2)
#
# Comparison of Standardized Residuals from Untransformed and Transformed Predictors
#
library(moments)
mod1_rstand <-rstandard(mod1)
qqnorm(mod1_rstand, ylab="Standardized Residuals of Assets", xlab="Normal Quantiles")
qqline(mod1_rstand)
hist(mod1_rstand)
print(skewness(mod1_rstand))
print(kurtosis(mod1_rstand))
#
mod2_rstand <-rstandard(mod2)
qqnorm(mod2_rstand, ylab="Standardized Residuals of Assets", xlab="Normal Quantiles")
qqline(mod2_rstand)
hist(mod2_rstand)
print(skewness(mod2_rstand))
print(kurtosis(mod2_rstand))
#
# Residuals Normality Tests
#
# Kolomogorov-Smirnov Test
print(ks.test(mod1_rstand,"pnorm"))
print(ks.test(mod2_rstand, "pnorm"))
# Shapiro-Wilk Test
print(shapiro.test(mod1_rstand))
print(shapiro.test(mod2_rstand))
#
# Plots for Checking Heteroscedascticity 
# using Standardized Residuals vs Standardized Predictions
#
# Getting Standardized Predictions for Each Model
#
mod1_pred <- predict(mod1)
mod2_pred <- predict(mod2)
mod1_pstand <-  (mod1_pred - mean(mod1_pred))/sd(mod1_pred)
mod2_pstand <-  (mod2_pred - mean(mod2_pred))/sd(mod2_pred)
#
# Do the plot for each model
#
par(mfrow=c(2,1))
plot(mod1_pstand, mod1_rstand, 
     main = "Model 1: lm(assets ~ age+mortgage)
     \nStandardized Predictions vs Standardized Residuals",
     xlab = "Standardized Predictions",
     ylab = "Standardized Residuals",
     xlim = c(-3, 3))
     
plot(mod2_pstand, mod2_rstand, 
     main = "Model 2: lm(assets ~ age+logHome+logMort)
     \nStandardized Predictions vs Standardized Residuals",
     xlab = "Standardized Predictions",
     ylab = "Standardized Residuals",
     xlim = c(-3, 3))
#
# Multicollinearity demonstration
#
# # From Rosenkrantz, "Probability and Statistics for Science, Engineering
# and Finance," CRC Press, Boca raton, 2009. Table 10.2.
# 12 1992 cars were measured for fuel efficiency. The response variable
# is miles per gallon (MPG).
#
car_weight <- c(2.495, 2.53, 2.62, 3.395, 3.03, 3.345, 3.04, 3.085, 3.495, 3.95, 3.47, 4.105) # weight in 1000 pounds
car_mpg <- c(32, 30, 29, 25, 27, 28, 29, 27, 28, 25, 28, 25) # Miles per gallon
car_disp <- c(1.9, 1.8, 1.6, 3, 2.2, 3.8, 2.2, 3, 3.8, 4.6, 3.8, 5.7) # Engine displacement in liters
#
M <- cbind(car_mpg, car_weight, car_disp)
print(cor(M))
#
print("car_weight and car_disp are highly correlated - high collinearity")
#
car_mod1 <- lm(car_mpg ~ car_disp)
car_mod2 <- lm(car_mpg ~ car_weight+car_disp)
#
summary(car_mod1)
summary(car_mod2)
#
# Collinearity diagnostics using library "olsrr"
library(olsrr)
ols_vif_tol(car_mod2)
#
ols_vif_tol(mod1)
ols_vif_tol(mod2)
#
# Outlier Analysis
##
df_age <- data.frame(id, assets, age)
#
# Non-Outlier - High Leverage Observation -Low Influence
df_age1 <- rbind(df_age, c(101, 138, 110))
#
mod0 <- lm(assets~age, df_age)
summary(mod0)
pred_line <- predict(mod0)
mod1 <- lm(assets~age, df_age1)
summary(mod1)
pred_line1 <- predict(mod1)
#
plot(df_age1$age, df_age1$assets, xlab = "Age", ylab = "Assets",  ylim=c(40, 180),
     main = "Non-Outlier - High Leverage Observation -Low Influence")
abline(mod0, col = "green")
abline(mod1, col = "red")
text(df_age1$age, df_age1$assets, labels=df_age1$id, 
     cex = ifelse(df_age1$id == 101,1.2,0.6), pos = 3, 
     col = ifelse(df_age1$id == 101,2,3))
#
# Outlier Observation - Low Leverage - High Influence
df_age2 <- rbind(df_age, c(101, 170, 15))
#
mod0 <- lm(assets~age, df_age)
summary(mod0)
pred_line <- predict(mod0)
mod1 <- lm(assets~age, df_age2)
summary(mod1)
pred_line1 <- predict(mod1)
#
plot(df_age2$age, df_age2$assets, xlab = "Age", ylab = "Assets",  ylim=c(40, 180),
     main = "Outlier Observation - Low Leverage - High Influence")
abline(mod0, col = "green")
abline(mod1, col = "red")
text(df_age2$age, df_age2$assets, labels=df_age2$id, 
     cex = ifelse(df_age2$id == 101,1.2,0.6), pos = 3, 
     col = ifelse(df_age2$id == 101,2,3))
#
#
# Outlier Observation - High Leverage - High Influence
df_age2 <- rbind(df_age, c(101, 42, 95))
#
mod0 <- lm(assets~age, df_age)
summary(mod0)
pred_line <- predict(mod0)
mod1 <- lm(assets~age, df_age2)
summary(mod1)
pred_line1 <- predict(mod1)
#
plot(df_age2$age, df_age2$assets, xlab = "Age", ylab = "Assets",  ylim=c(40, 180),
     main ="Outlier Observation - High Leverage - High Influence")
abline(mod0, col = "green")
abline(mod1, col = "red")
text(df_age2$age, df_age2$assets, labels=df_age2$id, 
     cex = ifelse(df_age2$id == 101,1.2,0.6), pos = 3, 
     col = ifelse(df_age2$id == 101,2,3))
#
# Outlier Diagnostics
#
plot(home, assets, xlab = "Home Value", ylab = "Assets")
text(home, assets, labels=paste(id, "-", home), cex = 0.6, pos = 3, col = 2)
#
mod_home <- lm(assets~home)
summary(mod_home)
leverage <- hatvalues(mod_home)
stud_res <- rstudent(mod_home)
cook_dist <- cooks.distance(mod_home)
#
#
df_home <-data.frame(home, assets, leverage, stud_res, cook_dist)
print(round(df_home,4))
#
plot(leverage, assets, xlab = "Leverage for Home Value", ylab = "Assets")
text(leverage, assets, labels=paste(id), cex = 0.6, pos = 3, col = 2)
#
# Plot of Leverage (hat-value) versus Studentized Residuals
##
plot(leverage, stud_res, xlab = "Leverage for Home Value", ylab = "Studentized Residuals")
text(leverage, stud_res, labels=paste(id), cex = 0.6, pos = 3, col = 2)
#
# Outlier Diagnostics using log-transformed Home
#
mod_loghome <- lm(assets~logHome)
summary(mod_loghome)
leverage <- hatvalues(mod_loghome)
stud_res <- rstudent(mod_loghome)
cook_dist <- cooks.distance(mod_loghome)
df_loghome <-data.frame(logHome, assets, leverage, stud_res, cook_dist)
print(round(df_loghome,4))