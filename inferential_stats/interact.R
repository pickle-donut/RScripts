# Clear the Environment
rm(list=ls())
library(MASS)
# Read csv file as a DataFrame
#
setwd("C:\\Users\\jonmc\\Documents\\git\\RScripts\\inferential_stats\\data\\")
#
df <- read.table('birth-weight.csv', 
                 header = TRUE, sep = ',')

#Assign variable names to DataFrame Column objects
low <- df$low
age <-df$age
lwt <- df$lwt
race<-df$race
smoke <- df$smoke
ptl<- df$ptl
ht <-df$ht
ui <- df$ui
ftv <- df$ftv
bwt <- df$bwt
#
library(ggplot2)
#
ggplot(df, aes(x=age, y=bwt, shape=race, color=race)) + 
geom_point() + 
geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Baby Birth Weight vs Mother's Age by Race") +
  xlab("Mother's Age in Years") +
  ylab("Baby's Birth Weight in Grams")
#
# Interaction between a Continuous predictor and a Categorical predictor
#
#
d_black <- ifelse(race == "black", 1, 0)
#
fac_black <- factor(d_black) # fac_black is only needed for ggplot - 
# the regression model uses d_black as dummy variable
#
#
ggplot(df, aes(x=age, y=bwt, shape=fac_black, color=fac_black)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Baby Birth Weight vs Mother's Age by Race - Black vs Others") +
  xlab("Mother's Age in Years") +
  ylab("Baby's Birth Weight in Grams")
#
# Regression Model with Interaction Term
#
mod1 <- lm(bwt ~ age+d_black+age*d_black)
summary(mod1)
#
#
# Regression Model with Interaction Term - Black vs White
#
dfbw <- subset(df, race == "black" | race == "white", select=c(bwt, age, race))
d_bw <- ifelse(dfbw$race == "black", 1, 0)
fac_d_bw = factor(d_bw)
#
ggplot(dfbw, aes(x=age, y=bwt, shape=fac_d_bw, color=fac_d_bw)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Baby Birth Weight vs Mother's Age by Race - Black vs White") +
  xlab("Mother's Age in Years") +
  ylab("Baby's Birth Weight in Grams")
#
#
mod_bw <- lm(bwt ~ age+d_bw+age*d_bw, data=dfbw)
summary(mod_bw)
#
# Interaction between Continuous Predictors
#
df1 <- read.table('icecream.csv', 
                 header = TRUE, sep = ',')
print(df1)
hi_inc <- factor(ifelse(df1$income >85, 1, 0))
ggplot(df1, aes(x=price, y=IC, shape=hi_inc, color=hi_inc)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Per Capita Ice Cream Consumption vs Price - High Income vs Low Income") +
  xlab("Price per Pint in Dollars") +
  ylab("Ice Cream Consumption in Pints")
#
hi_price <- factor(ifelse(df1$price > 0.27,1,0))
ggplot(df1, aes(x=income, y=IC, shape=hi_price, color=hi_price)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Per Capita Ice Cream Consumption vs Weekly Income - High Price vs Low Price") +
  xlab("Per Capita Weekly Income in Dollars") +
  ylab("Ice Cream Consumption in Pints")
#
# Regression model with interaction
#
mod2 <- lm(IC ~ price+income+price*income, data = df1)
summary(mod2)
print(anova(mod2))
#
# Prediction of Ice Cream consumption by a low income consumer for high priced ice-cream
#
df_a <- data.frame(price=0.292, income=76)
print(predict(mod2, df_a))
# Prediction of Ice Cream consumption by a High income consumer for high priced ice-cream
#
df_b <- data.frame(price=0.292, income=96)
print(predict(mod2, df_b))
#
# Multicollinearity in Interaction Models
#
library(olsrr)
ols_vif_tol(mod2)
#
# Creating Centered Variables and Interaction Term
#
price <- df1$price
income <- df1$income
pr_inc <- price*income
A <- cbind(price, income, pr_inc)
print (cor(A))
c_price <- price - mean(price)
c_income <- income - mean(income)
cpr_cinc <- c_price*c_income
#
# Correlation Matrix of Centered Variables
B <- cbind(c_price, c_income, cpr_cinc)
#
print (cor(B))
#
#
mod3 <- lm(IC ~ c_price+c_income+cpr_cinc, data = df1)
summary(mod3)
print(anova(mod3))
ols_vif_tol(mod3)
#
# Final centered model with only significant interaction term
# 
mod4 <- lm(IC ~ cpr_cinc, data = df1)
summary(mod4)