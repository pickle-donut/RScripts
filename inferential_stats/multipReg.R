# Clear the Environment
rm(list=ls())

# Read csv file as a DataFrame
#
setwd("C:\\Users\\jonmc\\Documents\\git\\RScripts\\inferential_stats\\data\\")
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
data <- cbind(age,home, mortgage, assets)
corr <- cor(data)
print(signif(corr, 4))
#
# Calculate correlations using cor() and partial correlations using pcor()
# for Home Value, Mortgage and Assets
# 
# install.packages(ppcor)
library(ppcor)
pdata <- cbind(home, mortgage, assets)
corr <- cor(pdata)
print(signif(corr, 4))
pcorr <- pcor(pdata)
print(pcorr)
#stall.librar
# Multiple Regression - Predict Assets using Age, Home Value and Mortgage Balance _ Original Model
#
m_reg1 <- lm(assets ~ age+home+mortgage)
summary(m_reg1)
print(anova(m_reg1))
#
# Creating plots of Assets and Overlaying Prediction lines for each of the three predictors
#
par(mfrow=c(3,1))
plot(age,assets, main="Scatterplot of Assets vs. Age")
plot(home,assets, main="Scatterplot of Assets vs Home Value")
plot(mortgage,assets, main="Scatterplot of Assets vs. Mortgage Balance")
#
# Simple Regression - Predict Assets using only Home Value
#
m_reg2 <- lm(assets ~ home)
summary(m_reg2)
#
# Predict Assets using m_reg1 - Original Model 
# Predict Assets using Age = 50, Home Value = 100 (in 1000's of dollars) 
# and Mortgage Balance = 50 (in 1000's of dollars)
#
df_p <- data.frame(age=50, home=100, mortgage=50)
print(predict(m_reg1, df_p))
#
#
# Develop Predicted Values and Residuals for all predictor values in the data set
#
df$Pred_assets <- predict(m_reg1)
df$Resid_assets <- residuals(m_reg1)
print(head(df))
#
#
# Stepwise Regression using olsrr library
#
library(olsrr)
df1 <- data.frame(assets, age, home, mortgage)
step_model <- lm(assets ~ ., data=df1)

#ols_step_forward_p(step_model, penter=0.10, details=FALSE)
#ols_step_backward_p(step_model, prem=0.30, details=FALSE)
ols_step_both_p(step_model, penter = 0.10, prem = 0.30, details = FALSE)

#
# The Standardized Multiple Regression Model
#
z_assets <- (assets - mean(assets))/sd(assets)
z_age <- (age - mean(age))/sd(age)
z_home <- (home - mean(home))/sd(home)
z_mortgage <- (mortgage - mean(mortgage))/sd(mortgage)
#
z_m_reg1 <- lm(z_assets ~ z_age+z_home+z_mortgage)
summary(z_m_reg1)
#
# Simple Regression with Categorical Predictors vs t-test
#
catm_reg <-  lm(assets ~ gender)
summary(catm_reg)
an_catm_reg <- anova(catm_reg)
print(an_catm_reg)
#
# Create Male and Female Groups
#
male_assets <- subset(df$Assets, df$Gender == 1)
female_assets <- subset(df$Assets, df$Gender == 0)
#
# Perform Independent sampel two-sided t-test
#
ttest <- t.test(male_assets, female_assets, alternative = c("two.sided"),
       mu = 0, paired = FALSE, conf.level = 0.95)
print(ttest)
#
# Multiple Regression with categorical Predictors
#
catm_reg1 <- lm(assets ~ gender+marital)
summary(catm_reg1)
an_catm_reg1 <- anova(catm_reg1)
print(an_catm_reg1)
#
# Multiple Regression with Categorical and Continuous Predictors
#
# First let us plot assets against age for the gender group
#
# Create cgender as a factor from the integer variable gender, to be used in ggplot
#
cgender <- factor(gender)
#
# Use the ggplot2 library
#
library(ggplot2)
#
# ggplot() requires data in the data frame; we will use our original data frame df
#
# aes in ggplot provides the axes. Setting color and shape to the cgender variable allows
# the identification of the gender group in the plot
# The geom_point() function allows the color, size and shape of the points to be set.
#
ggplot(df, aes(x=age, y=assets, shape=cgender, color=cgender)) + geom_point() 
#
# Now develop the actual multiple regression model with genderand age
#
ccm_reg1 <- lm(assets ~ gender+age)
summary(ccm_reg1)
an_ccm_reg1 <- anova(ccm_reg1)
print(an_ccm_reg1)
#
# Plot of predicted assets for each gender, by age
#
pred_ccm <- predict(ccm_reg1)
ggplot(df, aes(x=age, y=pred_ccm, shape=cgender, color=cgender)) + geom_point() 
#
# Multiple Regression Model for assets with 
# gender and marital (categorical) and 
# age and mortgage (continuous)
#
m_reg3 <- lm(assets ~gender+marital+age+mortgage)
summary(m_reg3)
an_m_reg3 <- anova(m_reg3)
print(an_m_reg3)
#
# Predict the Expected Value of Assets for an Unmarried Female, 
# Aged 50 with a Mortgage Balance of 100 (,in 1000's of dollars).
#
# This is a point estimate for the Expected Value of Assets in the Population
# for the given X-vector.
x_vals <- data.frame(gender=0,marital=0,age=50,mortgage=100)
print(predict(m_reg3, x_vals))
#
#
# 95% Confidence Interval for the Expected Value of Y in the population, for the above values of X
#
print(predict(m_reg3, x_vals, interval="confidence", level=0.95))
#
# 95% Prediction Interval for the actual value of Y in the population, for above values of X
#
print(predict(m_reg3, x_vals, interval="predict", level=0.95))
#
# Create a data frame with gender, marital, age, mortagage and assets -- df2
# Confidence and Prediction Intervals for the whole data set - conf_df
# Create data frames to store the confidence and prediction intervals - pred_df
# Then merge the specific columns from conf_df and pred_df with df2 to create data frame df3.
#
df2 <- data.frame(gender, marital, age, mortgage, assets)
conf_df <- data.frame(predict(m_reg3, df2, interval="confidence", level=0.05))
pred_df <- data.frame(predict(m_reg3, df2, interval="prediction", level=0.05))
df3 <- data.frame(df2, conf_df$fit, conf_df$lwr, conf_df$upr, pred_df$lwr, pred_df$upr)
print(head(df3))
#

