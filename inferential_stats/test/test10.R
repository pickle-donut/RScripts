# Clear the Environment
rm(list=ls())
library("ggplot2")
library("dplyr")
library("moments")
library("formula.tools")
library("olsrr")

options(max.print=10000)

# Read csv file as a DataFrame
#
df <- read.table(file.choose(), 
                 header = TRUE, sep = ',')
df$MedSchool <- as.factor(df$MedSchool)
df$Region <- as.factor(df$Region)
str(df)

plotScatter <- function(data, x, y, x_name, y_name, deviation){
  qplot(data=data, x, y, xlab = x_name, ylab = y_name) +
    ggtitle(label = paste(y_name," per ", x_name), subtitle = paste(deviation," times the IQR")) + 
    geom_text(aes(label=ifelse((x>deviation*IQR(x)|y>deviation*IQR(y)),paste(round(x,2), ",", round(y,2)),"")), hjust=1.1) +
    theme_minimal()
}

# 1
## a-b
devs <- c(5)
for (i in 1:length(devs)){
  for(col in 3:ncol(df)) {
    print(plotScatter(df, df[, col], df[, 2], names(df)[col], "InfctRsk", devs[i]))
  }
}

# 2
## a-b
df <- df %>% 
  mutate(lculture = log(Culture), lnurses = log(Nurses))
df

devs <- c(5)
for (i in 1:length(devs)){
  for(col in 13:ncol(df)) {
    print(plotScatter(df, df[, col], df[, 2], names(df)[col], "InfctRsk", devs[i]))
  }
}

# 1
## c
modFormula1 <- InfctRsk ~ Culture + Xray + Stay + Nurses
mod1 <- lm(modFormula1, data = df) 
summary(mod1)

mod1_rstand <-rstandard(mod1)
qqnorm(mod1_rstand, ylab="Standardized Residuals of Assets", xlab="Normal Quantiles")
qqline(mod1_rstand)
hist(mod1_rstand)
print(skewness(mod1_rstand))
print(kurtosis(mod1_rstand))

# Kolomogorov-Smirnov Test
print(ks.test(mod1_rstand,"pnorm"))

# Shapiro-Wilk Test
print(shapiro.test(mod1_rstand))

## d
# Getting Standardized Predictions for Each Model
mod1_pred <- predict(mod1)
mod1_pstand <-  (mod1_pred - mean(mod1_pred))/sd(mod1_pred)

# Plot
plot(mod1_pstand, mod1_rstand, 
     main = paste("Model 1: ", as.character(modFormula1),
                  "\nStandardized Predictions vs Standardized Residuals"),
     xlab = "Standardized Predictions",
     ylab = "Standardized Residuals",
     xlim = c(-3, 3))
abline(h=0, col="red")

## f
# Collinearity diagnostics using library "olsrr"
ols_vif_tol(mod1)

## e
# Outlier Diagnostics
mod_home <- lm(modFormula1, data = df)
summary(mod_home)
leverage <- hatvalues(mod_home)
stud_res <- rstudent(mod_home)
cook_dist <- cooks.distance(mod_home)

# Make table
df_home <-data.frame(df$ID, df$InfctRsk, df$Culture, df$Xray, df$Stay, df$Nurses, leverage, stud_res, cook_dist)
print(round(df_home,4))

df_home <- df_home %>% 
  arrange(desc(abs(stud_res))) %>%
  mutate(label = if_else(df.ID %in% c(8), df.ID, NULL))
print(df_home)

# Plot of Leverage 
plot(df_home$leverage, df_home$InfctRsk, main = "Plot of Leverage", xlab = "Leverage for All Model Inputs", ylab = "InfctRsk")
text(df_home$leverage, df_home$InfctRsk, labels=df_home$label, cex = 0.6, pos = 3, col = 2)

# Plot of Leverage (hat-value) versus Studentized Residuals
plot(leverage, stud_res, main = "Plot of Leverage (hat-value) versus Studentized Residuals", xlab = "Leverage for All Model Inputs", ylab = "Studentized Residuals")
text(leverage, stud_res, labels=df_home$label, cex = 0.6, pos = 3, col = 2)

#####################################################################

# 2
## c
modFormula2 <- InfctRsk ~ lculture + Xray + Stay + lnurses
mod2 <- lm(modFormula2, data = df)
summary(mod2)

mod2_rstand <-rstandard(mod2)
qqnorm(mod2_rstand, ylab="Standardized Residuals of Assets", xlab="Normal Quantiles")
qqline(mod2_rstand)
hist(mod2_rstand)
print(skewness(mod2_rstand))
print(kurtosis(mod2_rstand))

# Kolomogorov-Smirnov Test
print(ks.test(mod2_rstand, "pnorm"))

# Shapiro-Wilk Test
print(shapiro.test(mod2_rstand))

## d
# Getting Standardized Predictions for Each Model
mod2_pred <- predict(mod2)
mod2_pstand <-  (mod2_pred - mean(mod2_pred))/sd(mod2_pred)

# Plot
plot(mod2_pstand, mod2_rstand, 
     main =  paste("Model 1: ", as.character(modFormula2),
                   "\nStandardized Predictions vs Standardized Residuals"),
     xlab = "Standardized Predictions",
     ylab = "Standardized Residuals",
     xlim = c(-3, 3))
abline(h=0, col="red")

## f
# Collinearity diagnostics using library "olsrr"
ols_vif_tol(mod2)

## e
# Outlier Diagnostics
mod_home <- lm(modFormula2, data = df)
summary(mod_home)
leverage <- hatvalues(mod_home)
stud_res <- rstudent(mod_home)
cook_dist <- cooks.distance(mod_home)

# Make table
df_home <-data.frame(df$ID, df$InfctRsk, df$lculture, df$Xray, df$Stay, df$lnurses, leverage, stud_res, cook_dist)
print(round(df_home,4))

df_home <- df_home %>% 
  arrange(desc(abs(stud_res))) %>%
  mutate(label = if_else(df.ID %in% c(54), df.ID, NULL))
print(df_home)

# Plot of Leverage 
plot(df_home$leverage, df_home$InfctRsk, main = "Plot of Leverage", xlab = "Leverage for All Model Inputs", ylab = "InfctRsk")
text(df_home$leverage, df_home$InfctRsk, labels=df_home$label, cex = 0.6, pos = 3, col = 2)

# Plot of Leverage (hat-value) versus Studentized Residuals
plot(leverage, stud_res, main = "Plot of Leverage (hat-value) versus Studentized Residuals", xlab = "Leverage for All Model Inputs", ylab = "Studentized Residuals")
text(leverage, stud_res, labels=df_home$label, cex = 0.6, pos = 3, col = 2)