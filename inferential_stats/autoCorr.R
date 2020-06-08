# Clear the Environment
rm(list=ls())

# Read csv file as a DataFrame
#
setwd("C:\\Users\\sarathy\\Documents\\2019-Teaching\\Fall2019\\Fall2019-MSIS5503\\MSIS-5503-Data")
df <- read.table('Microsoft.csv', 
                 header = TRUE, sep = ',')
print(head(df))
#
mod1 <- lm(Revenues ~ Marketing, data=df)
summary(mod1)
resid1 <- residuals(mod1)
obs <- df$Obs
#
plot (obs, resid1,
      main= "Residuals from lm(Revenues ~ Marketing) Microsoft from Q1 - 1987 to Q3 2000",
      xlab = "Quarter",
      ylab = "Residuals",
      type = "l")
#
lnRevenues <- log(df$Revenues)
#
mod2 <- lm(lnRevenues ~ Marketing, data=df)
summary(mod2)
resid2 <- residuals(mod2)
plot (obs, resid2,
      main= "Residuals from lm(Revenues ~ lnMarketing) Microsoft from Q1 - 1987 to Q3 2000",
      xlab = "Quarter",
      ylab = "Residuals",
      type = "l")
#
#
st_resid2 <- rstandard(mod2)
st_resid2_l1 <- c(NA, head(st_resid2, -1))
M <- cbind(st_resid2, st_resid2_l1)
print(M)
print(cor(na.omit(M)))
#
plot(st_resid2, st_resid2_l1,
     main= "Standardized Lagged Residuals vs Standardized Residuals",
     xlab = "Standardized Residuals",
     ylab = "Lag1 Standardized Residuals")
#
# install.packages("lmtest")
library(lmtest)
dwtest(mod2, alternative = c("greater"))
#
mod3 <- lm(lnRevenues ~ obs)
summary(mod3)
st_resid3 <- rstandard(mod3)
plot (obs, st_resid3,
      main= "Residuals from lm(lnRevenues ~ obs) Microsoft from Q1 - 1987 to Q3 2000",
      xlab = "Quarter",
      ylab = "Residuals",
      type = "l")
dwtest(mod3, alternative = c("greater"))
#
#
obs_sq <- obs*obs
#
mod4 <- lm(lnRevenues ~ obs+obs_sq)
summary(mod4)
st_resid4 <- rstandard(mod4)
plot (obs, st_resid4,
      main= "Residuals from lm(lnRevenues ~ obs+obs_sq) Microsoft from Q1 - 1987 to Q3 2000",
      xlab = "Quarter",
      ylab = "Residuals",
      type = "l")
dwtest(mod4, alternative = c("greater"))
#
#
mod5 <- lm(lnRevenues ~ obs+obs_sq+Marketing, data=df)
summary(mod5)
st_resid5 <- rstandard(mod5)
plot (obs, st_resid5,
      main= "Residuals from lm(lnRevenues ~ obs+obs_sq+Marketing) Microsoft from Q1 - 1987 to Q3 2000",
      xlab = "Quarter",
      ylab = "Residuals",
      type = "l")
dwtest(mod5, alternative = c("greater"))
#