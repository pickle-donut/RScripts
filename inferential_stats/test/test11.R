# Clear the Environment
rm(list=ls())
library(ggplot2)
library(dplyr)
library(ggbeeswarm)
library(olsrr)

df <- read.table(file.choose(), header = TRUE, sep = ',')

names(df)

ggplot(data = df, aes(x = SqFeet, y = SalePrice)) + 
  geom_point()


df <- df %>%
  mutate(ln_SalePrice = log(SalePrice)
         , ln_SqFeet = log(SqFeet)
         , ln_Lot = log(Lot))
df %>%
  ggplot(aes(x = ln_SqFeet, y = ln_SalePrice)) + 
  geom_point()

# 2
ggplot(df, aes(x=as.factor(Air), y=ln_SalePrice, color=as.factor(Air))) + 
  # change point size to 0.5 and alpha to 0.8
  geom_beeswarm(cex = 0.5, alpha = 0.8) +
  # add a transparent boxplot on top of points
  geom_boxplot(alpha = 0)
lmod <- lm(ln_SalePrice ~ Air, data = df)
summary(lmod)

# 4 - 8
ggplot(df, aes(x=ln_SqFeet, y=ln_SalePrice, color=as.factor(Air))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

mod1 <- lm(ln_SalePrice ~ ln_SqFeet + Air + ln_SqFeet*Air, data = df)
summary(mod1)
print(anova(mod1))

df %>% 
  filter(Air == 0) %>%
  summarise(mean(ln_SalePrice))

# 9 
mod2 <- lm(SalePrice ~ SqFeet + Air + SqFeet*Air, data = df)
summary(mod2)
print(anova(mod2))

df_pred <- data.frame(SqFeet=2000, Air=1)
pred <- predict(mod2, df_pred)
print(pred)

# 10 
mod3 <- lm(ln_SalePrice ~ ln_SqFeet + ln_Lot + ln_SqFeet*ln_Lot, data = df)
summary(mod3)
print(anova(mod3))

# 11
names_vec <- c('ln_SalePrice', 'ln_SqFeet', 'ln_Lot', 'ln_Lot_ln_SqFeet')
ols_vif_tol(mod3)

# Correaltion before centering variables
A <- cbind(df$ln_SalePrice, df$ln_SqFeet, df$ln_Lot, (df$ln_Lot*df$ln_SqFeet))
colnames(A) <- names_vec
rownames(A) <- names_vec

print(cor(A))

# Center variables
cFt <- df$ln_SqFeet - mean(df$ln_SqFeet)
cLot <- df$ln_Lot - mean(df$ln_Lot)
cLot_cFt <- cLot*cFt

# Correlation Matrix of Centered Variables
B <- cbind(df$ln_SalePrice, cFt, cLot, cLot_cFt)
print (cor(B))

mod4 <- lm(df$ln_SalePrice ~ cFt + cLot + cLot_cFt)
ols_vif_tol(mod4)

# 12 - 13
summary(mod4)
print(anova(mod4))

mean(df$ln_Lot)
mean(df$ln_SqFeet)
