# Clear the Environment
rm(list=ls())

# 1
2*(1-pt(46.203, 47))

# 2e - Pt IV
2*(pt(-0.3523, 8, lower.tail = TRUE))

# 3
df <- read.csv(file.choose())
str(df)

# a
mod <- lm(Deaths ~ Consumption, data = df)
summary(mod)
df$P_Deaths <- predict(mod)
df$R_Deaths <- residuals.lm(mod)

print(mod)
print(df)

# b
plot(df$Consumption,df$Deaths,col="red", 
     xlab="Consumption", 
     ylab="Deaths", 
     xlim = c(0, 10), ylim= c(0, 325))
text(df$Consumption, df$Deaths, df$Deaths, cex=0.6, pos=3)
#
# Add Predicted Values to the Plot
# 
par(new=TRUE)
plot(df$Consumption, df$P_Deaths,type="l",
     yaxt='n', ann=FALSE, col="blue",  xlim = c(0, 10), ylim= c(0, 325))

anova_mod <- anova(mod)
print(anova_mod)
