# Clear the Environment
rm(list=ls())

# Read csv file as a DataFrame
#
setwd("C:\\Users\\jonmc\\Documents\\git\\RScripts\\inferential_stats\\data\\")
df <- read.table('Admit-Reg.csv', 
                 header = TRUE, sep = ',')
print(head(df))
admit <- df$admit
gre <- df$gre
gpa <- df$gpa
rank <- df$rank
mod1 <- lm(admit ~ gre+gpa+rank)
summary(mod1)
pred_admit <- predict(mod1)
#
# install.packages("scatterplot3d")
library(scatterplot3d)

scatterplot3d(gre, gpa, admit, highlight.3d=TRUE, col.axis="blue",
                     col.grid="lightblue", main="admit for gre, gpa", pch=20)
scatterplot3d(gre, gpa, pred_admit, highlight.3d=TRUE, col.axis="blue",
                     col.grid="lightblue", main="admit for gre, gpa", pch=20)
#
# Logistic Regression
#
logimod1 <- glm(admit ~ gre + gpa + rank, data = df, family = "binomial")
summary(logimod1)
df_a <- data.frame(gre=800, gpa=4, rank=1)
p_log_odds <- predict(logimod1, df_a)
p_odds <- exp(p_log_odds)
p_prob <- (p_odds/(1+p_odds))
#
print(paste("Predicted Log Odds = ", round(p_log_odds,4),
            "Predicted Odds Ratio = ", round(p_odds,4),
            "Predicted Probability of Success = ",round(p_prob,4)))
#
#
p_log_odds <- predict(logimod1)
p_odds <- exp(p_log_odds)
prob_admit <- (p_odds/(1+p_odds))
df_pred <- data.frame(gre, gpa, rank, admit, prob_admit)
print(head(df_pred))
#
glm.probs <- predict(logimod1,type = "response")
glm.probs[1:5]
#
# Classification Using Logistic Regression
#
pred_admit <- ifelse(glm.probs >= 0.5,1,0)
df_class <- cbind(admit, pred_admit)
print(head(df_class))
class_tbl <- xtabs(~ admit + pred_admit, data = df_class)
class_pct <- class_tbl/length(admit)
print(class_tbl)
print(class_pct)
print(paste("Correct classification Rate (percent): ",
            (class_pct[1,1] + class_pct[2,2])*100))
#
# Using rank as a categorical variable or factor

df$frank <- factor(df$rank)
logimod2 <- glm(admit ~ gre + gpa + frank, data = df, family = "binomial")
summary(logimod2)
#
df_s <- with(df, data.frame(gre =800, gpa = 3.5, frank = factor(1:4)))
prob_admit <- predict(logimod2,df_s, type = "response")
print(paste("Probability of admit in school with different ranks ", round(prob_admit,4)))
#
# Overall Model Test using Chi-Square Test
#
with(logimod2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#
# Multinomial (or polytomous) Logistic Regression
#
dfm <- read.table('hsbdemo.csv', 
                 header = TRUE, sep = ',')
print(head(dfm))
tbl <- with(dfm, table(ses, prog))
chisq.test(tbl)
prog <- dfm$prog
ses <- dfm$ses
write <- dfm$write
#
#install.packages("nnet")
library(nnet)
#
# Set academic as reference category for multinomial dependent variable prog
#
prog <- relevel(prog, ref = "academic")
#
# Set low as reference category for predictor variable ses
#
ses <- relevel(ses, ref = "low")
#
# Specify the polytomous logistic regression model
#
mmod1 <- multinom(prog ~ ses + write)
#
# Model summary
summary(mmod1)
#
# Wald's Test of Coefficients
#
z_value <- summary(mmod1)$coefficients/summary(mmod1)$standard.errors
print(z_value)
p_value <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p_value)
#
# Predictions from Model
#
#  Probability of being in the three different programs when write = 40, ses = low;
#
dses <- data.frame( write = 40,ses = "low")
print(dses)
predict(mmod1, newdata = dses, "probs")
#
# Predicted Probabilities for whole data set
#
dfp <- data.frame(ses, write, prog)
dfp$pp <- fitted(mmod1)
print(head(dfp))
#
pp_academic <- c(dfp$pp[,1])
print(head(pp_academic))
#
library(ggplot2)
#
#
ggplot(dfp, aes(x=write, y=pp_academic, shape=ses, color=ses)) + 
  geom_point() +
  ggtitle("Predicted Probability of Academic School vs Writing Score for each SES") +
  xlab("Writing Score") +
  ylab("Predicted Probability of Academic School")
#
head(dfp$pp)
#
# Choose COLUMN NAME (academic, general, vocation) from df$pp with the highest probability
#
dfp$p_class <- colnames(dfp$pp)[apply(dfp$pp,1,which.max)]
#
# Classify as 1 if this column name matches the actual prog value
#
dfp$classif <- ifelse(dfp$prog == dfp$p_class, 1, 0)
print(head(dfp))
#
# Develop Classification Table
#
classif_tbl <- xtabs(~ prog + p_class, data = dfp)
print(classif_tbl)
#
classif_pct <- classif_tbl/length(prog)
print(classif_pct)
print(paste("Correct classification Rate (percent): ",
            (classif_pct[1,1] + classif_pct[2,2] + classif_pct[2,2])*100))
