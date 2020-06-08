df <- read.table(file.choose(), header = TRUE, sep = ',')

## 1
# a - b
logimod1 <- glm(gender ~ height + hand + foot, data = df, family = "binomial")
summary(logimod1)
newdata1 <- with(df, data.frame(height = mean(height), hand = mean(hand), foot = mean(foot)))
newdata1$P <- predict(logimod1, newdata = newdata1, type = "response")
newdata1

# c
df_a <- data.frame(height = 1680, hand = 200, foot = 250)
p_log_odds <- predict(logimod1, df_a)
p_odds <- exp(p_log_odds)
p_prob <- (p_odds/(1+p_odds))

print(paste("Predicted Log Odds = ", round(p_log_odds,4),
            "Predicted Odds Ratio = ", round(p_odds,4),
            "Predicted Probability of Success = ",round(p_prob,4)))

# d
glm.probs <- predict(logimod1, type = "response")
glm.probs[1:5]

pred_gender <- ifelse(glm.probs >= 0.5,1,0)
df_class <- cbind(df$gender, pred_gender)
print(head(df_class))
class_tbl <- xtabs(~ df$gender + pred_gender, data = df_class)
class_pct <- class_tbl/length(df$gender)
print(class_tbl)
print(class_pct)
print(paste("Correct classification Rate (percent): ",
            (class_pct[1,1] + class_pct[2,2])*100))
print(paste("Sensitivity for Male Predicted Class: ",
            (class_pct[2,2]/(class_pct[2,2]+class_pct[2,1]))*100))
print(paste("Specificity for Male Predicted Class: ",
            (class_pct[1,1]/(class_pct[1,1]+class_pct[1,2]))*100))

# e
logimod2 <- glm(gender ~ height, data = df, family = "binomial")
summary(logimod2)
newdata2 <- with(df, data.frame(height = mean(height)))
newdata2$P <- predict(logimod2, newdata = newdata2, type = "response")
newdata2

df_a <- data.frame(height = 1680)
p_log_odds <- predict(logimod2, df_a)
p_odds <- exp(p_log_odds)
p_prob <- (p_odds/(1+p_odds))

print(paste("Predicted Log Odds = ", round(p_log_odds,4),
            "Predicted Odds Ratio = ", round(p_odds,4),
            "Predicted Probability of Success = ",round(p_prob,4)))

glm.probs <- predict(logimod2, type = "response")
glm.probs[1:5]

pred_gender <- ifelse(glm.probs >= 0.5,1,0)
df_class <- cbind(df$gender, pred_gender)
print(head(df_class))
class_tbl2 <- xtabs(~ df$gender + pred_gender, data = df_class)
class_pct2 <- class_tbl/length(df$gender)
print(class_tbl2)
print(class_pct2)
print(paste("Correct classification Rate (percent): ",
            (class_pct2[1,1] + class_pct2[2,2])*100))
print(paste("Sensitivity for Male Predicted Class: ",
            (class_pct2[2,2]/(class_pct2[2,2]+class_pct2[2,1]))*100))
print(paste("Specificity for Male Predicted Class: ",
            (class_pct2[1,1]/(class_pct2[1,1]+class_pct2[1,2]))*100))

# g
with(logimod1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# h
with(logimod1, pchisq(null.deviance - 1.49, df.null - 2, lower.tail = FALSE))

## 2
# a
dfm <- read.table(file.choose(),
                  header = TRUE, sep = ',')
library(nnet)

prog <- dfm$prog
ses <- dfm$ses
write <- dfm$write
math <- dfm$math
science <- dfm$science

# Set general as reference category for multinomial dependent variable prog
prog <- relevel(prog, ref = "general")

# Set low as reference category for predictor variable ses
ses <- relevel(ses, ref = "low")

# Specify the polytomous logistic regression model
mmod1 <- multinom(prog ~ ses + math + science + write)
summary(mmod1)


dses <- data.frame(ses = "high", math = 37, science = 37, write = 37)
print(dses)
predict(mmod1, newdata = dses, "probs")
