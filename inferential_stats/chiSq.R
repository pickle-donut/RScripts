# Clear the Environment
rm(list=ls())
#
# The Chi-Square Test for Independence
#
print(paste("The probability value > 12.5685,in a Chi-square distribution with (4) df is:"
            ,round(1 - pchisq(12.5685,4),4)))
#
#
college <- c("Community College", "Four_Year College", "Non-Students")
hours <- c("1-3","4-6", "7-9")
m <- cbind(c(111, 96, 91), c(96, 133, 150), c(48, 61, 53))
rownames(m) <- college
colnames(m) <- hours
print(m)
#
tbl <- as.table(m)
print(tbl)
ch <- chisq.test(tbl)
print(ch)
print(ch$residuals)
print(ch$expected)
#
# The Chi-square Goodness of Fit
#
absences <- c("0-2", "3-5", "6-8","9-11", "12+")
expected <- c(50,30,12,6,2)
probab <- expected/sum(expected)
print(probab)
actual <- c(35, 40, 20, 1, 4)
chisq.test(actual,p = probab)
print(round(1 - pchisq(22.33,4),4))
#
observed <- c(20, 57, 23)
expected_prob <- c(.25, .5, .25)
chisq.test(observed,p = expected_prob)
print(paste("The probability value > 2.14,in a Chi-square distribution with (2) df is:"
            ,round(1 - pchisq(2.14,2),4)))
#
# Chi-Square Test for Homogeneity
#
male <- c(72, 84, 49, 45)
female <- c(91, 86, 88, 35)
tbl <- cbind(male, female)
chisq.test(tbl)