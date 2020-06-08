# Clear the Environment
rm(list=ls())

# Read csv file as a DataFrame
#
setwd("C:\\Users\\sarathy\\Documents\\2019-Teaching\\Fall2019\\Fall2019-MSIS5503\\MSIS-5503-Data")
df <- read.table('ClassData.csv', 
                 header = TRUE, sep = ',')

#Assign variable names to DataFrame Column objects
id <- df$ID
name <- df$Name
age <- df$Age
gender <-df$Gender
education <- df$Education
creditscore <-df$CreditScore
income <- as.numeric(df$Income)
networth <-as.numeric(df$NetWorth)
sales <-as.numeric(df$Sales)

# Mean of Income
print(paste("The mean of Income is: ", round(mean(income), 2)))

# Median of Income
print(paste("The median of Income is: ", round(median(income), 2)))

# Frequency table to obtain mode
table(income)

# Mean of Income for Males
print(paste("The mean of Male Income is: ", round(mean(income[gender=="M"]), 2)))

# Mean of Income for Females
print(paste("The mean of Female Income is: ", round(mean(income[gender=="F"]), 2)))

deviation <- vector("numeric", 10)
sq_deviation <- vector("numeric", 10)
sum_sq_deviation = 0
# Measures of dispersion
for (i in 1:10) {
  deviation[i] <- income[i] - mean(income)
  sq_deviation[i] <- deviation[i]^2
  sum_sq_deviation <- sq_deviation[i] + sum_sq_deviation
}

print(paste("The sum of squared deviations from mean is ", sum_sq_deviation))
print(paste("The sample variance is ", sum_sq_deviation/9))
print(paste("The sample variance using var() function is ", var(income)))
print(paste("The sample standard deviation is ", sqrt(sum_sq_deviation/9)))
print(paste("The sample standard deviation using sd() is ", sd(income)))


# print sorted income to see the percentiles
print("Sorted Income")
print(income[order(income)])
quantile(income, probs = seq(0, 1, 0.05), na.rm = FALSE, names = TRUE, type = 2)
#
income_iqr <- IQR(income, type = 2)
print(income_iqr)
p25 <- quantile(income, probs = 0.25, na.rm=FALSE, names = TRUE, type=2)
p75 <- quantile(income, probs = 0.75, na.rm=FALSE, names = TRUE, type=2)
print(paste(min(income)," p25 ",p25, median(income), " p75 ",p75, " ", max(income)))
llimit <- p25 - 1.5*income_iqr
ulimit <- p75 + 1.5*income_iqr
print(paste("Lower limit ", llimit, " Upper limit ", ulimit))

print(boxplot.stats(income)$stats)
#
boxplot(income/1000, main="Box Plot for Income", 
        xlab="Income (in thousands) dollars",
        border="blue", 
        col="green",
        horizontal = TRUE) 
text(x=boxplot.stats(income/1000)$stats, labels = boxplot.stats(income/1000)$stats, y = 1.25)
#
#
hist(income/1000, main="Histogram of Income in 1000's dollars", 
     xlab="Income in 1000's dollars", 
     border="blue", 
     col="green",
     xlim=c(0,500),
     las=2, 
     breaks=seq(0,500,100))



