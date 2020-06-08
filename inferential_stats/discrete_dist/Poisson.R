# Clear the Environment
rm(list=ls())

# Read csv file as a DataFrame
#
setwd("C:\\Users\\sarathy\\Desktop\\2019-Teaching\\Fall2019\\Fall2019-MSIS5503\\MSIS-5503-Data")
df <- read.table('ClassData.csv', 
                 header = TRUE, sep = ',')

#Assign variable names to DataFrame Column objects
id <- df$ID
name <- df$Name
age <- df$Age
gender <-df$Gender
education <- df$Education
crediscore <-df$CreditScore
income <- df$Income
networth <-df$NetWorth
sales <-df$Sales
#
#Poisson Distribution
#
#dpois(x, lambda, log = FALSE) where
# x = number  of occurrences in a given time/space interval
# lamdda is mean number of occurrences in a given time/spce interval
#Example:
# Suppose that a customer purchases 12 (i.e., lamda = 12) times a month on average, 
#what is the probability that they will make 180 purchases over the next 12 months? (i.e., x = 15/month)
print(paste("probability that they will make 180 purchases over the next 12 months or 15 per month?", 
            round(dpois(15, 12, log = FALSE),4)))
#
print(paste("probability that they will make  at least 180 purchases over the next 12 months or 15 per month?", 
            1 - round(ppois(14, 12, log = FALSE),4)))
#
#table of probabilities
nsize = 25
result <- vector("numeric", nsize+1)
cum_result <- vector("numeric", nsize+1)
xPx <- vector("numeric", nsize+1)
x2Px <- vector("numeric", nsize+1)
for (i in 0:nsize) {
  result[i+1] <- dpois(i, 12, log = FALSE)
  xPx[i+1] <- i*result[i+1]
  x2Px[i+1] <- i*xPx[i+1]
  cum_result[i+1] <- ppois(i, 12, log = FALSE)
}
round(result[1:nsize+1], 4)
round(cum_result[1:nsize+1], 4)
#
# Mean = sum of xPx
Exp_val = sum(xPx)
print(paste("The Empirical (mean) Expected Value is",round(Exp_val,4), sep = " "))
# Var = sum of X2Px - (sum(xPx)^2)
varian = sum(x2Px) - Exp_val*Exp_val
print(paste("The Empirical Variance is",round(varian,4), sep = " "))
#
# Plot the probabilities
len_result <- length(result)
indx = len_result - 1
#
#Plot the probability mass function
plot((0:indx),result[1:len_result], type = "h", main = "Poisson Probability Plot 
     - Probability of number of purchases per month", 
     xlab = "# purchases/month", ylab = "Probability Mass", 
     col  = "blue", col.lab ="red", lwd=2)
text((0:indx), result[1:len_result], round(result[1:len_result], 4), cex=0.6, pos=4, col="red")

# Plot the cumulative probabilities
len_result <- length(result)
indx = len_result - 1
plot((0:indx),cum_result[1:len_result], type = "h", main = "Poisson Cumulative Probability Plot 
     - Cumulative Probability of number of purchases per month", 
     xlab = "at most # purchases/month", ylab = "Cumulative Probability", 
     col  = "blue", col.lab ="red", lwd=2)
text((0:indx), cum_result[1:len_result], round(cum_result[1:len_result], 4), cex=0.6, pos=4, col="red")

# The average number of children a Spanish woman has in her lifetime is 1.47. 
# Suppose that one Spanish woman is randomly chosen.
#
# Find the probability that she has no children in her lifetime.
print(paste("probability that she has no children in her lifetime", 
            round(dpois(0, 1.47, log = FALSE),4)))
# Find the probability that she has the same or fewer children than the Spanish average.
print(paste("probability that she has the same or fewer children than the Spanish average", 
            round(ppois(1.47, 1.47, log = FALSE),4)))
# Find the probability that she has more children than the Spanish average.
print(paste("probability that she has more children than the Spanish average", 
            1 - round(ppois(1.47, 1.47, log = FALSE),4)))
# Find the probability that she will have 2 more children, given that she already has 7 children
print(paste("probability that she has 2 more children given she has 7", 
            round(dpois(9, 1.47, log = FALSE),5)))
#