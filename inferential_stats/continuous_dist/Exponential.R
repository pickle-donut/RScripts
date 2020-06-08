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
#Exponential Distribution
#Example: The amount of time spouses shop for anniversary cards can be 
# modeled by an exponential distribution # with the average amount of time 
#equal to eight minutes. Hence, lambda parameter = number of occurrences per minute = 1/8
# X = rv = time elapsed before a card is bought
# Let us plot the time taken to shop for a card over a time horizon of 60 minutes
#
#dexp(x, rate = 1, log = FALSE) - Not used in c
#pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
#qexp(p, rate = 1, lower.tail = TRUE, log.p = FALSE)
#
print(paste("Probability that the person spent (at most) 10 minutes shopping for a card is ",
            round(pexp(10, 0.125, lower.tail = TRUE, log= FALSE),4) ))
#
print(paste("Probability that the person spent between 5 and 7 minutes shopping for a card is ",
            round(pexp(7, 0.125, lower.tail = TRUE, log= FALSE) - pexp(5, 0.125, lower.tail = TRUE, log= FALSE),4) ))
#
#table of probabilities for select values of X from 0 to 59.
nsize = 60
result <- vector("numeric", nsize+1)
cum_result <- vector("numeric", nsize+1)
#
for (i in 0:nsize) {
  result[i+1] <- round(dexp(i, 0.125, log = FALSE),4)
  cum_result[i+1] <- round(pexp(i, 0.125, lower.tail = TRUE, log = FALSE),4)
}
# Plot the probabilities
len_result <- length(result)
indx = len_result - 1
#Plot the probability mass function
plot((0:indx),result, 
      type = "l", 
      main = "Exponentially Distributed Shopping Time - Average 8 minutes 
     - Probability Density", 
      xlab = "Shopping Time in minutes", 
      ylab = "Probability Density", 
      col  = "blue", 
      col.lab ="red", 
      lwd=2)
# Plot the cumulative probabilities
plot((0:indx),cum_result,
      type = "l", 
      main = "Exponentially Distributed Shopping Time - Average 8 minutes 
     - Cumulative Probability", 
      xlab = "Shopping Time at most (in minutes)", 
      ylab = "Cumulative Probability", 
      col  = "blue", 
      col.lab ="red", lwd=2)
#
# 5th, 50th, and 95th percentiles
print(paste("The 5th percentile is ", round(qexp(0.05, 0.125, lower.tail = TRUE, log.p = FALSE),4)))
print(paste("The 5oth percentile is ", round(qexp(0.50, 0.125, lower.tail = TRUE, log.p = FALSE),4)))
print(paste("The 95th percentile is ", round(qexp(0.95, 0.125, lower.tail = TRUE, log.p = FALSE),4)))
#
#cA web site experiences traffic during normal working hours at a rate of 12 visits per hour. 
# Assume that the duration between visits has the exponential distribution. i.e., X ~ Expon(0.2/ minute)
print(paste("probability that the duration between two successive visits to the web site is more than 10 minutes", 
            1 - round(pexp(10, 0.2, lower.tail = TRUE, log = FALSE),4)))
print(paste("The top 25% of durations between visits are at least - in minutes", 
            round(qexp(0.75, 0.2, lower.tail = TRUE, log = FALSE),4)))
print(paste("probability that the duration of next visit is less than 5 minutess", 
            round(pexp(5, 0.2, lower.tail = TRUE, log = FALSE),4)))
print(paste("probability less than 7 visits in 1 hour", 
            round(ppois(7, 12, lower.tail = TRUE, log = FALSE),4)))
