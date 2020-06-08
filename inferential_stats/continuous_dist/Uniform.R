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
# Uniform Distribution
#
# dunif(x, min, max, log = FALSE) where
# x = unifromly distributed rv
#xExample: The amount of time, in minutes, that a person must wait for a bus 
# is uniformly distributed between zero and 15 minutes, inclusive.
#Here, a = 0 and b = 15
#
#table of probabilities for select values of X
nsize = 15
result <- vector("numeric", nsize+1)
cum_result <- vector("numeric", nsize+1)
for (i in 0:nsize) {
  result[i+1] <- dunif(i, 0, 15, log = FALSE)
  cum_result[i+1] <- punif(i, 0, 15, log = FALSE)
}
# Plot the probabilities
len_result <- length(result)
indx = len_result - 1
#
#Plot the probability mass function
plot((0:indx),result,
      xlim=c(0, 15),                                            #-- Specify Limits for the X-axis
      type = "l", 
      main = "Uniformly Distributed Wait Time between 0 and 15 
     - Probability Density", 
      xlab = "Wait Time in minutes", 
      ylab = "Probability Density", 
      col  = "blue", 
      col.lab ="red", 
      lwd=2)
#
# Plot the cumulative probabilities
plot((0:indx),cum_result,
      xlim=c(0, 15),                                            #-- Specify Limits for the X-axis
      type = "l", 
      main = "Uniformly Distributed Wait Time between 0 and 15 
     - Cumulative Probability", 
      xlab = "Wait Time at most", 
      ylab = "Cumulative Probability", 
      col  = "blue", 
      col.lab ="red", lwd=2)
text((0:indx), cum_result[1:len_result], round(cum_result[1:len_result], 4), cex=0.6, pos=4, col="red")
#
# Probability that the person waited for (at most) 10 minutes 
#Cumulative probability for i = 10
print(paste("Probability that the person waited for (at most) 10 minutes = ", 
            round(punif(10, 0, 15, log = FALSE),4)))
#
# Probability that the person waited between 5 and 7 minutes 
print(paste("Probability that the person waited between 5 and 7 minutes = ", 
            round(punif(7, 0, 15, log = FALSE) - punif(5, 0, 15, log = FALSE),4)))
#
# 10th, 50th, and 90th percentiles
print(paste("The 10th percentile is ",qunif(0.10, 0, 15, lower.tail = TRUE, log.p = FALSE), " minutes"))
print(paste("The 50th percentile is ",qunif(0.50, 0, 15, lower.tail = TRUE, log.p = FALSE), " minutes"))
print(paste("The 90th percentile is ",qunif(0.90, 0, 15, lower.tail = TRUE, log.p = FALSE), " minutes"))
#