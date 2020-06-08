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
#Normal Distribution
#
# dnorm(x, mean = 0, sd = 1, log = FALSE)
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#
#Terri Vogel, an amateur motorcycle racer, averages 129.71 seconds per 2.5 mile lap (in a seven-lap race) 
#with a standard deviation of 2.28 seconds. The distribution of her race times is normally distributed. 
#We are interested in one of her randomly selected laps.
#
#Find the percent of her laps that are completed in less than 130 seconds.
#
print(paste("Percent of her laps that are completed in less than 130 seconds = "
            ,round(pnorm(130, 129.71, 2.28, lower.tail = TRUE, log.p = FALSE), 4)*100)) 
#
#The fastest 3% of her laps are under
print(paste("The fastest 3% of her laps are under "
            ,round(qnorm(0.03, 129.71, 2.28, lower.tail = TRUE, log.p = FALSE), 4))) 
#
#The middle 80% of her laps are from _______ seconds to _______ seconds.
#
f10 = round(qnorm(0.10, 129.71, 2.28, lower.tail = TRUE, log.p = FALSE), 4) 
f90 = round(qnorm(0.90, 129.71, 2.28, lower.tail = TRUE, log.p = FALSE), 4) 
print(paste("The middle 80% of her laps are from ",f10,"seconds to ", f90, " seconds." ))
#
#
#table of probabilities for select values of X from 120 to 140.
nsize = 400
result <- vector("numeric", nsize)
cum_result <- vector("numeric", nsize)
x <- vector("numeric", nsize)
xPx <- vector("numeric", nsize)
x2Px <- vector("numeric", nsize)
for (i in 1:nsize) {
  # We are creating 400 values of x at 0.05 intervals, so 120 to 140.
  x[i] <- 120 + ((i-1)/20)                
  result[i] <- round(dnorm(x[i], 129.71, 2.28, log = FALSE), 4)
  cum_result[i] <- round(pnorm(x[i], 129.71, 2.28, lower.tail = TRUE, log.p = FALSE), 4)
}
#
# Plot the probabilities
len_result <- length(result)
indx = (len_result - 1)
#Plot the probability mass function
plot(x,result,                                                  #-- Plot(x,y)
      xlim=c(120,140),                                          #-- set x-axis limits between 120 and 140
      type = "p",                                               #-- p is for plotting points
      pch=20,                                                   #-- pch=20 is small circles for points
      main = "Normally Distributed Lap Times in Seconds 
              - Probability Density", 
      xlab = "Lap Times in Seconds", 
      ylab = "Probability Density", 
      col  = "blue", 
      col.lab ="red"
      )
#
# Plot the cumulative probabilities
len_result <- length(result)
indx = len_result - 1
plot(x,cum_result, 
      xlim=c(120,140), 
      type = "p", 
      main = "Normally Distributed Lap Times in Seconds 
     - Cumulative Probability", 
      xlab = "Lap Times (in seconds)", 
      ylab = "Cumulative Probability", 
      col  = "blue", 
      col.lab ="red", 
      pch=20)
#
#
print(paste("There is a 30% probability that at least",  
            round(qnorm(0.7, 0.28, 0.05, lower.tail = TRUE, log.p = FALSE),4)*100, 
      "% of 18 to 34 year old Facebook users check their profile before getting out of bed"))
#
print(paste("There is a 95% probability that at most",  
            round(qnorm(0.95, 0.28, 0.05, lower.tail = TRUE, log.p = FALSE),4)*100, 
            "% of 18 to 34 year old Facebook users check their profile before getting out of bed"))
# We flip a coin 100 times (n = 100) and note that it only comes up heads 20% (p = 0.20) of the time. 
# We will find the following using Binomial and Normal and Poisson and compare:
#
print(paste("Binomial Probability of X between 12 and 28 is", 
            round(pbinom(28, 100, 0.2, log.p = FALSE ) - pbinom(12, 100, 0.2, log.p = FALSE ),4)))
print(paste("Poisson Probability of X between 12 and 28 is", 
            round(ppois(28, 20, log.p = FALSE ) - ppois(12, 20, log.p = FALSE ),4)))
print(paste("Normal Probability of X between 12 and 28 is", 
            round(pnorm(28, 20, 4, log.p = FALSE ) - pnorm(12, 20, 4, log.p = FALSE ),4)))
#
print(paste("Binomial - There is a 68% chance that the number of heads is between", 
            qbinom(0.16, 100, 0.2, log.p = FALSE )," and ",qbinom(0.84, 100, 0.2, log.p = FALSE )))
print(paste("Poisson -  There is a 68% chance that the number of heads is between", 
            qpois(0.16, 20, log.p = FALSE ), " and ",qpois(0.84, 20, log.p = FALSE )))
print(paste("Normal -  There is a 68% chance that the number of heads is between", 
            qnorm(0.16, 20, 4, log.p = FALSE ), " and ",qnorm(0.84, 20, 4, log.p = FALSE )))