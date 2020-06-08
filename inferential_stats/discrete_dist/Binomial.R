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
# The number of trials is the number of records in the file
nsize <- length(income)
#
# Getting binomial probabilities; x is number of success 0,1,...,number of records.
# p = prob and x is number of successes
#log = FALSE says that probabilities are not logarithm of probabilities
#
prob = 0.52
x = 5
# dbinom gives pmf value - probability of x successes
#
print(paste("Probability of ",x," successes in ",nsize, " trials is:", 
            round(dbinom(x, nsize, prob, log = FALSE),4) ))
# pbinom gives cdf value - probability of <= x successes
#
print(paste("Probability of <= ",x," successes in ",nsize, " trials is:", 
            round(pbinom(x, nsize, prob, log = FALSE),4) ))
# qbinom gives the quantile value of x for the specified quantile.
#
print(paste("The value of x for the 35th quantile i.e., P(X <=x) = 0.35 is ", 
            qbinom(prob, nsize, 0.35, lower.tail = TRUE, log = FALSE)))
#
# Table of probabilities
#
# Setting vectors to hold intermediate results
#
# Because x=0,1,2,,,number of records, we need the vectors to have size 1 greater
# than number of records
#
result <- vector("numeric", nsize+1)
cum_result <- vector("numeric", nsize+1)
xPx <- vector("numeric", nsize+1)
x2Px <- vector("numeric", nsize+1)
rv_binom <-vector("numeric", 5)
#
# Each vector's index goes from 1 to 11. For example result[1] will hold probability x = 0
# and result[11] will hold probability x = 10
#
for (i in 0:nsize) {
  result[i+1] <- dbinom(i, nsize, prob, log = FALSE)
  cum_result[i+1] <- pbinom(i, nsize, prob, log = FALSE)
  print(paste("X = ",i, "probability = ",round(result[i+1],4), 
              "cumumlative probability = ", round(cum_result[i+1],4), sep = " "))
}

# Checking the Empirical mean vs Theoretical mean = np and 
# Checking the Empirical variance vs Theoretical variance = np(1-p) 
# Checking the Empirical standard deviation vs Theoretical standard deviation = sqrt(np(1-p))
for (i in 0:nsize) {
  xPx[i+1] <- i*result[i+1]
  x2Px[i+1] <- i*xPx[i+1]
}

# Mean = sum of xPx
Exp_val = sum(xPx)
print(paste("The Expected Value (empirical mean) is",round(Exp_val, 4), sep = " "))

# Var = sum of X2Px - (sum(xPx)^2)
varian = sum(x2Px) - Exp_val*Exp_val
print(paste("The Empirical Variance is",round(varian, 4), sep = " "))
print(paste("The Empirical standard deviation is",round(sqrt(varian), 4), sep = " "))

# Plot the pmf 
len_result <- length(result)
indx = len_result - 1
#
plot((0:indx),result[1:len_result],                #--x-axis, y-axis
     type = "h",                                   #--type is histogram
     main = "Binomial Probability Plot 10 trials", #--Main title
     sub = "Probability of Success p = 0.52",      #--sub-title
     xlab = "# Success",                           #--X-Label
     ylab = "Probability Mass",                    #--Y-label
     col  = "blue",                                #--color of pillars
     col.lab ="red",                               #-- color of X and Y labels
     lwd=10)                                       #--width of pillars
#
text((0:indx), result[1:len_result],               #-- adding text legend to pillars;x-axis;y-axis
     round(result[1:len_result], 4),               #-- value shown in pillars
     cex=0.6,                                      #-- size expansion for text font
     pos=4,                                        #-- 4 indicates show to right of pillar (choose 1,2,3,4)
     col="red")                                    #-- color of text

# Plot the cdf
len_result <- length(result)
indx = len_result - 1
plot((0:indx),cum_result[1:len_result], 
     type = "h", 
     main = "Binomial Cumulative Probability Plot 10 trials", 
     sub = "Cumulative Probability of Success p = 0.52", 
     xlab = "# Success", ylab = "Probability Mass", 
     col  = "blue", 
     col.lab ="red", 
     lwd=10)
text((0:indx), cum_result[1:len_result], round(cum_result[1:len_result], 4), cex=0.6, pos=4, col="red")
#
