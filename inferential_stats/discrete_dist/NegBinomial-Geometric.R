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
#Negative binomial - number of males we will see before the rth Female, 
# assuming that p=Probabilty of seeing a female = 0.48
#
x <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
r = 2
p_female=0.48
nsize = 16
result <- vector("numeric", 16)
cum_result <- vector("numeric", 16)
for (i in 0:15) {
  result[i+1] <- dnbinom(i, r, p_female, log = FALSE)
  cum_result[i+1] <- pnbinom(i, r, p_female, log = FALSE)
}
print("Number of failures before 2 successes")
x
print("Probabilities")
round(result[1:nsize], 4)
print("Cumulative Probabilities")
round(cum_result[1:nsize], 4)
#
#
# Plot the probabilities
len_result <- length(result)
indx = len_result - 1
#
#Plot the probability mass function
plot((0:indx),result[1:len_result], type = "h", main = "Negative Binomial Probability Plot 
     - Probability of number of males before 2 females", 
     sub = "Probability of Success p = 0.48", xlab = "# Success", ylab = "Probability Mass", 
     col  = "blue", col.lab ="red", lwd=10)
text((0:indx), result[1:len_result], round(result[1:len_result], 4), cex=0.6, pos=4, col="red")

# Plot the cumulative probabilities
len_result <- length(result)
indx = len_result - 1
plot((0:indx),cum_result[1:len_result], type = "h", main = "Negative Binomial Cumulative Probability Plot 
     - Probability of at least these number of males before 2 females", 
     sub = "Cumulative Probability of Success p = 0.48", xlab = "# Success", ylab = "Probability Mass", 
     col  = "blue", col.lab ="red", lwd=10)
text((0:indx), cum_result[1:len_result], round(cum_result[1:len_result], 4), cex=0.6, pos=4, col="red")
#
# Geometric Distribution
# What is the probability that the next 5 customers are Male (the sixth customer is a Female)?
#
r = 1
print(paste("The probability that the next 5 customers are Male (the sixth customer is a Female) is ", 
            round(dnbinom(2, r, 0.7, log = FALSE), 4), sep = " "))
#
# Red Miata Problem - Book Problem 104 - Page 292
#
# Find the probability that she must call at most four dealerships.
#
print(paste("probability that she must call at most four dealerships ", 
            round(pnbinom(4, 1, 0.28, log = FALSE), 4)))
#
# Find the probability that she must call three or four dealerships.
#
print(paste("probability that she must call 3 or 4 four dealerships ",
            (round(dnbinom(3, 1, 0.28, log = FALSE) + round(dnbinom(4, 1, 0.28, log = FALSE)), 4))))
