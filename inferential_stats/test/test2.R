#1
dnbinom(0, 1, 0.3, log = FALSE) + dnbinom(1, 1, 0.3, log = FALSE)
1 - (dnbinom(0, 1, 0.3, log = FALSE) + dnbinom(1, 1, 0.3, log = FALSE))

#2
phyper(4,13,39,13)

#3
hand <- c(1, 4, 4, 4)
size <- 13
prob <- c(0.25, 0.25, 0.25, 0.25)
dmultinom(hand, size, prob, log = FALSE)

#4
dnbinom(4,1,0.1)
dnbinom(7,3,0.1)

#5
dpois(3, 2.5, log = FALSE)
ppois(3, 2.5, log = FALSE)
ppois(5, 2.5, log = FALSE, lower.tail = FALSE)
#table of probabilities
nsize = 10
result <- vector("numeric", nsize+1)
cum_result <- vector("numeric", nsize+1)
xPx <- vector("numeric", nsize+1)
x2Px <- vector("numeric", nsize+1)
for (i in 0:nsize) {
  result[i+1] <- dpois(i, 2.5, log = FALSE)
  xPx[i+1] <- i*result[i+1]
  x2Px[i+1] <- i*xPx[i+1]
  cum_result[i+1] <- ppois(i, 2.5, log = FALSE)
}
round(result[1:nsize+1], 4)
round(cum_result[1:nsize+1], 4)

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