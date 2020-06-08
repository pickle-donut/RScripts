nsize <- 22
prob = 0.85
x = 15

# a
print(paste("Probability of 16 or more successes in ",nsize, " trials is:", 
            1 - round(pbinom(x, nsize, prob, log = FALSE),4) ))

#b
print(paste("The value of x for the 30th quantile i.e., P(X >= x) = 0.3 is ", 
            qbinom(prob, nsize, 0.70, lower.tail = TRUE, log = FALSE)))

#c
print(paste("The value of x for the 10th quantile i.e., P(X <=x) = 0.10 is ", 
            qbinom(prob, nsize, 0.1, lower.tail = TRUE, log = FALSE)))


# d
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
  
}

df <- data.frame(x = seq(0,22,1), probability = result, cumulative_probability = cum_result, row.names = NULL)
print(df, row.names = FALSE)

#e
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
print(paste("The Empirical standard deviation is",round(sqrt(varian), 4), sep = " "))

# f
# Plot the pmf 
len_result <- length(result)
indx = len_result - 1
#
plot((0:indx),result[1:len_result],                #--x-axis, y-axis
     type = "h",                                   #--type is histogram
     main = "Binomial Probability Plot 22 trials", #--Main title
     sub = "Probability of Success p = 0.85",      #--sub-title
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
     main = "Binomial Cumulative Probability Plot 22 trials", 
     sub = "Cumulative Probability of Success p = 0.85", 
     xlab = "# Success", ylab = "Probability Mass", 
     col  = "blue", 
     col.lab ="red", 
     lwd=10)

text((0:indx), cum_result[1:len_result], 
     round(cum_result[1:len_result], 4), cex=0.6, pos=4, col="red")