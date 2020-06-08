# install.packages("moments")
# Samples and Sampling Distributions
#
library("moments")
#
# First sample
X1 <- c(rnorm(50, 10, 5))
#
print(paste("The sample mean is: ", round(mean(X1),4)))
print(paste("The sample standard deviation is: ", round(sd(X1),4)))
print(paste("The sample median is: ", round(median(X1),4)))
print(paste("The sample skewness is: ", round(skewness(X1),4)))
print(paste("The sample kurtosis is: ", round(kurtosis(X1),4)))
h <- hist(X1, main="Histogram of Weight Loss in a sample of size 50", 
           xlab="Weight Loss in pounds", 
           border="blue", 
           col="green",
           xlim=c(-5,30),
           las=1, 
           breaks=14)
#
# Second Sample
X2 <- c(rnorm(50, 10, 5))
#
print(paste("The sample mean is: ", round(mean(X2),4)))
print(paste("The sample standard deviation is: ", round(sd(X2),4)))
print(paste("The sample median is: ", round(median(X2),4)))
print(paste("The sample skewness is: ", round(skewness(X2),4)))
print(paste("The sample kurtosis is: ", round(kurtosis(X2),4)))
hist(X2, main="Histogram of Weight Loss in a sample of size 50", 
     xlab="Weight Loss in pounds", 
     border="blue", 
     col="green",
     xlim=c(-5,30),
     las=1, 
     breaks=14)

# W = X1 + X2
W = X1 + X2
print(paste("The sample mean of X1 + X2 is: ", round(mean(W),4)))
print(paste("The sample standard deviation of X1 + X2 is: ", round(sd(W),4)))
hist(W, main="Histogram of X1 + X2 in a sample of size 50", 
     xlab="X1 + X2", 
     border="blue", 
     col="green",
     xlim=c(-5,30),
     las=1, 
     breaks=14)
#
# X-bar = (X1 + X2)/2 = W/2
x_bar = (X1 + X2)/2
print(paste("The sample mean of x_bar is: ", round(mean(x_bar),4)))
print(paste("The sample standard deviation of x_bar is: ", round(sd(x_bar),4)))
hist(W, main="Histogram of x-bar in a sample of size 50", 
     xlab="x_bar", 
     border="blue", 
     col="green",
     xlim=c(-5,30),
     las=1, 
     breaks=14)
#
# Generating an empirical sampling distribution of sample mean - X-bar
# Define the x_bar vector
num_samp = 1000
samp_size = 50
x_bar <- vector("numeric", num_samp)
#
# Each x-bar is the mean of a random sample of size 50 drawn from a Normal(10, 5)
#
# We are generating 1000 X-bars (from 1000 samples) and storing them in the x-bar vector
#
for (i in 1:num_samp) {
  x_bar[i] = mean(rnorm(samp_size, 10, 5))
}
#
# Calculate the mean and standard deviation (called standard error) of x-bar 
# from the empirical sampling distribution formed by 1000 samples of size 50
#
Exp_x_bar <- mean(x_bar)
stderr <- sd(x_bar)
print(paste("The Expected value of X-bar is: ", round(Exp_x_bar,4)))
print(paste("The standard error or standard deviation of X-bar is: ", round(stderr,4),
            " versus predicted std error ",round(5/sqrt(samp_size),4)))
hist(x_bar, 
     main=paste("Histogram of Sampling Distribution of X-bar from ",num_samp,
                " samples of size ", samp_size, ""),
     xlab="X-bar", 
     border="blue", 
     col="green",
     xlim=c(5, 15),
     las=1, 
     breaks=20)
#
exp_samp <- rexp(100, 1)
mean(exp_samp)
hist(exp_samp,
     main=paste("Histogram of a sample from a right-skewed Exponential Population" ))
#
unif_samp <- runif(100, 0, 2)
mean(unif_samp)
hist(unif_samp,
     main=paste("Histogram of a sample from a symmetric Uniform Population" ))
 #
beta_samp <- rbeta(100, 50, 1, ncp = 0)
mean(beta_samp)
hist(beta_samp,
     main=paste("Histogram of a sample from a left-skewed Beta Population" ))
#
# Generating an empirical sampling distribution of sample mean - X-bar
# Define the x_bar vector
num_samp = 1000
samp_size = 100
x_bar <- vector("numeric", num_samp)
#
# Each x-bar is the mean of a random sample of size 100 drawn from a 3 different distrbutions
# exponential (right-skewed), uniform (symmetric), beta (left-skewed)
# We are generating 1000 X-bars (from 1000 samples) and storing them in the x-bar vector
#
for (i in 1:num_samp) {
# Uncomment the distribution to be used in the next three lines; leave the other two commented
#   x_bar[i] = mean(rexp(samp_size, 1))
#   x_bar[i] = mean(runif(samp_size, 0, 2))
   x_bar[i] = mean(rbeta(samp_size, 50, 1, ncp = 0))
}
#
# Calculate the mean and standard deviation (called standard error) of x-bar 
# from the empirical sampling distribution formed by 1000 samples of size 50
#
Expec_x_bar <- mean(x_bar)
stderr <- sd(x_bar)
print(paste("The Expected value of X-bar is: ", round(Expec_x_bar,4)))
print(paste("The standard error or standard deviation of X-bar is: ", round(stderr,4)))
hist(x_bar, 
     main=paste("Histogram of Sampling Distribution of X-bar from ",num_samp,
                " samples of size ", samp_size, ""),
     xlab="X-bar", 
     border="blue", 
     col="green",
#     xlim=c(5, 15),
     las=1, 
     breaks=20)
# We also collect other quantities such as skewness and kurtosis of the sampling distribution
#
skewness(x_bar)
kurtosis(x_bar)
#
#
qqnorm(x_bar)
qqline(x_bar, col = 2)
d <- density(x_bar)
plot(d)
x <- seq(7, 13, by=0.1)
y <- dnorm(x,Exp_x_bar,stderr)
lines(x, y, col = "red")
y1 <- dnorm(x,10.00,5/sqrt(samp_size))
lines(x, y1, col = "blue")