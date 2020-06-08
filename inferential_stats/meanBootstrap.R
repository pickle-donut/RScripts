#
# What bootstrap samples look like and their means
#
for (i in 1:5)  {
   b_samp <- sample(stat_score, 10, replace=TRUE)
  print(b_samp)
  print(paste("mean of this bootstrapped sample is: ",mean(b_samp)))
}
#
library(boot)
stat_score <- c(65,65,70,67,66,63,63,68,72,71)
#
# We create a data frame of stat_score
#
df <- data.frame(stat_score)
print(df[5,])
#
# We have to create our own R function - mean_fun - boot() will use to re-sample n=10 observations,    
# calculate the mean of the re_sampled observations, and retuen it back to bootstrap.
# boot() will do it R=k times, so you will have k boot-strapped means. In our case, k=5000 replicates.
#
mean_fun <- function(df, i)
  {
  d <- df[i, ]
  return(mean(d))
}
#
# The actual bootstrapping is now performed by the boot() function. The drop=FALSE retains each sample as a data frame.
#
bo <- boot(df[, "stat_score", drop = FALSE], statistic=mean_fun, R=5000)
#
# bo($t) will contain R=5000 bootstrapped sample means
#
# We can do a histogram of bo$t to see the boot-strapped sampling distribution of sample means
#
hist(bo$t,
     main = "Bootrapping sample Means, R=5000 samples",
     xlab="Bootstrapped Sample Means for R=5000 samples",
     col="blue")
#
# We can print out the mean of all the boots-strapped sample means. This is the Expected value of X-bar
# Because this is bootstrapping, the expected value will not be exactly the population mean
# That is, it is biased
print(paste("Mean of Bootstrapped Sample Means",round(mean(bo$t),4)))
#
# The standard error is the standard deviation of the boot-strapped sampling distribution
#
print(paste("Standard Deviation of Bootstrapped Sample Means - Standard Error",round(sd(bo$t),4)))
#
# We can obtain a bot-strapped 95% confidence interval using boot.ci; 
# bca means intervals are calculated using the adjusted bootstrap percentile (BCa) method.
#
boot.ci(bo, conf=0.95, type="bca")



