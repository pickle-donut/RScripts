install.packages('moments')

library(moments)

# 1
conf <- c(4, 36, 18, 19, 4, 1, 1, 1)
days <- c(2,  3,  4,  5, 6, 7, 8, 9)

series <- list()
for(i in 1:length(conf)){
  series[[i]] <- rep(days[i], conf[i])
}
series <- unlist(series)

hist(series, main = "Histogram of Days Spent at Conferences"
     , xlab = "Days"
     , border = "black"
     , col = "green"
     , xlim = c(0, 10)
     , ylim = c(0, 40)
     , breaks = seq(0, 10, 1)
)

quantile(series, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE)


quantile(series, probs = seq(0, 1, 0.05), na.rm = FALSE, names = TRUE)

IQR(series, type = 2)

boxplot(series, main = "Boxplot of Days Spent at Conference"
        , xlab = "Days"
        , col = "lightgrey"
        , horizontal = TRUE
        )
text(x = boxplot.stats(series)$out, labels = boxplot.stats(series)$out, y = 1.1)

quantile(series, probs = 0.25, na.rm = FALSE, names = TRUE, type = 2)
quantile(series, probs = 0.75, na.rm = FALSE, names = TRUE, type = 2)

mean(series)

sd(series)

table(series)

skewness(series)
kurtosis(series)

# 2
lo <- c(11.4, 20.45, 29.45, 38.45, 47.45, 56.45, 65.45, 74.45)
hi <- sort(union(lo[-1], c(20.45, 83.45)))
mid <- as.character((lo + ((hi - lo) / 2)))
paste(mid, collapse = ", ")


obese_interval <- c("11.4-20.45", "20.45-29.45", "29.45-38.45","38.45-47.45","47.45-56.45","56.45-65.45","65.45-74.45","74.45-83.45")
xi <- c(15.925, 24.95, 33.95, 42.95, 51.95, 60.95, 69.95, 78.95)
fi <- c(29, 13, 4, 0, 2, 1, 0, 1)
df1 <- data.frame(obese_interval, xi, fi)
df1$pi <- df1$fi/sum(df1$fi)
df1$xifi <- df1$xi*df1$fi

barplot(df1$fi,
        main = "Obesity Distribution - Frequency"
        , horiz = FALSE
        , xlab = "Obesity Interval"
        , ylab = "Frequency"
        , ylim = c(0, 30)
        , names.arg=obese_interval
)
barplot(df1$pi,
        main = "Obesity Distribution - Relative Frequency"
        , horiz = FALSE
        , xlab = "Obesity Interval"
        , ylab = "Relative Frequency"
        , ylim = c(0, 0.6)
        , names.arg=obese_interval
)

df1
sum(df1[4:8,pi])/ sum(df1[,pi])


expected_val <- sum(df1$xifi)/sum(df1$fi)
print(expected_val)

df1$deviation <- df1$xi - expected_val
df1$sq_dev = df1$deviation*df1$deviation
varian <- sum(df1$fi*df1$sq_dev)/(sum(df1$fi)-1)
std_dev <- sqrt(varian)
print(std_dev)

# 3
# Generating an empirical sampling distribution of sample mean - X-bar
# Define the x_bar vector
num_samp = 5000
samp_size = 30
x_bar <- vector("numeric", num_samp)

mu = 5
sigma = 2

for (i in 1:num_samp) {
  x_bar[i] = mean(rnorm(samp_size, mu, sigma))
}
#
# Calculate the mean and standard deviation (called standard error) of x-bar 
# from the empirical sampling distribution formed by 5000 samples of size 300
#
Exp_x_bar <- mean(x_bar)
stderr <- sd(x_bar)
print(paste("The Expected value of X-bar is: ", round(Exp_x_bar,4)))
print(paste("The standard error or standard deviation of X-bar is: ", round(stderr,4),
            " versus predicted std error ",round(sigma/sqrt(samp_size),4)))
hist(x_bar, 
     main=paste("Histogram of Sampling Distribution of X-bar from ",num_samp,
                " samples of size ", samp_size, ""),
     xlab="X-bar", 
     border="blue", 
     col="green",
     xlim=c(3,7),
     las=1, 
     breaks=20)


#####################################################################################################


# Generating an empirical sampling distribution of sample mean - X-bar
# Define the x_bar vector
num_samp = 5000
samp_size = 300
x_bar <- vector("numeric", num_samp)

mu = 5
sigma = 2

for (i in 1:num_samp) {
  x_bar[i] = mean(rnorm(samp_size, mu, sigma))
}
#
# Calculate the mean and standard deviation (called standard error) of x-bar 
# from the empirical sampling distribution formed by 5000 samples of size 300
#
Exp_x_bar <- mean(x_bar)
stderr <- sd(x_bar)
print(paste("The Expected value of X-bar is: ", round(Exp_x_bar,4)))
print(paste("The standard error or standard deviation of X-bar is: ", round(stderr,4),
            " versus predicted std error ",round(sigma/sqrt(samp_size),4)))
hist(x_bar, 
     main=paste("Histogram of Sampling Distribution of X-bar from ",num_samp,
                " samples of size ", samp_size, ""),
     xlab="X-bar", 
     border="blue", 
     col="green",
     xlim=c(4,6),
     las=1, 
     breaks=20)
