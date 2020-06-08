# Clear the Environment
rm(list=ls())
#
#Case 1: X1 ~ N(5, 2); X2 ~ N(4,2) - sample Size 50
#
x1 <- rnorm(50, 5, 2)
x2 <- rnorm(50, 3, 2)
#
# Independent two-sample test - Known population standard deviations
#
n1 = 50
n2 = 50
x1_bar = mean(x1)
print(x1_bar)
x2_bar = mean(x2)
print(x2_bar)
sigma1 = 2
sigma2 = 2
se1 <- (sigma1^2/n1)
se2 <- (sigma2^2/n2)
std_err <- sqrt(se1+ se2)
print(std_err)
z_statistic <- (x1_bar - x2_bar)/std_err
print(z_statistic)
# Lower-tailed test
#p_value <- (pnorm(z_statistic,deg_free))
#Upper-tailed test
#p_value <- (1 - pnorm(z_statistic,0,1))
# Two-tailed test
p_value <- (2*(1-pnorm(abs(z_statistic),0,1)))
print(p_value)
#
#
#Case 2: X1 ~ N(5, 2); X2 ~ N(4,2) - sample Size 50
#
x1 <- rnorm(50, 5, 8)
x2 <- rnorm(50, 3, 8)
#
# Independent two-sample test - Known population standard deviations
#
n1 = 50
n2 = 50
x1_bar = mean(x1)
print(x1_bar)
x2_bar = mean(x2)
print(x2_bar)
sigma1 = 8
sigma2 = 8
se1 <- (sigma1^2/n1)
se2 <- (sigma2^2/n2)
std_err <- sqrt(se1+ se2)
print(std_err)
z_statistic <- (x1_bar - x2_bar)/std_err
print(z_statistic)
# Lower-tailed test
#p_value <- (pnorm(z_statistic,deg_free))
#Upper-tailed test
#p_value <- (1 - pnorm(z_statistic,0,1))
# Two-tailed test
p_value <- (2*(1-pnorm(abs(z_statistic),0,1)))
print(p_value)