# Clear the Environment
rm(list=ls())
#
# 10.96 - Page 597 
# Independent two-sample test - Known population standard deviations
#
n1 = 21
n2 = 31
x1_bar = 31
x2_bar = 22
sigma1 = 6
sigma2 = 3
se1 <- (sigma1^2/n1)
se2 <- (sigma2^2/n2)
std_err <- sqrt(se1+ se2)
print(std_err)
z_statistic <- (x1_bar - x2_bar)/std_err
print(z_statistic)
# Lower-tailed test
#p_value <- (pnorm(z_statistic,deg_free))
#Upper-tailed test
p_value <- (1 - pnorm(z_statistic,0,1))
# Two-tailed test
#p_value <- 2*(1-*pnorm(abs(z_statistic,0,1)))
print(p_value)
#
# Book Problem 10.2 - Page 565
# Independent two-sample test - unknown population standard deviations
#
n1 = 11
n2 = 9
x1_bar = 4
x2_bar = 3.5
s1 = 1.5
s2 = 1
se1 <- (s1^2/n1)
se2 <- (s2^2/n2)
std_err <- sqrt(se1+ se2)
print(std_err)
t_statistic <- (x1_bar - x2_bar)/std_err
print(t_statistic)
deg_free_n <- (se1 + se2)^2
deg_free_d <- (se1^2/(n1 -1)) + (se2^2/(n2 -1))
deg_free <- deg_free_n/deg_free_d
print(deg_free)
# Lower-tailed test
#p_value <- (pt(t_statistic,deg_free))
# Upper-tailed test
p_value <- (1 - pt(t_statistic,deg_free))
# Two-tailed test
#p_value <- 2*(1-pt(abs(t_statistic,deg_free)))
print(p_value)
#
# Book Problem 10.98 - Page 597
# Independent two-sample test - unknown population standard deviations
wife <- c(2,2,3,3,4,2,1,1,2,4)
husband <- c(2,2,1,3,2,1,1,1,2,4)
t.test(wife,husband,paired=FALSE, alternative = "less")
#
#
n1 = length(wife)
n2 = length(husband)
x1_bar = mean(wife)
x2_bar = mean(husband)
s1 = sd(wife)
s2 = sd(husband)
se1 <- (s1^2/n1)
se2 <- (s2^2/n2)
std_err <- sqrt(se1+ se2)
print(std_err)
t_statistic <- (x1_bar - x2_bar)/std_err
print(t_statistic)
deg_free_n <- (se1 + se2)^2
deg_free_d <- (se1^2/(n1 -1)) + (se2^2/(n2 -1))
deg_free <- deg_free_n/deg_free_d
print(deg_free)
# Lower-tailed test
p_value <- (pt(t_statistic,deg_free))
print(p_value)
#
#
# Book Problem 10.107 - Page 599 
# Independent two-sample test - Population Proportions
#
xA = 67169
nA = 248775
xB = 42769
nB = 155525
pprime_A = xA/nA
pprime_B = xB/nB
pc = (xA + xB)/(nA + nB)
std_err <- sqrt(pc*(1-pc)*((1/nA)+(1/nB)))
print(std_err)
z_statistic <- (pprime_A - pprime_B)/std_err
print(z_statistic)
# Lower-tailed test
p_value <- (pnorm(z_statistic,0,1))
#Upper-tailed test
#p_value <- (1 - pnorm(z_statistic,0,1))
# Two-tailed test
#p_value <- 2*(1-*pnorm(abs(z_statistic,0,1)))
print(p_value)
#
# Book Problem 10.115 - Page 601
# Dependent samples - Matched or Paired samples - Paired t-test
#
start_chol <- c(140, 220, 110, 240, 200, 180, 190, 360, 280, 260)
end_chol <- c(140, 230, 120, 220, 190, 150, 200, 300, 300, 240)
xd <- start_chol - end_chol
x_bar_d <-mean(xd)
std_err <- sd(xd)/sqrt(length(xd))
print(x_bar_d)
print(std_err)
t_statistic <- x_bar_d/std_err
print(t_statistic)
deg_free <- length(xd) - 1
print(deg_free)
# Lower-tailed test
# p_value <- (pt(t_statistic,deg_free))
# Upper-tailed test
p_value <- (1 - pt(t_statistic,deg_free))
# Two-tailed test
#p_value <- 2*(1-pt(abs(t_statistic,deg_free)))
print(p_value)
#
t.test(start_chol,end_chol,paired=TRUE, alternative = "greater")

