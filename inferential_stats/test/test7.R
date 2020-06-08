# 1
# Independent two-sample test - Known population standard deviations
n1 = 36
n2 = 23
x1_bar = 679
x2_bar = 559
sigma1 = 180
sigma2 = 180
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

# 2
# Independent two-sample test - unknown population standard deviations
W <- c(9,3,4,3,4,4)
E <- c(9,8,7,6,5,3)
t.test(E, W, paired=FALSE, alternative = "less")

n1 = length(W)
n2 = length(E)
x1_bar = mean(E)
x2_bar = mean(W)
s1 = sd(W)
s2 = sd(E)
se1 <- (s1^2/n1)
se2 <- (s2^2/n2)
std_err <- sqrt(se1 + se2)
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

# 3
# Independent two-sample test - Population Proportions
xA = 20
nA = 107
xB = 47
nB = 92
pprime_A = xA/nA
pprime_B = xB/nB
pc = (xA + xB)/(nA + nB)
std_err <- sqrt(pc*(1-pc)*((1/nA)+(1/nB)))
print(std_err)
z_statistic <- (pprime_A - pprime_B)/std_err
print(z_statistic)
# Lower-tailed test
#p_value <- (pnorm(z_statistic,0,1))
#Upper-tailed test
#p_value <- (1 - pnorm(z_statistic,0,1))
# Two-tailed test
p_value <- 2*(1-pnorm(abs(z_statistic),0,1))
print(p_value)

# 4
# Dependent samples - Matched or Paired samples - Paired t-test
hy <- c(107,358,209,209,167,179,179,625,179,245)
hilt <- c(169,289,299,198,169,214,169,459,159,239)
xd <- hy - hilt
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
#p_value <- (1 - pt(t_statistic,deg_free))
# Two-tailed test
p_value <- 2*(1-pt(abs(t_statistic),deg_free))
print(p_value)
#
t.test(hy,hilt,paired=TRUE, alternative = "two.sided")

