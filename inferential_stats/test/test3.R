#1
punif(31, 1, 53) - punif(2, 1, 53)
1 - punif(40, 1, 53)
(punif(28, 1, 53) - punif(12, 1, 53)) / punif(28,1,53)
qunif(0.75, 1, 53, lower.tail = TRUE)

#2
pexp(3, 0.16111, lower.tail = TRUE, log.p = FALSE)
1 - pexp(3, 0.16111, lower.tail = TRUE, log.p = FALSE)
1 - pexp(1, 1.93332, lower.tail = TRUE, log.p = FALSE)
1 - (dpois(0, 1.93332) + dpois(1, 1.93332))

# 3
1 - pnorm(20/15) #or 1 - pnorm(120, 100, 15),  pnorm(20/15, lower.tail = FALSE)
qnorm(0.02, 100, 15, lower.tail = FALSE)
paste("Middle 50%:", qnorm(0.25, 100, 15), 'to', qnorm(0.75, 100, 15))

# 4
pnorm(-0.6) #or pnorm(220,250,50)
qnorm(0.8, 250, 50)

# 5
pbinom(95, 200, .1, log.p = FALSE) - pbinom(75, 200, .1, log.p = FALSE)
ppois(95, 20, log.p = FALSE) - ppois(75, 20, log.p = FALSE)
pnorm(95, 20, (18^.5), log.p = FALSE) - pnorm(75, 20, (18^.5), log.p = FALSE)

pbinom(25, 200, .1, log.p = FALSE) - pbinom(15, 200, .1, log.p = FALSE)
ppois(25, 20, log.p = FALSE) - ppois(15, 20, log.p = FALSE)
pnorm(25, 20, (18^.5), log.p = FALSE) - pnorm(15, 20, (18^.5), log.p = FALSE)

pbinom(50, 200, .1, log.p = FALSE, lower.tail = FALSE)
ppois(50, 20, log.p = FALSE, lower.tail = FALSE)
pnorm(50, 20, (18^.5), log.p = FALSE, lower.tail = FALSE)

dnbinom(10, 1, .1, log = FALSE)



