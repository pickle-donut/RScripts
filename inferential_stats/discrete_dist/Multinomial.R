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


# Multinomial Probability of 2 PhDs, 2 Masters, 2 Bachelors and 4 HS
x_vec <- c(2,2,2,4)
n_size = 10
prob_x <- c(0.1, 0.2, 0.3, 0.4)
m_result <- dmultinom(x_vec, n_size, prob_x, log = FALSE)
print(paste("The Multinomial Probability of 2 PhDs, 2 Masters, 2 Bachelors and 4 HS is",m_result,sep=" "))
#
# If we select 5 customers, what is the probability that there will be 2 Masters and 3 HS?
x_vec <- c(0, 2, 0, 3)
n_size = 5
prob_x <- c(0.1, 0.2, 0.3, 0.4)
m_result <- dmultinom(x_vec, n_size, prob_x, log = FALSE)
print(paste("The Multinomial Probability of 0 PhDs, 2 Masters, 0 Bachelors and 3 HS is",m_result,sep=" "))
#
# Suppose we have a bowl with 10 marbles - 2 red marbles, 3 green marbles, and 5 blue marbles. 
# We randomly select 4 marbles from the bowl, with replacement. 
# What is the probability of selecting 2 green marbles and 2 blue marbles?
#
x_vec <- c(0, 2, 2)
n_size = 4
prob_x <- c(0.2, 0.3, 0.5)
m_result <- round(dmultinom(x_vec, n_size, prob_x, log = FALSE), 4)
print(paste("The Multinomial probability of selecting 2 green marbles and 2 blue marbles is",m_result,sep=" "))
