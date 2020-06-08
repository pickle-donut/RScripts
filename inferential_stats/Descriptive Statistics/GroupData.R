# Nominal Group Example
# Book Page 129 - Table 2.41
# Bar Plot
#
science_pct <- c(28.9, 7.6, 12.1, 18.5, 24.2, 8.7)
towns <- c("Alabaster", "Concordia", "Genoa", "Mocksville", "Tynesson", "West End")
labeled_towns <- paste(towns,science_pct,"%", sep="")
barplot(science_pct,
        main = "Science Competition Population",
        horiz=FALSE,
        xlab = "Towns", 
        ylab = "Percent of Population",
        ylim =c(0, 40),
        names.arg=towns)
# Pie Chart
pie(science_pct, labels=labeled_towns,main = "Science Competition Population")
#
# Ordinal Group Example
# Book Page 151 - Problem 2.108
# Bar Plots
#
age_group <-c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
rel_freq <- c(18.0, 8.0, 22.8, 15.0, 13.1, 11.0, 10.3)
cum_rel_freq <- cumsum(rel_freq)
barplot(rel_freq,
        main = "Age Distribution of Japanese-Americans in Santa Clara, CA",
        horiz = FALSE,
        ylim =c(0, 30),
        names.arg=age_group)
text(rel_freq, labels =rel_freq, cex=1.2, pos=4, col="red")
barplot(cum_rel_freq,
        main = "Cumulative Relative Frequency",
        sub = "Age of Japanese-Americans in Santa Clara, CA",
        ylim=c(0, 100),
        horiz = FALSE,
        names.arg=age_group)
text(cum_rel_freq, labels =cum_rel_freq, cex=1.2, pos=3, col="red")
#
# Numerical (range) Group variables
# Book Example 2.30, Page 107
# The mid-point of the numerical range is used as a number value for the variable
# 
grade_interval <- c("50-56.5", "56.5-62.5", "62.5-68.5","68.5-74.5","74.5-80.5","80.5-86.5","86.5-92.5","92.5-98.5")
xi <- c(53.25, 59.5, 69.5, 71.5, 77.5, 83.5, 89.5, 95.5)
fi <- c(1,0,4,4,2,3,4,1)
df1 <- data.frame(grade_interval, xi, fi)
df1$pi <- df1$fi/sum(df1$fi)
df1$xifi <- df1$xi*df1$fi
expected_val <- sum(df1$xifi)/sum(df1$fi)
df1$deviation <- df1$xi - expected_val
df1$sq_dev = df1$deviation*df1$deviation
df1
varian <- sum(df1$fi*df1$sq_dev)/(sum(df1$fi)-1)
std_dev <- sqrt(varian)
print(expected_val)
print(std_dev)


#
#
# Load the data into a vector using the c() function
hours_sleep <- c(4,4,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,
                   8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,10,10,10)
# Create a frequency table of the data using the table() function
#
tbl_stu_freq <- table(hours_sleep)
# Put the table in a data Frame
df_students <- data.frame(tbl_stu_freq)
df_students
# 
# When we load table tbl_stu_freq into the data frame df_students
# the first column containing number of students in the data frame
# becomes a class variable (categorical)
# We want to convert them into numbers
# We do that using as.numeric(as.character(df_students$hours_sleep))
# num_hours is now a numeric vector representing hours_sleep
#We simply add num_hours as a new column in the data frame df_students
#
df_students$num_hours <- as.numeric(as.character(df_students$hours_sleep))
#
# Add cum_freq and cum_rel_freq as new columns in the data frame df_students
df_students$rel_freq <- df_students$Freq/sum(df_students$Freq) 
df_students$cum_rel_freq <- cumsum(df_students$rel_freq) 
df_students
#
#Multiply num_hours * Freq to get xifi and add as a column to df_students
#
df_students$xifi <- df_students$num_hours * df_students$Freq
df_students
#
# We now calculate the expected value (sample mean) from the frequency table
#
expected_val = sum(df_students$xifi)/sum(df_students$Freq)
print(paste("The expected value of student sleep hours (sample mean) is ",round(expected_val,4),sep=""))
#
# We calculate the deviation of num_students from the mean as well as its square 
#
df_students$mean_dev <- df_students$num_hours - expected_val
df_students$mean_dev_sq <- df_students$mean_dev * df_students$mean_dev
df_students
#
varian <- sum(df_students$Freq*df_students$mean_dev_sq)/(sum(df_students$Freq) - 1)
stand_dev <- sqrt(varian)
print(paste("The sample standard deviation is ",round(stand_dev,4),sep=""))
#
#From raw data
#
print(paste("The expected value (sample mean) is ",round(mean(hours_sleep),4),sep=""))
print(paste("The sample standard deviation is ",round(sd(hours_sleep),4),sep=""))
print(paste("The sample median is ",round(median(hours_sleep),4),sep=""))
#
#
hist(hours_sleep, main="Histogram for Student Sleep Hours", 
     xlab="Student Sleep time in Hours", 
     border="blue", 
     col="green",
     xlim=c(2,12),
     las=1, 
     breaks=7)
#
plot(df_students$num_hours, df_students$rel_freq, 
     main="Relative Frequency Graph for Student Sleep Hours",
     xlab="Student Sleep time in Hours", 
     xlim=c(2,12),
     ylab = "Relative Frequency",
     type ="l")
plot(df_students$num_hours, df_students$cum_rel_freq, 
     main="Cumulative Relative Frequency Graph for Student Sleep Hours",
     xlab="Student Sleep time in Hours", 
     xlim=c(2,12),
     ylab = "Cumulative Relative Frequency",
     type ="l")
# Obtain the quantiles and the IQR
quantile(hours_sleep, probs = seq(0,1, 0.05), na.rm = FALSE, names = TRUE, type = 2)
IQR(hours_sleep, type = 2)


