# Book Example 2.15 Page 91

# Load the data into a vector using the c() function
hours_sleep <- c(4,4,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,
                   8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,10,10,10)
print(paste("The expected value (sample mean) is ",round(mean(hours_sleep),4),sep=""))
print(paste("The sample standard deviation is ",round(sd(hours_sleep),4),sep=""))
print(paste("The sample median is ",round(median(hours_sleep),4),sep=""))
#
# Create a frequency table of the data using the table() function
#
tbl_stu_freq <- table(hours_sleep)
cum_freq <- cumsum(stu_freq)
cum_rel_freq <- cum_freq/length(hours_sleep) # length() gives number of elements in vector
#
# Consolidate the table, cum_freq and cumsum into a data frame
#
df_students <- data.frame(tbl_stu_freq, cum_freq, cum_rel_freq)
# print the data frame
df_students
# Obtain the quantiles and the IQR
quantile(stat_students, probs = seq(0,1, 0.05), na.rm = FALSE, names = TRUE, type = 7)
IQR(hours_sleep)
#Plot the cumulative frequency using a line chart; type ="l"
plot(cum_rel_freq, type ="l")
# 
# Mean and Standard Deviation from Frequency table
#
# When we load table tbl_stu_freq into the data frame df_students
# the first column containing number of students in the data frame
# becomes a class variable (categorical)
# We want to convert them into numbers
# We do that using as.numeric(as.character(df_students$stat_students))
# num_students is now a numeric vector
#
num_hours <- as.numeric(as.character(df_students$hours_sleep))
num_hours
#
# Freq is obtained as a vector first 
stu_freq <- df_students[,"Freq"] # ignore first column, get second column titled "Freq"
stu_freq
#
# We now calculate the expected value (sample mean) from the frequency table
#
expected_val = sum(num_hours*stu_freq)/sum(stu_freq)
print(paste("The expected value (sample mean) is ",round(expected_val,4),sep=""))
#
# We calculate the deviation of num_students from the mean as well as its square 
#
mean_dev <- num_students - expected_val
mean_dev_sq <- mean_dev * mean_dev
#
# We then calculate the sample variance followed by the sample standard deviation
# To calculate the product stu_freq*mean_dev_sq, we first transpose stu_freq using the t() function.
# Then we use the matrix product operator %*% to do the matrix multiplication
#
varian <- (t(stu_freq)%*%mean_dev_sq)/(sum(stu_freq) - 1)
stand_dev <- sqrt(varian)
print(paste("The sample standard deviation is ",round(stand_dev,4),sep=""))


xi <- c(53.25, 59.5, 69.5, 71.5, 77.5, 83.5, 89.5, 95.5)
fi <- c(1,0,4,4,2,3,4,1)
#
# # t(xi) gives the transpose for xi; 
# we use matrix multiplication %*% with fi, to give the sum of fi*xi
exp_val <- (t(xi) %*% fi)/sum(fi) 
print(paste("The frequency table expected value (sample mean) is: ",round(exp_val, 4) ,sep=""))
#
x_deviation <- xi - exp_val
x_deviation_sq <- (x_deviation)*(x_deviation)
samp_sd = sqrt(t(fi)%*%x_deviation_sq/7)
print(paste("The frequency table sample standard deviation is: ",round(samp_sd, 4) ,sep=""))


tempr <- c(61, 61, 62, 64, 66, 67, 67, 67, 68, 69, 
           70, 70, 70, 71, 71, 72, 74, 74, 74, 75, 
           75, 75, 76, 76, 77, 78, 78, 79, 79, 95)
tbl1 <- table(tempr)
tabl1.df <- data.frame(tbl1)
tabl1.df
tabl1.df$tempr_val <- as.numeric(as.character(tabl1.df$tempr))
tabl1.df$frq <- tabl1.df[,"Freq"]
tabl1.df
expected_val = sum(tabl1.df$tempr_val*tabl1.df$frq)/round(sum(tabl1.df$frq),4)
print(paste("The expected value (sample mean) is ",round(expected_val,4),sep=""))
tabl1.df$mean_dev<-tabl1.df$tempr_val - expected_val
tabl1.df
tabl1.df$mean_dev_sq <- tabl1.df$mean_dev * tabl1.df$mean_dev
tabl1.df
varian <- (t(tabl1.df$frq)%*%tabl1.df$mean_dev_sq)/(sum(tabl1.df$Freq) - 1)
stand_dev <- sqrt(varian)
print(paste("The sample standard deviation is ",round(stand_dev,4),sep=""))
mean(tempr)
sd(tempr)

