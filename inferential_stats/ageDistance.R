


# Read csv file as a DataFrame
#
setwd("C:\\Users\\sarathy\\Documents\\2019-Teaching\\Fall2019\\Fall2019-MSIS5503\\MSIS-5503-Data")
df1 <- read.table('Driver-Age.csv', 
                 header = TRUE, sep = ',')

#Assign variable names to DataFrame Column objects
age <- df1$Age
distance <- df1$Distance
print(df1)
#
#
samp_means <- c(mean(distance), mean(age))
print(samp_means)
samp_sd <- c(sd(distance), sd(age))
print(round(samp_sd,3))
samp_cor <- cor(distance, age)
print(round(samp_cor,3))
#
reg_mod <- lm(distance ~ age)
summary(reg_mod)
#Generate Predicted Values and Residuals and Add them to the Data Frame df1
#
df1$p_distance <- predict(reg_mod) 
df1$r_distance <- residuals.lm(reg_mod)
print (df1)
#
# Plot of Networth vs Age
#
plot(age,distance,col="red", 
     xlab="Age in Years", 
     ylab="Visibility Distance in Feet", 
     xlim = c(0, 90), ylim= c(200, 600))
text(age, distance, distance, cex=0.6, pos=3)
#
# Add Predicted Values to the Plot
# 
par(new=TRUE)
plot(age,df1$p_distance,type="l",
     yaxt='n', ann=FALSE, col="blue",  xlim = c(0, 90), ylim= c(200, 600))
#
# Obtaining Sums of Squares using ANOVA
#
anova_mod <- anova(reg_mod)
print(anova_mod)
#
#
