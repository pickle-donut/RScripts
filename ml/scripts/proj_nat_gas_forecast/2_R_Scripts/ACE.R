#############################################
#=============Read in Libraries=============#
#############################################
install.packages("acepack")
install.packages("psych")
install.packages("Hmisc")

library(acepack)
library(psych)


#########################################################
#==============Setup the Working Directory==============#
#########################################################
workingdirectory = "C:\\Users\\jonmc\\Desktop\\Gradz School\\Semester # 2\\OOP for Data Science\\Project\\Data-CSV"
setwd(workingdirectory)


#############################################
#===============Read in data================#
#############################################
projData = read.table(file.choose(), sep = ',', header = TRUE)
str(projData)
colnames(projData)
projData$Report_Date = strptime(as.character(projData$Report_Date), "%Y-%m-%d")

#Standardize variables.
projData$Crude_Price = ((projData$Crude_Price - mean(projData$Crude_Price)) / sd(projData$Crude_Price))
projData$Gas_Price = ((projData$Gas_Price - mean(projData$Gas_Price)) / sd(projData$Gas_Price))
projData$Gold_Price = ((projData$Gold_Price - mean(projData$Gold_Price)) / sd(projData$Gold_Price))
projData = projData[,2:4]


###############################################
#===============Subsample Data================#
###############################################
#Set percentages.
train.size = 0.6
valid.size = 0.4
test.size = 0

#Calculate the sample sizes.
samp.train = floor(train.size * nrow(projData))
samp.valid = floor(valid.size * nrow(projData))
samp.test = floor(test.size * nrow(projData))

#Determine the indices in each subset.
indices.train = sort(sample(seq_len(nrow(projData)), size = samp.train))
indices.valid_test = setdiff(seq_len(nrow(projData)), indices.train)
indices.valid = sort(sample(indices.valid_test, size = samp.valid))
indices.test = setdiff(indices.valid_test, indices.valid)

#Use indices to select the data from dataframe.
projData.train = projData[indices.train,]
projData.valid = projData[indices.valid,]
projData.test = projData[indices.test,]


#################################################
#==============Descriptive Analysis=============#
#################################################
#Descriptive statistics of projData.
describe(projData.train)			
describe(projData.valid)
summary(projData.train)			
summary(projData.valid)			


###################################################
#===============ACE/AVAS Functions================#
###################################################
#Functions provided by Brant Deppa at Winona University.
saceplot = function (x, y, a, xname = "x", yname = "y") {
  par(mfrow = c(2, 2), pty = "m")
  plot(x, a$tx, xlab = paste("Untransformed ", xname), ylab = 
  paste("Transformed ",xname), main = "Plot of Transformed x vs. x",
  cex = 0.6,pch=20)
  rug(x)
  plot(y, a$ty, xlab = paste("Untransformed ", yname), ylab =
  paste("Transformed ", yname), main = "Plot of Transformed y vs. y", cex =  
  0.6,pch=20)
  rug(y)
  plot(a$tx, a$ty, main = "Plot of Transformed y vs. Transformed x", 
       xlab = "Transformed x", ylab = "Transformed y", cex = 0.6,pch=20)
  r <- a$ty - a$tx
  rug(a$tx)
  plot(a$tx, r, xlab = "Fit (tx)", ylab = "Residuals", 
  sub = "Residuals vs. Fit ", cex = 0.6,pch=20)
  abline(h=0,lty=2,col="red")
  par(mfrow = c(1, 1))
  invisible()
}


maceplot = function (xmat, y, x, nrow = 2, ncol = 2) {
    par(mfrow = c(nrow, ncol), ask = T)
    for (i in 1:ncol(xmat)) {
        plot(xmat[, i], x$tx[, i], xlab = dimnames(xmat)[[2]][i], 
            ylab = "Transformed x", pch = 20,col="red")
        rug(xmat[, i])
    }
    plot(y, x$ty, xlab = "y", ylab = "transformed y", cex = 0.7, 
        pch = 20,col="blue")
    fit <- rep(0, length(y))
    for (k in 1:ncol(xmat)) {
        fit <- fit + x$tx[, k]
    }
    plot(fit, x$ty, main = "Plot of Transformed y vs. Fit", 
        xlab = "Fit", ylab = "Transformed y ", pch = 20,col="blue")
    r <- x$ty - fit
    plot(fit, r, xlab = "tx", ylab = "residuals", sub = "Residuals vs. 
    Fit",pch = 20,col="red")
    abline(h = 0, col = "black", lty = 2)
    par(mfrow = c(1, 1), ask = F)
    invisible()
}


#+++++++++++++++++++++++++++++++++++++++++++++++
#Assess linearity. Plot combinations.
#+++++++++++++++++++++++++++++++++++++++++++++++
#Plot training set.
pairs(projData.train, panel=panel.smooth)

######################################################################################################

#Plot validation set.
pairs(projData.valid, panel=panel.smooth)


#+++++++++++++++++++++++++++++++++++++++++++++++
#Correlation analysis.
#+++++++++++++++++++++++++++++++++++++++++++++++
library(Hmisc)	#Only load this after using psych; it overrides psych

#Training set.
#This shows the correlation values (rounded) and their associated p-values.
rcorr(as.matrix(projData.train))

######################################################################################################

#Validation set.
#This shows the correlation values (rounded) and their associated p-values.
rcorr(as.matrix(projData.valid))


####################################################################
#===============Create Linear Regression Models====================#
####################################################################

#+++++++++++++++++++++++++++++++++++++++++++++++
#Regression analysis.
#+++++++++++++++++++++++++++++++++++++++++++++++
names(projData)

#Training set.
projData_reg1 = lm(Gas_Price ~ Crude_Price + Gold_Price, data = projData.train)
summary(projData_reg1)

######################################################################################################

#Validation set.
projData_reg2 = lm(Gas_Price ~ Crude_Price + Gold_Price, data = projData.valid)
summary(projData_reg2)

#+++++++++++++++++++++++++++++++++++++++++++++++
#Homoscedasticity and normality assessment.		
#+++++++++++++++++++++++++++++++++++++++++++++++
#Plot training set.
par(mfrow=c(2,1))
plot(projData_reg1)

######################################################################################################

#Plot validation set.
par(mfrow=c(2,1))
plot(projData_reg2)


###################################################
#===============ACE/AVAS Model====================#
###################################################
#Plot training set.
#Create multiple ACE plots, one for each variable.
par(mfrow=c(2,2))
x = model.matrix(Gas_Price ~ .,data=projData.train)[,-1]
y = projData$Gas_Price
ace.projData1 = ace(x,y)

#Returns the r-squared.
#A linear regression of Y on the individual variables yields a maximum multiple correlation R^2
ace.projData1$rsq      
maceplot(x,y,ace.projData1)  

######################################################################################################

#Plot validation set.
#Create multiple ACE plots, one for each variable.
par(mfrow=c(2,2))
x = model.matrix(Gas_Price ~ .,data=projData.valid)[,-1]
y = projData$Gas_Price
ace.projData1 = ace(x,y)

#Returns the r-squared.
#A linear regression of Y on the individual variables yields a maximum multiple correlation R^2
ace.projData1$rsq      
maceplot(x,y,ace.projData1)  


##################################################################################
#===============Create ACE-Corrected Linear Regression Models====================#
##################################################################################

#+++++++++++++++++++++++++++++++++++++++++++++++
#Regression analysis.
#+++++++++++++++++++++++++++++++++++++++++++++++
names(projData)

#Training set.
projData_reg1 = lm(Gas_Price~ poly(-1*Crude_Price,4) + poly(Gold_Price,4), data = projData.train)
summary(projData_reg1)

######################################################################################################

#Validation set.
projData_reg2 = lm(Gas_Price ~ poly(-1*Crude_Price,5) + poly(-1*Gold_Price,5), data = projData.valid)
summary(projData_reg2)


#+++++++++++++++++++++++++++++++++++++++++++++++
#Homoscedasticity and normality assessment.		
#+++++++++++++++++++++++++++++++++++++++++++++++
#Plot training set.
par(mfrow=c(2,1))
plot(projData_reg1)

######################################################################################################

#Plot validation set.
par(mfrow=c(2,1))
plot(projData_reg2)



