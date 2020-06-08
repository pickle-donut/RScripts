#++++++++++++++++++++++++++++++++++++++++++
# Read in libraries.
#++++++++++++++++++++++++++++++++++++++++++
install.packages("psych")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("fmsb")
install.packages("car")

library(psych)
library(ggplot2)
#library(Hmisc)	#Only load this after using psych; it overrides psych
library(car)	#Used for Durbin-Watson test and VIF scores


#++++++++++++++++++++++++++++++++++++++++++
# Set up working directory.
#++++++++++++++++++++++++++++++++++++++++++
workingdirectory = "C:\\Users\\jonmc\\Documents\\git\\RScripts\\ml\\data\\general"
setwd(workingdirectory)


#########################################################################################

#++++++++++++++++++++++++++++++++++++++++++
# Read in Data
#++++++++++++++++++++++++++++++++++++++++++
#Read tab-delimited text file.
temptable = paste(workingdirectory, "\\test.txt", sep = "")
origHospitalData = read.table(temptable, header=T, sep = "\t")
origHospitalData

#Allow for editing of individual data elements in table.
origHospitalData = edit(origHospitalData )

#Display column names, summarize data, display datatypes, and convert datatypes.
names(origHospitalData)		
str(origHospitalData)
origHospitalData$PositionID = as.factor(origHospitalData$PositionID)
origHospitalData$Compensation = as.factor(origHospitalData$Compensation)
origHospitalData$MaxTerm = as.factor(origHospitalData$MaxTerm)

#Get the number of incomplete cases				
sum(!complete.cases(origHospitalData)) 

#Create subsets. hospitalData1 for logistic regression, hospitalData2 for multiple regression.
numeric_cols = origHospitalData[,sapply(origHospitalData, is.numeric)]
hospitalData1 = data.frame(origHospitalData['DonorType'],numeric_cols)
hospitalData2 = numeric_cols


#########################################################################################

#+++++++++++++++++++++++++++++++++++++++++++++++
#Create training, testing, and validation sets.
#+++++++++++++++++++++++++++++++++++++++++++++++
#Set percentages.
train.size = 0.6
valid.size = 0.2
test.size = 0.2

#Calculate the sample sizes.
samp.train = floor(train.size * nrow(hospitalData1))
samp.valid = floor(valid.size * nrow(hospitalData1))
samp.test = floor(test.size * nrow(hospitalData1))

#Determine the indices in each subset.
indices.train = sort(sample(seq_len(nrow(hospitalData1)), size = samp.train))
indices.valid_test = setdiff(seq_len(nrow(hospitalData1)), indices.train)
indices.valid = sort(sample(indices.valid_test, size = samp.valid))
indices.test = setdiff(indices.valid_test, indices.valid)

#Use indices to select the data from dataframe.
hospitalData1.train = hospitalData1[indices.train,]
hospitalData1.valid = hospitalData1[indices.valid,]
hospitalData1.test = hospitalData1[indices.test,]


#########################################################################################
#Set percentages.
train.size = 0.6
valid.size = 0.2
test.size = 0.2

#Calculate the sample sizes.
samp.train = floor(train.size * nrow(hospitalData2))
samp.valid = floor(valid.size * nrow(hospitalData2))
samp.test = floor(test.size * nrow(hospitalData2))

#Determine the indices in each subset.
indices.train = sort(sample(seq_len(nrow(hospitalData2)), size = samp.train))
indices.valid_test = setdiff(seq_len(nrow(hospitalData2)), indices.train)
indices.valid = sort(sample(indices.valid_test, size = samp.valid))
indices.test = setdiff(indices.valid_test, indices.valid)

#Use indices to select the data from dataframe.
hospitalData2.train = hospitalData2 [indices.train,]
hospitalData2.valid = hospitalData2 [indices.valid,]
hospitalData2.test = hospitalData2 [indices.test,]


#########################################################################################

#################################################
#==============Descriptive Analysis=============#
#################################################
#Descriptive statistics of hospitalData1.
names(hospitalData1)
unique(hospitalData1.train$DonorType)
unique(hospitalData1.valid$DonorType)
unique(hospitalData1.test$DonorType)
describe(hospitalData1.train)			
describe(hospitalData1.valid)
describe(hospitalData1.test)
summary(hospitalData1.train)			
summary(hospitalData1.valid)			
summary(hospitalData1.test)	

#Descriptive statistics of hospitalData2.
names(hospitalData2)
describe(hospitalData2.train)			
describe(hospitalData2.valid)
describe(hospitalData2.test)
summary(hospitalData2.train)			
summary(hospitalData2.valid)			
summary(hospitalData2.test)	


#################################################
#==============Logistic Regression==============#
#################################################
#Perform logistic regression using DonorType for hospitalData1.train.
donorType_reg1 = glm(DonorType~InOperExp*OutOperExp, binomial, data=hospitalData1.train)
summary(donorType_reg1)
anova(donorType_reg1, test="Chisq")
donorType_reg2 = glm(DonorType~InOperExp+OutOperExp, binomial, data=hospitalData1.train)
summary(donorType_reg2)

	
#Perform logistic regression using DonorType for hospitalData1.valid.
donorType_reg1 = glm(DonorType~InOperExp+OutOperExp, binomial, data=hospitalData1.valid)
summary(donorType_reg1)
anova(donorType_reg1, test="Chisq")

#Perform logistic regression using DonorType for hospitalData1.test. 
donorType_reg1 = glm(DonorType~InOperExp+OutOperExp, binomial, data=hospitalData1.test)
summary(donorType_reg1)
anova(donorType_reg1, test="Chisq")


#########################################################################################

########################################################
#==============Assess 5 Assumptions====================#
########################################################
#Target Variable: OperRev

########
#STEP 1#
#+++++++++++++++++++++++++++++++++++++++++++++++
#Assess linearity. Plot combinations.
#+++++++++++++++++++++++++++++++++++++++++++++++
#Plot training set.
pairs(hospitalData2.train, panel=panel.smooth)

#Plot validation set.
pairs(hospitalData2.valid, panel=panel.smooth)

#Plot testing set.
pairs(hospitalData2.test, panel=panel.smooth)


########
#STEP 2#
#+++++++++++++++++++++++++++++++++++++++++++++++
#Correlation analysis.
#+++++++++++++++++++++++++++++++++++++++++++++++
#This shows the correlation values (rounded) and their associated p-values.
rcorr(as.matrix(hospitalData2.train))

#This shows the correlation values (rounded) and their associated p-values.
rcorr(as.matrix(hospitalData2.valid))

#This shows the correlation values (rounded) and their associated p-values.
rcorr(as.matrix(hospitalData2.test))


#########################################################################################

########################################################
#==============Multiple Linear Regression==============#
########################################################

########
#STEP 3#
#+++++++++++++++++++++++++++++++++++++++++++++++
#Regression analysis and independence.
#+++++++++++++++++++++++++++++++++++++++++++++++
#Copy the results for VIF scores.
#Do this for each of the regression model.
#Independence Assumption
# (i) H0: No autocorrelation, non-significant, p > alpha --> Pass.
#(ii) HA: Autocorrelation greater than 0, significant, p <= alpha --> Fail.
names(hospitalData2.train)
hospitalData2_reg = lm(OperRev~NoFTE+NetPatRev+InOperExp+OutOperExp+AvlBeds, data = hospitalData2.train)
summary(hospitalData2_reg)

#Calculate VIF for objective collinearity assessment.
#Less than 10 minimum acceptance, preferably less than 5.
vif(hospitalData2_reg)

#Test independence assumption
durbinWatsonTest(hospitalData2_reg)

hospitalData2_reg = lm(OperRev~NoFTE+NetPatRev+InOperExp+OutOperExp+AvlBeds, data = hospitalData2.valid)
summary(hospitalData2_reg)

#Calculate VIF for objective collinearity assessment.
#Less than 10 minimum acceptance, preferably less than 5.
vif(hospitalData2_reg)

#Test independence assumption
durbinWatsonTest(hospitalData2_reg)

hospitalData2_reg = lm(OperRev~NoFTE+NetPatRev+InOperExp+OutOperExp+AvlBeds, data = hospitalData2.test)
summary(hospitalData2_reg)

#Calculate VIF for objective collinearity assessment.
#Less than 10 minimum acceptance, preferably less than 5.
vif(hospitalData2_reg)

#Test independence assumption
durbinWatsonTest(hospitalData2_reg)


##########
#STEP 4&5#
#+++++++++++++++++++++++++++++++++++++++++++++++
#Homoscedasticity and normality assessment.		
#+++++++++++++++++++++++++++++++++++++++++++++++
plot(hospitalData2_reg)		#Click "Enter" to cycle through plots.


#########################################################################################

########################################################
#==============Log Transform Regression================#
########################################################

########
#STEP 3#
#+++++++++++++++++++++++++++++++++++++++++++++++
#Regression analysis and independence.
#+++++++++++++++++++++++++++++++++++++++++++++++
#Copy the results for VIF scores.
#Do this for each of the regression model.
#Independence Assumption
# (i) H0: No autocorrelation, non-significant, p > alpha --> Pass.
#(ii) HA: Autocorrelation greater than 0, significant, p <= alpha --> Fail.
names(hospitalData2.train)
hospitalData2_reg2 = lm(log(OperRev)~NoFTE+NetPatRev+InOperExp+OutOperExp+AvlBeds, data = hospitalData2.train)
summary(hospitalData2_reg2)

#Calculate VIF for objective collinearity assessment.
#Less than 10 minimum acceptance, preferably less than 5.
vif(hospitalData2_reg2)

#Test independence assumption
durbinWatsonTest(hospitalData2_reg2)

hospitalData2_reg2 = lm(log(OperRev)~NoFTE+NetPatRev+InOperExp+OutOperExp+AvlBeds, data = hospitalData2.valid)
summary(hospitalData2_reg2)

#Calculate VIF for objective collinearity assessment.
#Less than 10 minimum acceptance, preferably less than 5.
vif(hospitalData2_reg2)

#Test independence assumption
durbinWatsonTest(hospitalData2_reg2)

hospitalData2_reg2 = lm(log(OperRev)~NoFTE+NetPatRev+InOperExp+OutOperExp+AvlBeds, data = hospitalData2.test)
summary(hospitalData2_reg2)

#Calculate VIF for objective collinearity assessment.
#Less than 10 minimum acceptance, preferably less than 5.
vif(hospitalData2_reg2)

#Test independence assumption
durbinWatsonTest(hospitalData2_reg2)


##########
#STEP 4&5#
#+++++++++++++++++++++++++++++++++++++++++++++++
#Homoscedasticity and normality assessment.		
#+++++++++++++++++++++++++++++++++++++++++++++++
plot(hospitalData2_reg2)		#Click "Enter" to cycle through plots.


#########################################################################################





