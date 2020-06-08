#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Read Libraries.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
install.packages("dummies")

library(psych)
library(plyr)
library(car) #Used for Durbin-Watson test, VIF scores, binning
library(dummies) #Used to create dummy variables


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Set up working directory.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
workingdirectory = "C:\\Users\\jonmc\\Documents\\git\\RScripts\\ml\\data\\general"
setwd(workingdirectory)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Set up working directory.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
temptable = paste(workingdirectory, "\\splityield.txt", sep = "")
origSplitData = read.table(temptable, sep = '\t', header = TRUE)
colnames(origSplitData)
str(origSplitData)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Scrub data.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Get number of complete cases and remove missing data.
complete.cases(origSplitData)
splitsamp_data = na.omit(origSplitData )

# Map factor variables with numeric values.
blockVec = as.factor(mapvalues(splitsamp_data$block, from = c("A", "B", "C", "D"), to = c(1, 2, 3, 4)))
irrigationVec = as.factor(mapvalues(splitsamp_data$irrigation, from = c("control", "irrigated"), to = c(1, 2)))
densityVec = as.factor(mapvalues(splitsamp_data$density, from = c("high", "medium", "low"), to = c(1, 2, 3)))
fertilizerVec = as.factor(mapvalues(splitsamp_data$fertilizer, from = c("N", "P", "NP"), to = c(1, 2, 3)))

# Create a data frame with converted data.
splitsamp_data = data.frame(splitsamp_data['yield'], blockVec, irrigationVec, densityVec, fertilizerVec)
colnames(splitsamp_data) = c('yield', 'block', 'irrigation', 'density', 'fertilizer')
str(splitsamp_data)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create dummy variables.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create dummy variables for block.
split_dummy1 = dummy(splitsamp_data$block, sep = '_')
colnames(split_dummy1)
colnames(split_dummy1) = c('Block_A', 'Block_B', 'Block_C', 'Block_D')
split_dummy1 = as.data.frame(split_dummy1)
splitsamp_data = data.frame(splitsamp_data, split_dummy1)

# Create dummy variables for irrigation.
split_dummy2 = dummy(splitsamp_data$irrigation, sep = '_')
colnames(split_dummy2)
colnames(split_dummy2) = c('Irr_Control', 'Irr_Irrigated')
split_dummy2 = as.data.frame(split_dummy2)
splitsamp_data = data.frame(splitsamp_data, split_dummy2)

# Create dummy variables for density.
split_dummy3 = dummy(splitsamp_data$density, sep = '_')
colnames(split_dummy3)
colnames(split_dummy3) = c('Density_Hi', 'Density_Med', 'Density_Lo')
split_dummy3 = as.data.frame(split_dummy3)
splitsamp_data = data.frame(splitsamp_data, split_dummy3)

# Create dummy variables for fertilizer.
split_dummy4 = dummy(splitsamp_data$fertilizer, sep = '_')
colnames(split_dummy4)
colnames(split_dummy4) = c('Fert_N', 'Fert_P', 'Fert_NP')
split_dummy4 = as.data.frame(split_dummy4)
splitsamp_data = data.frame(splitsamp_data, split_dummy4)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create regression model with all variables.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
colnames(splitsamp_data)
split_reg1 = lm(yield~Block_A + Block_B + Block_C + Irr_Control + Density_Hi + Density_Med + Fert_N + Fert_P, data = splitsamp_data)
summary(split_reg1)

# Assess multicollinearity.
vif(split_reg1)

# Assess independence.
durbinWatsonTest(split_reg1)

# Assess homoscedasticity and normality.
plot(split_reg1)