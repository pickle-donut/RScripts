#+++++++++++++++++++++++++++++++++
#Read in libraries for functions.
#+++++++++++++++++++++++++++++++++
install.packages("psych")
install.packages("ggplot2")
install.packages("tree")

library(psych)
library(ggplot2)
library(tree)

#+++++++++++++++++++++++++++++++++
#Read in libraries for functions.
#+++++++++++++++++++++++++++++++++
workingdirectory = "C:\\Users\\jonmc\\Documents\\git\\RScripts\\ml\\data\\general"
setwd(workingdirectory)
getwd()

#+++++++++++++++++++++++++++++++++
#Read in data.
#+++++++++++++++++++++++++++++++++
#Read tab-delimited text file.
temptable = paste(workingdirectory, "\\test.txt", sep = "")
origHospitalData = read.table(temptable, header=T, sep = "\t")
origHospitalData

#Display column names, summarize data, display datatypes.
names(origHospitalData)
summary(origHospitalData)			
str(origHospitalData)

#Get the number of incomplete cases				
sum(!complete.cases(sampleData)) 

#Create four subsets for the four trees.
hospitalData1 = origHospitalData[c('OperInc','NoFTE','NetPatRev','InOperExp','OutOperExp','OperRev','AvlBeds')]
hospitalData2 = origHospitalData[c('OperRev','NoFTE','NetPatRev','InOperExp','OutOperExp','OperInc','AvlBeds')]
hospitalData3 = origHospitalData[c('TypeControl','NoFTE','NetPatRev','InOperExp','OutOperExp','OperRev','OperInc','AvlBeds')]
hospitalData4 = origHospitalData[c('DonorType','NoFTE','NetPatRev','InOperExp','OutOperExp','OperRev','OperInc','AvlBeds')]

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
samp.train = floor(train.size * nrow(hospitalData2 ))
samp.valid = floor(valid.size * nrow(hospitalData2 ))
samp.test = floor(test.size * nrow(hospitalData2 ))

#Determine the indices in each subset.
indices.train = sort(sample(seq_len(nrow(hospitalData2 )), size = samp.train))
indices.valid_test = setdiff(seq_len(nrow(hospitalData2 )), indices.train)
indices.valid = sort(sample(indices.valid_test, size = samp.valid))
indices.test = setdiff(indices.valid_test, indices.valid)

#Use indices to select the data from dataframe.
hospitalData2.train = hospitalData2 [indices.train,]
hospitalData2.valid = hospitalData2 [indices.valid,]
hospitalData2.test = hospitalData2 [indices.test,]

#########################################################################################
#Set percentages.
train.size = 0.6
valid.size = 0.2
test.size = 0.2

#Calculate the sample sizes.
samp.train = floor(train.size * nrow(hospitalData3 ))
samp.valid = floor(valid.size * nrow(hospitalData3 ))
samp.test = floor(test.size * nrow(hospitalData3 ))

#Determine the indices in each subset.
indices.train = sort(sample(seq_len(nrow(hospitalData3 )), size = samp.train))
indices.valid_test = setdiff(seq_len(nrow(hospitalData3 )), indices.train)
indices.valid = sort(sample(indices.valid_test, size = samp.valid))
indices.test = setdiff(indices.valid_test, indices.valid)

#Use indices to select the data from dataframe.
hospitalData3.train = hospitalData3 [indices.train,]
hospitalData3.valid = hospitalData3 [indices.valid,]
hospitalData3.test = hospitalData3 [indices.test,]

#########################################################################################
#Set percentages.
train.size = 0.6
valid.size = 0.2
test.size = 0.2

#Calculate the sample sizes.
samp.train = floor(train.size * nrow(hospitalData4))
samp.valid = floor(valid.size * nrow(hospitalData4))
samp.test = floor(test.size * nrow(hospitalData4))

#Determine the indices in each subset.
indices.train = sort(sample(seq_len(nrow(hospitalData4)), size = samp.train))
indices.valid_test = setdiff(seq_len(nrow(hospitalData4)), indices.train)
indices.valid = sort(sample(indices.valid_test, size = samp.valid))
indices.test = setdiff(indices.valid_test, indices.valid)

#Use indices to select the data from dataframe.
hospitalData4.train = hospitalData4[indices.train,]
hospitalData4.valid = hospitalData4[indices.valid,]
hospitalData4.test = hospitalData4[indices.test,]

#+++++++++++++++++++++++++++++++++++++++++++++++
#Commence regression tree analysis.
#+++++++++++++++++++++++++++++++++++++++++++++++
nrow(hospitalData1.train)
hospitalData1.train_tree = tree(hospitalData1.train)
hospitalData1.train_tree
plot(hospitalData1.train_tree)
text(hospitalData1.train_tree)

#Prune the train tree if necessary.
hospitalData1_prune_tree = prune.tree(hospitalData1.train_tree)
hospitalData1_prune_tree
plot(hospitalData1_prune_tree)
#No further refinement needed.

nrow(hospitalData2.train)
hospitalData2.train_tree = tree(hospitalData2.train)
hospitalData2.train_tree
plot(hospitalData2.train_tree)
text(hospitalData2.train_tree)

#Prune the train tree if necessary.
hospitalData2_prune_tree = prune.tree(hospitalData2.train_tree)
hospitalData2_prune_tree
plot(hospitalData2_prune_tree)
hospitalData2_prune_tree2 = prune.tree(hospitalData2.train_tree, best=2)
hospitalData2_prune_tree2
plot(hospitalData2_prune_tree2)
text(hospitalData2_prune_tree2)

nrow(hospitalData1.valid)									
hospitalData1.valid_tree = tree(hospitalData1.valid) 
hospitalData1.valid_tree
plot(hospitalData1.valid_tree)
text(hospitalData1.valid_tree)

#Prune the valid tree if necessary.
hospitalData1_prune_tree = prune.tree(hospitalData1.valid_tree)
hospitalData1_prune_tree
plot(hospitalData1_prune_tree)
#No further refinement needed.

nrow(hospitalData2.valid)
hospitalData2.valid_tree = tree(hospitalData2.valid)
hospitalData2.valid_tree
plot(hospitalData2.valid_tree)
text(hospitalData2.valid_tree)

#Prune the valid tree if necessary.
hospitalData2_prune_tree = prune.tree(hospitalData2.valid_tree)
hospitalData2_prune_tree
plot(hospitalData2_prune_tree)
#No further refinement needed.

nrow(hospitalData1.test)
hospitalData1.test_tree = tree(hospitalData1.test)
hospitalData1.test_tree
plot(hospitalData1.test_tree)
text(hospitalData1.test_tree)

#Prune the test tree if necessary.
hospitalData1_prune_tree = prune.tree(hospitalData1.test_tree)
hospitalData1_prune_tree
plot(hospitalData1_prune_tree)
#No further refinement needed.

nrow(hospitalData2.test)
hospitalData2.test_tree = tree(hospitalData2.test)
hospitalData2.test_tree
plot(hospitalData2.test_tree)
text(hospitalData2.test_tree)

#Prune the test tree if necessary.
hospitalData2_prune_tree = prune.tree(hospitalData2.test_tree)
hospitalData2_prune_tree
plot(hospitalData2_prune_tree)
#No further refinement needed.

#+++++++++++++++++++++++++++++++++++++++++++++++
#Commence classification tree analysis.
#+++++++++++++++++++++++++++++++++++++++++++++++
nrow(hospitalData3.train)
hospitalData3.train_tree = tree(TypeControl~., hospitalData3.train, mindev=1e-6, minsize=2)
hospitalData3.train_tree
plot(hospitalData3.train_tree)
text(hospitalData3.train_tree)

#Prune the train tree if necessary.
hospitalData3_prune_tree = prune.tree(hospitalData3.train_tree)
hospitalData3_prune_tree
plot(hospitalData3_prune_tree)
hospitalData3_prune_tree2 = prune.tree(hospitalData3.train_tree, best=5)
hospitalData3_prune_tree2
plot(hospitalData3_prune_tree2)
text(hospitalData3_prune_tree2)

nrow(hospitalData4.train)					
hospitalData4.train_tree = tree(DonorType~., hospitalData4.train, mindev=1e-6, minsize=2)
hospitalData4.train_tree
plot(hospitalData4.train_tree)
text(hospitalData4.train_tree)

#Prune the train tree if necessary.
hospitalData4_prune_tree = prune.tree(hospitalData4.train_tree)
hospitalData4_prune_tree
plot(hospitalData4_prune_tree)
hospitalData4_prune_tree2 = prune.tree(hospitalData4.train_tree, best=2)
hospitalData4_prune_tree2
plot(hospitalData4_prune_tree2)
text(hospitalData4_prune_tree2)

nrow(hospitalData3.valid)
hospitalData3.valid_tree = tree(TypeControl~., hospitalData3.valid, mindev=1e-6, minsize=2)
hospitalData3.valid_tree
plot(hospitalData3.valid_tree)
text(hospitalData3.valid_tree)

#Prune the valid tree if necessary.
hospitalData3_prune_tree = prune.tree(hospitalData3.valid_tree)
hospitalData3_prune_tree
plot(hospitalData3_prune_tree)
#No further refinement needed.

nrow(hospitalData4.valid)
hospitalData4.valid_tree = tree(DonorType~., hospitalData4.valid, mindev=1e-6, minsize=2)
hospitalData4.valid_tree
plot(hospitalData4.valid_tree)
text(hospitalData4.valid_tree)

#Prune the valid tree if necessary.
hospitalData4_prune_tree = prune.tree(hospitalData4.valid_tree)
hospitalData4_prune_tree
plot(hospitalData4_prune_tree)
#No further refinement needed.

nrow(hospitalData3.test)
hospitalData3.test_tree = tree(TypeControl~., hospitalData3.test, mindev=1e-6, minsize=2)
hospitalData3.test_tree
plot(hospitalData3.test_tree)
text(hospitalData3.test_tree)

#Prune the test tree if necessary.
hospitalData3_prune_tree = prune.tree(hospitalData3.test_tree)
hospitalData3_prune_tree
plot(hospitalData3_prune_tree)
#No further refinement needed.

nrow(hospitalData4.test)
hospitalData4.test_tree = tree(DonorType~., hospitalData4.test, mindev=1e-6, minsize=2)
hospitalData4.test_tree
plot(hospitalData4.test_tree)
text(hospitalData4.test_tree)

#Prune the test tree if necessary.
hospitalData4_prune_tree = prune.tree(hospitalData4.test_tree)
hospitalData4_prune_tree
plot(hospitalData4_prune_tree)
#No further refinement needed.


