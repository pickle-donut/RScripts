####Set up working directory.
setwd("C:\\Users\\jonmc\\Documents\\git\\RScripts\\ml\\data\\general\\")
getwd()

####Read in data.
reduction_data = read.table("ect_data.txt", header=T, sep="\t")
names(reduction_data)

#### Create dataframes for like variables.
attitude_data.pca = reduction_data[c("attitude1_01","attitude1_02","attitude1_03","attitude1_04")]
intent_data.pca = reduction_data[c("intent1_01","intent1_02","intent1_03","intent1_04")]
peruse_data.pca = reduction_data[c("peruse01","peruse02","peruse03","peruse04")]
satis_data.pca = reduction_data[c("satis01","satis02","satis03","satis04")]
reduction_data.pca = reduction_data

##############################
#Principle Component Analysis#
##############################

####Perform PCA on attitude.
pcamodel_reduc1 = princomp(attitude_data.pca,cor=TRUE)	#save PCA model with loadings, attitude subset
pcamodel_reduc1
####Perform PCA on intent.
pcamodel_reduc2 = princomp(intent_data.pca,cor=TRUE)		#save PCA model with loadings, intent subset
pcamodel_reduc2
####Perform PCA on peruse.
pcamodel_reduc3 = princomp(peruse_data.pca,cor=TRUE)		#save PCA model with loadings, peruse subset
pcamodel_reduc3
####Perform PCA on satis.
pcamodel_reduc4 = princomp(satis_data.pca,cor=TRUE)		#save PCA model with loadings, satis subset
pcamodel_reduc4
####Perform PCA on all data.
pcamodel_reduc5 = princomp(reduction_data.pca,cor=FALSE)	#save PCA model with loadings, all data
pcamodel_reduc5

####Create scree plots.
par(mfrow = c(3,2))
plot(pcamodel_reduc1,main="Attitude Scree Plot")	      #screeplot of pcamodel_reduc1
plot(pcamodel_reduc2,main="Intent Scree Plot")	            #screeplot of pcamodel_reduc2
plot(pcamodel_reduc3,main="Attitude Scree Plot")	      #screeplot of pcamodel_reduc3
plot(pcamodel_reduc4,main="Satisfaction Scree Plot")	      #screeplot of pcamodel_reduc4
plot(pcamodel_reduc5,main="All Columns Scree Plot")	      #screeplot of pcamodel_reduc5

#################
#Factor Analysis#
#################

####Perform FA on Attitude: Confirm PCA results. Factors = 1 from PCA analysis on Attitude.
reduction_data.FA1 = factanal(~attitude1_01+attitude1_02+attitude1_03+attitude1_04,factors=1,rotation="varimax", scores="none",data=reduction_data)					
reduction_data.FA1
####Perform FA on Intent: Confirm PCA results. Factors = 1 from PCA analysis on Intent.
reduction_data.FA2 = factanal(~intent1_01+intent1_02+intent1_03+intent1_04,factors=1,rotation="varimax", scores="none",data=reduction_data)					
reduction_data.FA2	
####Perform FA on Peruse: Confirm PCA results. Factors = 1 from PCA analysis on Peruse.
reduction_data.FA3 = factanal(~peruse01+peruse02+peruse03+peruse04,factors=1,rotation="varimax", scores="none",data=reduction_data)					
reduction_data.FA3
####Perform FA on Satis: Confirm PCA results. Factors = 1 from PCA analysis on Satis.
reduction_data.FA4 = factanal(~satis01+satis02+satis03+satis04,factors=1,rotation="varimax", scores="none",data=reduction_data)					
reduction_data.FA4
####Perform FA on all data(varimax): Confirm PCA results. Factors = 4 from PCA analysis on all data.
reduction_data.FA5 = factanal(~attitude1_01+attitude1_02+attitude1_03+attitude1_04+intent1_01+intent1_02+intent1_03+intent1_04+peruse01+peruse02+peruse03+peruse04+satis01+satis02+satis03+satis04,factors=4,rotation="varimax", scores="none",data=reduction_data)				
reduction_data.FA5
####Perform FA on all data (promax): Confirm PCA results. Factors = 4 from PCA analysis on all data.
reduction_data.FA6 = factanal(~attitude1_01+attitude1_02+attitude1_03+attitude1_04+intent1_01+intent1_02+intent1_03+intent1_04+peruse01+peruse02+peruse03+peruse04+satis01+satis02+satis03+satis04,factors=4,rotation="promax", scores="none",data=reduction_data)				
reduction_data.FA6	

####################
#K-Means Clustering#
####################
####Install tree library.
install.packages("tree")
library(tree)

####Read in data.
sampleData = read.table("car.test.frame.txt", header=T, sep="\t")
kmeans_data = sampleData
names(kmeans_data)
summary(kmeans_data)
str(kmeans_data)

####Define and plot K-Means.
par(mfrow=c(1,3))
plot(kmeans_data$Mileage, kmeans_data$Price, pch = 18, col = kmeans_data$Type, main = "Grouping Var.")
km = kmeans(data.frame(kmeans_data$Mileage, kmeans_data$Price),6)
plot(kmeans_data$Mileage, kmeans_data$Price, col=km[[1]], main = "6 KM Groups", xlab = "Mileage", ylab = "Price")
km2 = kmeans(data.frame(kmeans_data$Mileage, kmeans_data$Price),8)
plot(kmeans_data$Mileage, kmeans_data$Price, col=km2[[1]], main = "8 KM Groups", xlab = "Mileage", ylab = "Price")
table(km[[1]], kmeans_data$Type)
table(km2[[1]], kmeans_data$Type)

####Perform agglomerative analysis.
sample_dist = dist(sampleData)
sample_clust = hclust(sample_dist)
plot(sample_clust, labels=sample_labels, main = "Agglomerative Clustering")


