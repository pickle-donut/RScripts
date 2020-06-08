library(cluster)

myData = data.frame(IDs, target, lat, long, inFeatures)
myData = na.omit(myData)

gapData = data.frame(lat, long, inFeatures)
gapData = na.omit(gapData)
gapResults = clusGap(scale(gapData), FUN = kmeans, K.max = clusterNum, B = 500)
k = maxSE(gapResults$Tab[,"gap"], gapResults$Tab[,"SE.sim"], method = "Tibs2001SEMax")

km = kmeans(scale(myData[-c(1,2,3)]), k)
kMeansTable = data.frame(myData, kMeansCluster = km$cluster)

goodnessOfFit = data.frame(km$betweenss / km$totss)