library(randomForest)

features = data.frame(inFeatures)
myData = data.frame(IDs, target, features)
myData = na.omit(myData)
myData = myData[-c(1,2)]

rdf = randomForest(x = myData, y = actual, nodesize = nodesize, importance = TRUE, keep.forest = TRUE, ntree = ntree)
predictedRDF = predict(rdf, myData)
columns = colnames(features)

varImportance = data.frame(features = columns, importance(rdf))
