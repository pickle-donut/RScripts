library(reshape)

varmatFromSdev = function (sdev){
	vars = sdev^2
	vars = vars/sum(vars)
	rbind("Standard deviation" = sdev, "Proportion of Variance" = vars, "Cumulative Proportion" = cumsum(vars))
}

dt = data.frame(inTable)
dt = dt[complete.cases(dt),]

pcaObj = princomp(dt[2:ncol(dt)], scores = TRUE, cor = TRUE)
varmat = varmatFromSdev(pcaObj$sdev)
pcaSummaryTable = data.frame(Name = dimnames[[1]], varmat, stringsAsFactors = FALSE, check.names = FALSE)
pcaSummaryTableUnpivot = melt(pcaSummaryTable, id = c("Name"))