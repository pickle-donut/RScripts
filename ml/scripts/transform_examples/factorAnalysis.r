library(reshape)

dt = data.frame(inTable)
dt = dt[complete.cases(dt),]

fit = factanal(dt[2:ncol(dt), as.numeric(numFactors), rotation='varimax')
rloads = unclass(loadings(fit))
variable = row.names(rloads)
uniqueness = unclass(fit$uniqueness)
rotatedLoadings = data.frame(variable,uniqueness,rloads)
rotatedLoadingsUnpivot = melt(rotatedLoadings, id = c("Variable"))