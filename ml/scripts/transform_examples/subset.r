inTable = data.frame(IDs, target, inFeatures)
inTable = na.omit(inTable)

analysisSet = inTable

train.size = 0.5
valid.size = 0.5
test.size = 0

samp.train = floor(train.size * nrow(inTable))
samp.valid = floor(train.valid * nrow(inTable))
samp.test = floor(train.test * nrow(inTable))

indices.train = sort(sample(seq_len(nrow(inTable)), size = samp.train))
indices.valid_test = setdiff(seq_len(nrow(inTable)), indices.train)
indices.valid = sort(sample(indices.valid_test), size = samp.valid))
indices.test = setdiff(indices.valid_test, indices.valid)

inTable.train = inTable[indices.train,]
inTable.valid = inTable[indices.valid,]
inTable.test = inTable[indices.test,]