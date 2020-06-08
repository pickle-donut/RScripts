animals <- c('cat','dog','rat','python', NA)
warehouses <- seq(from = 25, to = 100, by = 25)
warehouses <- paste("IEOOOOO", warehouses, sep = "")
warehouses <- c(warehouses, NA)

df <- data.frame(anml = sample(animals, 10, replace = TRUE)
                 , ware =sample(warehouses, 10, replace = TRUE))

df.names <- names(df)
df <- sapply(df, as.character)


ls <- list()

for(c in 1:ncol(df)){
  this.col.name <- df.names[c]
  
  unique.values <- unique(df[,c])
  unique.values <- na.omit(unique.values)
  sequence <- seq(unique.values)
  identity.matrix <- data.frame(unique.values, sequence)
  
  ls[[c]] <- identity.matrix
  names(ls)[c] <- this.col.name
}

df <- data.frame(df)

for(i in 1:length(ls)){

  df.temp <- data.frame(ls[i])
  foreign.key <- names(df.temp)[i]
  names(df.temp)[1] <- names(df)[i]
  
  df <- merge(df, df.temp, by = intersect(names(df), names(df.temp)), all.x = TRUE)
  
}



