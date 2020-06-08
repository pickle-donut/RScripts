# Clear the Environment
rm(list=ls())

library(ggplot2)

# Read csv file as a DataFrame
#
df <- read.table(file.choose(), 
                 header = TRUE, sep = ',')


plotScatter <- function(data, x, y, x_name, y_name, deviation){
  qplot(data=data, x, y, xlab = x_name, ylab = y_name) +
    geom_text(aes(label=ifelse((x>deviation*IQR(x)|y>deviation*IQR(y)),paste(x, ",", y),"")), hjust=1.1) +
    theme_minimal()
}

devs <- c(3, 4, 5)
  for (i in 1:length(devs)){
    for(col in 2:ncol(df)) {
      print(plotScatter(df, df[, col], df[, 2], names(df)[col], "InfctRsk", devs[i]))
    }
  }

