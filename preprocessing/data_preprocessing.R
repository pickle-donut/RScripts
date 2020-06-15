##################################################################################################
# Bureau of Labor Statistics (BLS) Resoures                                                      #
# Sub-directory where all data is stored: https://downloadt.bls.gov/pub/time.series/nb/          #
# Data set catalog: https://catalog.data.gov/dataset/employee-benefits-survey                    #
#                                                                                                #
# Purpose of this script: start preprocessing by recoding joining and rewrite of files           #
##################################################################################################
# Install necessary packages
# install.packages('tidyverse')
install.packages(c('readr', 'dplyr', 'lubridate', 'purrr', 'tidyr', 'ggplot2', 'stringr','tibble','forcats'))

# And load them for use
library('readr')
library('dplyr')
library('purrr')
library('tidyr')
library('tibble')
library('ggplot2')
library('stringr')
library('forcats')
library('lubridate')

# Point to raw data folder
project.direct <- 'C:\\Users\\jonmc\\OneDrive\\Documents\\R\\EmployeeBenefits\\'
working.direct <- paste0(project.direct, 'rawData\\')
working.direct <- setwd(working.direct)
getwd()

file.names <- c(
                  'nb.aspect'
                  , 'nb.contacts'
                  , 'nb.data.1.AllData'
                  , 'nb.datatype'
                  , 'nb.estimate'
                  , 'nb.footnote'
                  , 'nb.industry'
                  , 'nb.occupation'
                  , 'nb.ownership'
                  , 'nb.provision'
                  , 'nb.series'
                  , 'nb.subcell'
                  , 'nb.survey'
)

# Read files into list object
data.files <- lapply(Sys.glob('*.txt'), read.delim)

# Apply names to list elements
names(data.files) <- file.names

# Testing list manipulation
# data.files$nb.survey
# data.files["nb.occupation"]

df <- as.data.frame(data.files["nb.occupation"])
names(df) <- sub('^nb.\w+\.', '', names(df))

summary(df)
  