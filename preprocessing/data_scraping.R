##################################################################################################
# Bureau of Labor Statistics (BLS) Resoures                                                      #
# Sub-directory where all data is stored: https://downloadt.bls.gov/pub/time.series/nb/          #
# Data set catalog: https://catalog.data.gov/dataset/employee-benefits-survey                    #
#                                                                                                #
# Purpose of this script: connect to web data source, extract html, and write to local directory #
##################################################################################################
# Install necessary packages
install.packages(c('xml2','rvest'))

# And load them for use
library('xml2')
library('rvest')

# Use current working directory and create new folder
# Point to raw data folder
project.direct <- 'C:\\Users\\jonmc\\OneDrive\\Documents\\R\\EmployeeBenefits\\'
working.direct <- paste0(project.direct, 'rawData\\')
working.direct <- setwd(working.direct)
getwd()

files.vector <- c(
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

urls <- paste0('https://downloadt.bls.gov/pub/time.series/nb/', files.vector)


for (url in urls) {
  
  # Read HTML to parse
  raw <- read_html(url)
  html <- xml_text(raw)
  
  temp.file <- paste0(gsub('https://downloadt.bls.gov/pub/time.series/nb', working.direct, url),".txt")
  
  # Name file and write it to project location
  file.conn <- file(temp.file)
  writeLines(html, file.conn)
  close(file.conn)
}

