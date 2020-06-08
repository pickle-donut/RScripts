#R - Question Group 1#
########################################################################################################################################################################################
car_frame_data = read.table("C:\\Users\\jonmc\\Documents\\git\\RScripts\\ml\\data\\general\\car.test.frame.txt", header = T, sep = "\t")

names(car_frame_data)

ncol(car_frame_data)

nrow(car_frame_data)

unique(car_frame_data)

car_frame_data[,sapply(car_frame_data,is.factor)]

unique(car_frame_data$Type)

unique(car_frame_data$Country)

car_frame_data[57,3]

car_frame_data[24,]

car_frame_data[29,1:3] #Method 1#
car_frame_data[29,c(1,2,3)] #Method 2#
car_frame_data[29,c("Price", "Country", "Reliability")] #Method 3#

car_frame_data[29,c(3,7)] #Method 1#
car_frame_data[29,c("Reliability", "Disp.")] #Method 2#

newHP1 = car_frame_data[,8] #Method 1#
newHP2 = car_frame_data$HP #Method 2#

#R - Question Group 2#
########################################################################################################################################################################################
car_frame_data[car_frame_data$Type=="Compact" & car_frame_data$Reliability>=4,]

car_frame_data[car_frame_data$Type=="Compact" & car_frame_data$Country=="Japan" & car_frame_data$Reliability>=3,]

nrow(car_frame_data[car_frame_data$Country=="Japan/USA",])

nrow(car_frame_data[car_frame_data$Country=="Japan" | car_frame_data$Country=="USA" | car_frame_data$Country=="Japan/USA",])

nrow(car_frame_data[car_frame_data$Country=="Japan/USA" & car_frame_data$Reliability>=4,])

split.num = round(nrow(car_frame_data)*.70,0)
nrow(car_frame_data)
x = 1:60
car_frame_data.split = car_frame_data[sample(x,split.num,replace=F),]
nrow(car_frame_data.split)

samp.size = round(nrow(car_frame_data)/8, 0)
indices.one = sort(sample(seq_len(nrow(car_frame_data)), size=samp.size))
indices.not_1 = setdiff(seq_len(nrow(car_frame_data)), indices.one)
indices.two = sort(sample(indices.not_1, size=samp.size))
indices.not_12 = setdiff(indices.not_1, indices.two)
indices.three = sort(sample(indices.not_12, size=samp.size))
indices.not_123 = setdiff(indices.not_12, indices.three)
indices.four = sort(sample(indices.not_123, size=samp.size))
indices.not_1234 = setdiff(indices.not_123, indices.four)
indices.five = sort(sample(indices.not_1234, size=samp.size))
indices.not_12345 = setdiff(indices.not_1234, indices.five)
indices.six = sort(sample(indices.not_12345, size=samp.size))
indices.not_123456 = setdiff(indices.not_12345, indices.six)
indices.seven = sort(sample(indices.not_123456, size=samp.size))
indices.not_1234567 = setdiff(indices.not_123456, indices.seven)
indices.eight = sort(sample(indices.not_1234567, size=samp.size))
car_frame_data.1 = car_frame_data[indices.one,]
car_frame_data.2 = car_frame_data[indices.two,]
car_frame_data.3 = car_frame_data[indices.three,]
car_frame_data.4 = car_frame_data[indices.four,]
car_frame_data.5 = car_frame_data[indices.five,]
car_frame_data.6 = car_frame_data[indices.six,]
car_frame_data.7 = car_frame_data[indices.seven,]
car_frame_data.8 = car_frame_data[indices.eight,]

car_data = car_frame_data[,sapply(car_frame_data,is.numeric)]

car_data = subset(car_data, select=-c(Price,HP)) 

country_weight = subset(car_frame_data, select=c(Country,Weight)) 

colnames(country_weight) = c("cntry", "wght")












