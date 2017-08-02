#1-Data Acquisition
#wdbc <- read.table(file.choose(), sep = ',')
wdbc_orig <- read.table("D:/R Docs/ML code/DATA SET/wdbc.data", sep = ',')

#removing 1st column(ID number)
wdbc_for_model <- wdbc_orig[ ,-1]

#removing 1st and 2nd(Diagnosis, M = malignant, B = benign) column from original dataset
wdbc <- wdbc_orig[ ,-c(1,2)]
#View(wdbc)
typeof(wdbc)
#normalizing values to make range of all variables between 0 and 1
normlize <- function(x){(x - min(x))/(max(x)-min(x))}  #defined a normalization function

wdbc_norm <- as.data.frame(lapply(wdbc, normlize))
#View(wdbc_norm)

#summary before and after Normalization
summary(wdbc[ ,1:5])
summary(wdbc_norm[ ,1:5])

#Splliting dataset into training and testing
wdbc_norm_train <- wdbc_norm[1:450, ]
wdbc_norm_test <- wdbc_norm[451:569, ]

#Implementing k-NN model
#to classifies each patiants into Diagnosis category,(M = malignant, B = benign)
#1st column of wdbc_for_model which is our Target variable
library(class)
train_target <- wdbc_for_model[1:450, 1]
test_target <- wdbc_for_model[451:569, 1]
knn_model <- knn(wdbc_norm_train, wdbc_norm_test, cl = train_target, k=21)
knn_model

library(animation)
knn.ani(wdbc_norm_train[,c(3,4)], wdbc_norm_test[,c(3,4)], cl = train_target, k=21)

#confusion matrix to check accuracy
table(knn_model, test_target)

#knn_model B   M
#       B  92  2
#       M  0   25

#accuracy = 0.9831933 <-[(92+25)/(92+25+0+2)]

