#1-Data Acquisition
#Load Diabetes in Pima Indian Women dataset
library(MASS)
data("Pima.te")

#2-Divide Dataset
#we will divide dataset into training data and testing data
set.seed(2)
library(caTools) #sample.split function is present in this package

split <- sample.split(Pima.te, SplitRatio = 0.7) 
#split

training_data <- subset(Pima.te, split== 'TRUE')
testing_data <- subset(Pima.te, split== 'FALSE')

#4-Implement Logistic model basedon 'wt' and 'disp' only to predict 'vs'
logistic_model <- glm(type ~ ., data = training_data, family='binomial')
summary(logistic_model)

#5-Optimize Model
#removing all except 'glu' and 'ped'
#logistic_model <- glm(type ~ glu+ped , data = training_data, family='binomial')
#summary(logistic_model)


#6-Model validation
#predicting values of 'vs' for testing dataset
prediction <- predict(logistic_model, testing_data, type = 'response')
prediction

#"prediction" returns Probabilities , so we convert them into "0" and "1" by using if-else
predicted_test=ifelse(prediction>0.5, 1, 0)


library(caret)
confusionMatrix(table(Prediction=predicted_test, Actual = testing_data$type))


#need to choose optimum threshold value of probability by drawing ROCR
#Threshold is choosen on the basis of prediction of traning data (not on testing data)
test <- predict(logistic_model, training_data, type = 'response')

library(ROCR)

ROCRpred <- prediction(test, training_data$type)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')

plot(ROCRperf, colourize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

#so from above plot we found that Threshold can be set be set 0.4 or 0.45
#we have to maintain maximum True positive rate and Minimum False positive rate
table(Actual=testing_data$type, predicted=prediction>0.4)
table(Actual=testing_data$type, predicted=prediction>0.45)


#we found that 0.4 is best to set Threshold probability value.

#7-predicting for new dataset
new_data <- list(npreg=4, glu=110, bp=45, skin=47, bmi=51.3, ped=0.776, age=30)
predicted_type<- predict(logistic_model, new_data, type = 'response')
predicted_type

