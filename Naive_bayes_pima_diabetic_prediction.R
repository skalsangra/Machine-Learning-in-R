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

#3-creating model
library(e1071)
library(caret)#for confusionmatrix() function

model_nb <- naiveBayes(type ~ ., training_data)
model_nb  # will show all prior,conditional and posterior probabilities

#4-prediction and accuracy of model
pred_type <- predict(model_nb, testing_data)
pred_type  #here we directly getting response as "yes" or "no"

confusionMatrix(table(pred_type, testing_data$type)) # will show matrix, accuracy, kappa(shoud be high), p-value(shoud be <0.05) etc

#it is found that this model is not giving good result for this prediction
#may be eliminate some variables

#5-predicting for new dataset
new_data <- list(npreg=4, glu=110, bp=45, skin=47, bmi=51.3, ped=0.776, age=30)
predicted_type<- predict(model_nb, new_data)
predicted_type
