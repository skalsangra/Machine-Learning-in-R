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

training_rf <- subset(Pima.te, split== 'TRUE')
testing_rf <- subset(Pima.te, split== 'FALSE')

#3-Implementing dt model
library(randomForest)

model_rf<- randomForest(type ~ ., data = training_rf)
model_rf

#plotting the result as a tree
plot(model_rf, margin = 0.1)
text(model_rf, use.n = TRUE, pretty = TRUE, cex=0.8)

#to find priorities of variables based on GINI INDEX
#next two functions are under library 'randomForest' not applicable for decision tree model
importance(model_rf)
varImpPlot(model_rf)

#4-prediction and accuracy of model
pred <- predict(model_rf, testing_rf, type = 'class')
#here this model predict values of probabilities thats why to convert them
# into 'yes' or 'no' we must have to use type='class' 
#otherwise in confusionmatrix function error will occur...beware
pred


library(caret)
confusionMatrix(table(pred, testing_rf$type))

#5-predicting for new dataset
new_data <- list(npreg=4, glu=110, bp=45, skin=47, bmi=51.3, ped=0.776, age=30)
predicted_type<- predict(model_rf, new_data)
predicted_type
