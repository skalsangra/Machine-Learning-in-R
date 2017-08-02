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

training_dt <- subset(Pima.te, split== 'TRUE')
testing_dt <- subset(Pima.te, split== 'FALSE')

#3-Implementing dt model
library(rpart)

model_dt<- rpart(type ~ ., data = training_dt)
summary(model_dt)
model_dt$method
model_dt$parms

#plotting the result as a tree
plot(model_dt, margin = 0.1)
text(model_dt, use.n = TRUE, pretty = TRUE, cex=0.8)



#4-prediction and accuracy of model
pred_dt <- predict(model_dt, testing_dt, type = 'class')
#here this model predict values of probabilities thats why to convert them
# into 'yes' or 'no' we must have to use type='class' 
#otherwise in confusionmatrix function error will occur...beware
pred_dt


library(caret)
confusionMatrix(table(pred_dt, testing_dt$type))

#5-predicting for new dataset
new_data <- list(npreg=4, glu=110, bp=45, skin=47, bmi=51.3, ped=0.776, age=30)
predicted_type<- predict(model_dt, new_data, type = 'class')
predicted_type
