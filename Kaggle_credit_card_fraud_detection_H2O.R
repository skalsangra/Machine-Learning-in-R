#1-Data Acquisition
setwd("D:/R Docs/ML code/Kaggle")

library(data.table)

#loading creditcard dataset using fread
credit <- fread(file = "Credit Card Fraud Detection/creditcard.csv", stringsAsFactors = FALSE)

#check that no datapoint is missing, otherwise we need to fix the dataset
##apply(credit,2,function(x) sum(is.na(x)))
colSums(is.na(credit))  #very fast than apply function whooouuu:)

#2-Divide Dataset
#we will divide dataset into training data and testing data
set.seed(2)
library(caTools) #sample.split function is present in this package

split <- sample.split(credit$Class, SplitRatio = 0.7) 
#split

training_credit <- subset(credit, split== 'TRUE')
testing_credit <- subset(credit, split== 'FALSE')

# Load the H2O R package and start an local H2O cluster
library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()

#put data to h2o cluster
train.h2o <- as.h2o(training_credit)
test.h2o <- as.h2o(testing_credit)

#dependent variable (Class)
y.dep <- 31

#independent variables (dropping ID variables)
x.indep <- c(1:30)

#4-Implement Logistic model 
regression.model <- h2o.glm( y = y.dep, x = x.indep, training_frame = train.h2o, family = "binomial")

h2o.performance(regression.model)

#6-Model validation
#predicting values of 'Class' for testing dataset
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))


#"prediction" returns Probabilities , so we convert them into "0" and "1" by using if-else
predicted_test=ifelse(predict.reg$p1 > 0.5, 1, 0)

#now lets compare our solution with Kaggle's solution
library(caret)
confusionMatrix(table(Prediction=predicted_test, Actual = testing_credit$Class))



