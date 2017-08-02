#1-Data Acquisition
#Load Boston housing data
library(MASS)
data(Boston)
 

#2-Divide Dataset
#we will divide dataset into training data and testing data
set.seed(2)
library(caTools) #sample.split function is present in this package

split <- sample.split(Boston$medv, SplitRatio = 0.7) 
#split

training_data <- subset(Boston, split== 'TRUE')
testing_data <- subset(Boston, split== 'FALSE')

#3-Exploratary Analysis
#Relations among all the variables through scatterplot matrix

library(lattice)
splom(~Boston[c(1:6, 14)],groups = NULL, data = Boston, axis.line.tck=0, axis.text.alpha=0)

#correlation between all variables
cr <- cor(Boston)  #this will give correlation matrix

#to visualize correlation matrix 'cr' use corrplot() function
library(corrplot)
corrplot(cr, type="lower")

model <- lm(medv~., data=training_data)

summary(model)
#4-Implement model
model <- lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+ptratio+black+lstat, data=training_data)

summary(model)

#5-Optimize Model
#removing 'indus' and 'age' having higher p-value(>0.05) that are less significant to the model
model <- lm(medv~crim+zn+chas+nox+rm+dis+rad+ptratio+black+lstat, data=training_data)

summary(model)

######################################################
#checking multicollinearity among indepedent variables by finding variance inflation factor(VIF)
library(car)
vif(model)

#if vif>5 for any variable remove it from model
#########################################################
#6-Model validation
#predicting values of 'medv' for testing dataset
predic<- predict(model, testing_data)
#predic

#comparing prdicted values with actual values by plotting
plot(testing_data$medv, type = 'l', lty=1.8, col='green')
lines(predic, type = 'l', col='red')

#typeof(training_data)
#7-Prediction of new sample_data so first create list of sample data
sample_data <- list(crim=0.23899, zn=13.7, chas=0, nox=0.479, rm=6.9989, dis=5.976, rad=17, ptratio=16.9, black=390.6, lstat=6.1)
predic<- predict(model, sample_data)
predic