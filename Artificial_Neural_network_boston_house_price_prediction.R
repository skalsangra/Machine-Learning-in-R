###########################################################################################
#We are going to use the Boston dataset in the MASS package.
#The Boston dataset is a collection of data about housing values in the suburbs of Boston. 
#Our goal is to predict the median value of owner-occupied homes (medv) 
#using all the other continuous variables available.
###########################################################################################
set.seed(500)
library(MASS)
data <- Boston

#check that no datapoint is missing, otherwise we need to fix the dataset
apply(data,2,function(x) sum(is.na(x)))
#no data is missing here

#Normalizing data by use of "min-max method"
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

#Splitting data into Training and Testing set
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train_ <- scaled[index,]
test_ <- scaled[-index,]

#fitting Neural Network model
library(neuralnet)
#n <- names(train_)
#f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
#For some reason the formula y~. is not accepted in the neuralnet() function. 
#You need to first write the formula and then pass it as an argument in the fitting function.
#The hidden argument accepts a vector with the number of neurons for each hidden layer, 
#for regression -> linear.output=TRUE(or T) and for classification -> linear.output=FALSE
nn_model <- neuralnet(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, data=train_,hidden=c(5,3), linear.output=T)

#plotting the model
plot(nn_model)

#prdiction of testing dataset(exclude last target variable "medv" first)
pred_medv <- compute(nn_model, test_[,1:13])

#pred_medv contains result for each neuron(pred_medv$neurons) and output(pred_medv$net.result)
#comparing prdicted values with actual values by plotting
plot(test_$medv, type = 'l', lty=1.8, col='green')
lines(pred_medv$net.result, type = 'l', col='red')

#since this model is built on Normalized values(both input and output)
#so to predict for new dataset first normalized that then to see actual output
#revert back normalized output into original range
#7-Prediction of new sample_data so first create list of sample data
sample_data <- data.frame(crim=0.23899, zn=13.7, indus=11.4, chas=0, nox=0.479, rm=6.9989, age=83.7, dis=5.976, rad=17,tax=335, ptratio=16.9, black=390.6, lstat=6.1)
typeof(sample_data)

normlize <- function(x){(x - min(x))/(max(x)-min(x))}  #defined a normalization function
#how can we normalize only one value of each variable....got it
sample_data_norm <- as.data.frame(lapply(sample_data, normlize))
predic<- compute(nn_model, sample_data_norm)
predic
