library(MASS)
library(neuralnet)

####Setting the seed so the we get same results each time
##we run neural nets
set.seed(123)

###Storing the data set named "Boston" into DataFrame
DataFrame <- Boston

###Help on Boston data 
help("Boston")

###Structure of Boston data
str(DataFrame)

###Histogram of the medv
hist(DataFrame$medv)

####Check the dimention of this data frame
dim(DataFrame)


###Check first 3 rows
head(DataFrame,3)

###Check the summary of each variables 
##This will give min and max value for each of the variable
apply(DataFrame,2,range)

###Seems like scale of each variable is not same

### Normalizing the data in interval [0,1]
### Normalization  is necessay so that each variable is scale properly
### and none of the variables overdominates 
###scale function will give mean =0 and standard deviation=1 for each variable


maxValue <- apply(DataFrame, 2, max) 
minValue <- apply(DataFrame, 2, min)

DataFrame<-as.data.frame(scale(DataFrame,center = minValue,scale = maxValue-minValue))


###Lets create the train and test data set

ind<-sample(1:nrow(DataFrame),400)
trainDF<-DataFrame[ind,]
testDF<-DataFrame[-ind,]


###Lets take some configuration for neural network
###say 13-4-2-1
###So number of hidden layes=2
###input layer had 10 units

##We need this as formula
##medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + 
##  tax + ptratio + black + lstat


allVars<-colnames(DataFrame)
predictorVars<-allVars[!allVars%in%"medv"]
predictorVars<-paste(predictorVars,collapse = "+")
form=as.formula(paste("medv~",predictorVars,collapse = "+"))

neuralModel<-neuralnet(formula =form,hidden = c(4,2),linear.output = T,
                       data =trainDF)


###Plot the neural net

plot(neuralModel)


###Predict for test data set 
predictions <- compute(neuralModel,testDF[,1:13])
str(predictions)

predictions <- predictions$net.result*(max(testDF$medv)-min(testDF$medv))+min(testDF$medv)
actualValues <- (testDF$medv)*(max(testDF$medv)-min(testDF$medv))+min(testDF$medv)


MSE <- sum((predictions - actualValues)^2)/nrow(testDF)
MSE

plot(testDF$medv,predictions,col='blue',main='Real vs Predicted',pch=1,cex=0.9,type = "p",xlab = "Actual",ylab = "Predicted")



###A line with 45 degree slope is showing that predictions and actual values 
###of unseen data are almost the same


###For more better training of model for any dirty data 
###preprocessing and cleaning of data is must
###also crossvalidation is must 

