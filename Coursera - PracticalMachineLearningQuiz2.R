rm(list = ls())

#1 Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

##Which of the following commands will create training and test sets with about 50% of the observations assigned to each?
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#2 Load the cement data using the commands:
rm(list = ls())
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

##Make a plot of the outcome (CompressiveStrength) versus the index of the samples. Color by each of the variables in the data set (you may find the cut2() function in the Hmisc package useful for turning continuous covariates into factors). 
##What do you notice in these plots?

library(Hmisc)
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(Cement,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(BlastFurnaceSlag,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(FlyAsh,g=3))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(Water,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(Superplasticizer,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(CoarseAggregate,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(FineAggregate,g=4))
qplot(x = 1:length(inTrain), y = CompressiveStrength, data = training, color = cut2(Age,g=4))

#3 Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log transform to try to make the data more symmetric. 
##Why would that be a poor choice for this variable?

qplot(Superplasticizer, data = training)
qplot(log(Superplasticizer+1), data = training)

#4 Load the Alzheimer's disease data using the commands:
rm(list = ls())
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Find all the predictor variables in the training set that begin with IL. 
#Perform principal components on these variables with the preProcess() function from the caret package. 
#Calculate the number of principal components needed to capture 80% of the variance. 
#How many are there?
subset = training[,grep("^IL", names(training))]
preProcess(subset, thresh = 0.8, method = "pca")$numComp

#5  Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. 
#Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% of the variance in the predictors. 
#Use method="glm" in the train function. What is the accuracy of each method in the test set? Which is more accurate?
trainSubset = training[,grep("^IL", names(training))]
testSubset = testing[,grep("^IL", names(testing))]
pp = preProcess(trainSubset, thresh = 0.8, method = "pca")
trainTransformed <- predict(pp, trainSubset)
testTransformed <- predict(pp, testSubset)
trainSubset$diagnosis = training$diagnosis
testSubset$diagnosis = testing$diagnosis
trainTransformed$diagnosis = training$diagnosis
testTransformed$diagnosis = testing$diagnosis
glmpca = train(diagnosis ~ ., data = trainTransformed, method = "glm")
glm = train(diagnosis ~ ., data = trainSubset, method = "glm")
round(confusionMatrix(testSubset$diagnosis,predict(glm, testSubset))$overall["Accuracy"],2)
round(confusionMatrix(testTransformed$diagnosis,predict(glmpca, testTransformed))$overall["Accuracy"],2)
