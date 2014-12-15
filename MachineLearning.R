source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')
call("caret")
call("randomForest")
call("knitr")
call("AppliedPredictiveModeling")

#Set name of training and validating datafiles
train.data=paste(datadir,"pml-training.csv",sep = "")
validate.data=paste(datadir,"pml-testing.csv",sep = "")
train.url="http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
validate.url="http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

##Check for training file, download if not there
if (!file.exists(train.data)) {
        download.file(train.url, destfile = train.data)
}

##Check for testing file, download if not there
if (!file.exists(validate.data)) {
        download.file(validate.url, destfile = validate.data)
}

##Read the data
validate <- read.csv(validate.data, sep = ",", na.strings = c("", "NA"))
train <- read.csv(train.data, sep = ",", na.strings = c("", "NA"))

##Clean out variables not used
rm(validate.data)
rm(train.data)
rm(validate.url)
rm(train.url)

##Clean the column names 
names(validate) <- tolower(names(validate))
names(validate) <- gsub("_","",names(validate))
names(train) <- tolower(names(train))
names(train) <- gsub("_","",names(train))

##Remove columns full of NAs. Only use variables in test dataset.
vars <- names(validate[,colSums(is.na(validate)) == 0])[8:59]
train.df <- train[,c(vars,"classe")]
validate.df <- validate[,c(vars,"problemid")]

##clean out unused datasets
rm(validate)
rm(train)

##Split the training dataset
inTrain <- createDataPartition(train.df$classe, p = 0.80, list = FALSE)
training <- train.df[inTrain,]
testing <- train.df[-inTrain,]
validate <- validate.df

##clean out unused datasets
rm(inTrain)
rm(train.df)
rm(validate.df)

##Some predictor variables may be highly correlated. 
##The find correlations function was used to identify correlations (positive or negative) greater than .90.  
outcome = which(names(training) == "classe")
highcorrcols = findCorrelation(abs(cor(training[,-outcome])),0.90)
highcorrvars = names(training)[highcorrcols]
training = training[,-highcorrcols]
outcome = which(names(training) == "classe")

##The variables with high correlation were 
highcorrvars[1:length(highcorrvars)-1]
highcorrvars[length(highcorrvars)]

#A random forest was used to discover the most important variables. 
#The random forest method reduces overfitting and is good for nonlinear variables.
fsRF = randomForest(training[,-outcome], training[,outcome], importance = T)
rfImp = data.frame(fsRF$importance)
impFeatures = order(-rfImp$MeanDecreaseGini)
inImp = createDataPartition(training$classe, p = 0.05, list = F)
featurePlot(training[inImp,impFeatures[1:4]],training$classe[inImp], plot = "pairs")
varImpPlot(fsRF, type=1)

##Set the training control
ctrl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

##k-nearest neighbors
modelknn <- train(classe ~ ., training, method = "knn", trControl = ctrl)
resultsknn <- data.frame(modelknn$results)

##Decision Tree
modeldt<- train(classe~., training, method="ctree", trControl = ctrl)
resultsdt <- data.frame(modeldt$results)

##Random Forest
modelrf <- train(classe ~ ., training, method = "rf", ntree = 200, trControl = ctrl)
resultsrf <- data.frame(modelrf$results)

##Fit the models to the test dataset
fitknn <-predict(modelknn, testing)
fitrf <-predict(modelrf, testing)
fitdt <-predict(modeldt, testing)

##Compare fits
confusionMatrix(fitrf, fitknn)
confusionMatrix(fitrf, fitdt)
confusionMatrix(fitknn, fitdt)
confusionMatrix(fitknn, testing$classe)
confusionMatrix(fitrf, testing$classe)
confusionMatrix(fitdt, testing$classe)

##Accuracy Table
accuracy.table <- data.frame(Model=c("Random Forest", "Decision Tree", "KNN"),
                             Accuracy=c(round(max(head(resultsrf)$Accuracy), 2),
                                        round(max(head(resultsdt)$Accuracy), 2),
                                        round(max(head(resultsknn)$Accuracy), 2)))
kable(accuracy.table)

##Do the predictions
pred.1 <- predict(modelknn, validate)
pred.2 <- predict(modeldt, validate)
pred.3 <- predict(modelrf, validate)


##Make a table and check if they all agree
pred.df <- data.frame(rf.pred = pred.3, dt.pred = pred.2, knn.pred = pred.1)
colnames(pred.df) <- c("RF", "DT", "KNN")
kable(pred.df)