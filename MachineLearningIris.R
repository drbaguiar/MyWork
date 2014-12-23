source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')
call("caret")
call("randomForest")
call("knitr")
call("AppliedPredictiveModeling")
call("rpart")
call("adabag")
call("corrplot")

##Read the data
train <- iris

##Clean the column names 
names(train) <- tolower(names(train))
names(train) <- gsub("_","",names(train))

##Split the iris dataset
inTrain <- createDataPartition(train$species, p = 0.60, list = FALSE)
training <- train[inTrain,]
testing.df <- train[-inTrain,]

##Split the iris dataset
inTrain <- createDataPartition(testing.df$species, p = 0.60, list = FALSE)
testing <- testing.df[inTrain,]
validate <- testing.df[-inTrain,]

##clean out unused datasets
rm(inTrain)
rm(train)
rm(testing.df)

##PCA with Caret
preProc <- preProcess(log10(training[,-5]+1),method="pca",pcaComp=2)
speciesPC <- predict(preProc,log10(training[,-5]+1))
plot(speciesPC[,1],speciesPC[,2],col=training$species)
modelFit <- train(training$species ~ .,method="knn",data=speciesPC)


##Some predictor variables may be highly correlated. 
##The find correlations function was used to identify correlations (positive or negative) greater than .90.  
outcome = which(names(training) == "species")
highcorrcols = findCorrelation(abs(cor(training[,-outcome])),0.90)
highcorrvars = names(training)[highcorrcols]
training = training[,-highcorrcols]
outcome = which(names(training) == "species")

##The variables with high correlation were 
highcorrvars[1:length(highcorrvars)-1]
highcorrvars[length(highcorrvars)]

##Pretty Correlation plot requires corrplot library
corrplot(cor(iris[,1:4]))
corrplot(cor(training[,1:3]))

#A random forest was used to discover the most important variables. 
#The random forest method reduces overfitting and is good for nonlinear variables.
fsRF = randomForest(training[,-outcome], training[,outcome], importance = T)
rfImp = data.frame(fsRF$importance)
impFeatures = order(-rfImp$MeanDecreaseGini)
inImp = createDataPartition(training$species, p = 0.05, list = F)
featurePlot(training[inImp,impFeatures[1:3]],training$species[inImp], plot = "pairs")
varImpPlot(fsRF, type=1)

##Set the training control
ctrl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

##Stochastic Gradient Boosting 
modelgbm <- train(species ~ ., training, method = "gbm", trControl = ctrl)
resultsgbm <- data.frame(modelgbm$results)

##ADA Boosting - requries rpart and adabag libraries
modelada <-boosting(species ~ ., training, mfinal=10)
modelada$weight #here we get weights for all trees.
modelada$importance #here we get the importance for all variables
modelada$trees #here we get the trees 

t1<-modelada$trees[[1]] #Here we only plot one tree
library(tree)
plot(t1)
text(t1,1.5)

##k-nearest neighbors
modelknn <- train(species ~ ., training, method = "knn", trControl = ctrl)
resultsknn <- data.frame(modelknn$results)

##Decision Tree
modeldt<- train(species~., training, method="ctree", trControl = ctrl)
resultsdt <- data.frame(modeldt$results)

##Random Forest
modelrf <- train(species ~ ., training, method = "rf", trControl = ctrl)
resultsrf <- data.frame(modelrf$results)

##Navie Bayes
modelnb <- train(species ~.,method="nb",data=training)
resultsnb <- data.frame(modelnb$results)

##Fit the models to the test dataset
fitknn <-predict(modelknn, testing)
fitrf <-predict(modelrf, testing)
fitdt <-predict(modeldt, testing)
fitada <-predict(modelada, testing)
fitnb <-predict(modelnb, testing)

##Plot fits
qplot(fitknn,fitrf,colour=species,data=testing)
qplot(fitknn,fitdt,colour=species,data=testing)
qplot(fitrf,fitdt,colour=species,data=testing)
qplot(fitrf,fitnb,colour=species,data=testing)

## Fit a model that combines predictors
predDF <- data.frame(fitrf,fitknn,species=testing$species)
combModFit <- train(species ~.,method="gam",data=predDF)
resultscomb <- data.frame(combModFit$results)
combPred <- predict(combModFit,predDF)

##Compare fits
confusionMatrix(fitrf, fitknn)
confusionMatrix(fitrf, fitdt)
confusionMatrix(fitknn, fitdt)
confusionMatrix(fitada$class, fitdt)
confusionMatrix(fitada$class, fitknn)
confusionMatrix(fitada$class, fitrf)


confusionMatrix(fitknn, testing$species)
confusionMatrix(fitrf, testing$species)
confusionMatrix(fitdt, testing$species)
confusionMatrix(fitada$class, testing$species)

##Accuracy Table
accuracy.table <- data.frame(Model=c("Random Forest", "Decision Tree", "KNN", "NB"),
                             Accuracy=c(round(max(head(resultsrf)$Accuracy), 2),
                                        round(max(head(resultsdt)$Accuracy), 2),
                                        round(max(head(resultsknn)$Accuracy), 2),
                                        round(max(head(resultsnb)$Accuracy), 2)))
kable(accuracy.table)

##Do the predictions on the validation data
pred.1 <- predict(modelknn, validate)
pred.2 <- predict(modeldt, validate)
pred.3 <- predict(modelrf, validate)
pred.4 <- predict(modelada,validate)
pred.5 <- predict(modelnb,validate)
rattle
##Combine 
#predVDF <- data.frame(pred1=pred.1,pred2=pred.3)
#pred.6 <- predict(combModFit,predVDF)

##Make a table and check if they all agree
pred.df <- data.frame(rf.pred = pred.3, dt.pred = pred.2, knn.pred = pred.1, ada.pred = pred.4$class, nb.pred=pred.5)
colnames(pred.df) <- c("RF", "DT", "KNN", "ADABoost", "NB")
kable(pred.df)