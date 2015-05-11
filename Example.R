rm(list=ls())
library(caret)
library(RWeka)
set.seed(1234)

# separate data into test and train sets, 70/30 split in this case
splitIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train <- iris[splitIndex, ]
test <- iris[-splitIndex, ]
testInd <- test[ ,!colnames(test) %in% "Species"]
testDep <- as.factor(test[, names(test) == "Species"]) 

TrainData <- iris[,1:4]
TrainClasses <- iris[,5]


#First Model
jripFit1 <- train(TrainData, TrainClasses,method = "JRip")
jripFit1
plot(jripFit1)

#Second Model
jripFit2 <- train(TrainData, TrainClasses,method = "JRip",preProcess = c("center", "scale"),tuneLength = 10,trControl = trainControl(method = "cv"))
jripFit2
plot(jripFit2)

# K means

neighborCount=2

modelKNN <- knn3(Species ~ ., data = train, k = neighborCount, prob = TRUE)

predKNN <- predict(modelKNN, testInd, type = "prob")

confKNN <- confusionMatrix(testDep, predKNN)
