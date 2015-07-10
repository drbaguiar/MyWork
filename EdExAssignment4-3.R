#Logistic Regression Problem
# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/census.csv")
df<-cleanit(df)
summary(df)

# count blanks remove blanks
barplot(colSums(!is.na(df)))
#df <- na.omit(df)
colSums(!is.na(df))

# Percentage 
x<-  table(df$over50k)
addmargins(x)
prop.table(x)

# Split the data
library(caTools)
set.seed(2000)
split = sample.split(df$over50k, SplitRatio = 0.6)
dfTrain = subset(df, split==TRUE)
dfTest = subset(df, split==FALSE)
rm(df,split)

# Dep and Independent Vars define columns we will be working with
depvar <- 'over50k'
indepvars <-c('.')
exclude <- c() # numerical variables to exclude from using all
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
f2 <- paste(f1,paste(exclude,collapse=' - '),sep=' - ')

# Baseline on Testing data 
# Determine the Majority
bl <-table(dfTest$over50k)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority
predictTestBase <-rep(majority,nrow(dfTest))
#Compare
cm <- table(dfTest$over50k,predictTestBase, exclude=NULL)
addmargins(cm)
=97getstats(cm)


# Build Receiver Operator Charastics ROC
library(ROCR)

# Prediction function
ROCRpredTest = prediction(predictTestBase,dfTest$over50k)

# Performance function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))

# Fit the model
fit<-glm(f1,data=dfTrain,family=binomial)
summary(fit)

# Predict on Testing
thres = .5
predictTest <- predict(fit, type="response", newdata=dfTest)
cm <- table(dfTest$over50k,predictTest>thres)
addmargins(cm)
getstats(cm)

# Pull out mistakes
subset(dfTest, predictTest >= thres & over50k == 0)
subset(dfTest, predictTest <= thres & over50k == 1)

# Load CART packages
library(rpart)
library(rpart.plot)

# Build a regression tree
CARTFit = rpart(f1, data=dfTrain,method="class")
prp(CARTFit)

# Make predictions
treePredict <- predict(CARTFit, newdata = dfTest, type = "class")
cm <-table(dfTest$over50k, treePredict)
thres=0.5
addmargins(cm)
getstats(cm)

# Build Receiver Operator Charastics ROC
library(ROCR)

# Prediction function
predictROC <- predict(CARTFit, newdata = dfTest)
ROCRpredTest = prediction(predictROC[,2],dfTest$over50k)

# Performance function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))

#Smaller training set
set.seed(1)
trainSmall = dfTrain[sample(nrow(dfTrain), 2000), ]

# Random Forest
library(randomForest)
set.seed(1)
forestFit = randomForest(over50k~., data = trainSmall)

# Make predictions
PredictForest = predict(forestFit, newdata = dfTest)
cm <- table(dfTest$over50k, PredictForest)
addmargins(cm)
getstats(cm)

# One metric that we can look at is the number of times, aggregated over all of the trees in the random forest 
# model, that a certain variable is selected for a split. To view this metric, run the following lines of R code 
# (replace "MODEL" with the name of your random forest model):
# This code produces a chart that for each variable measures the number of times that variable was selected for splitting (the value on the x-axis). 
vu = varUsed(forestFit, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forestFit$forest$xlevels[vusorted$ix]))

# A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf 
# of the tree is. In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. 
# Therefore, one way to measure the importance of a variable is to average the reduction in impurity, taken over all the times that 
# variable is selected for splitting in all of the trees in the forest. To compute this metric, run the following command in R 
# (replace "MODEL" with the name of your random forest model):
varImpPlot(forestFit)

#Determine cp value
library(caret)
library(e1071)
numFolds <- trainControl(method="cv", number=10)
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data=dfTrain,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

#Based on above, use cp = 0.002
# Train using CV
treeTrainCV <- rpart(f1,data=dfTrain,method="class", cp=0.002)
prp(treeTrainCV)

# Make predictions
PredictForestCV = predict(treeTrainCV, newdata = dfTest, type="class")
cm <- table(dfTest$over50k, PredictForestCV)
addmargins(cm)
getstats(cm)
