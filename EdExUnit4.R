#Trees
# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/stevens.csv")
df<-cleanit(df)
summary(df)

# count blanks remove blanks
colSums(!is.na(df))
#df <- na.omit(df)
#colSums(!is.na(df))

# Split the data
library(caTools)
set.seed(3000)
split = sample.split(df$reverse, SplitRatio = 0.7)
dfTrain = subset(df, split==TRUE)
dfTest = subset(df, split==FALSE)
rm(df,split)

# Dep and Independent Vars define columns we will be working with
depvar <- 'reverse'
indepvars <-c('circuit', 'issue','petitioner',' respondent','lowercourt','unconst')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

# Classification tree
# Rpart
library(rpart)
library(rpart.plot)
treeTrain <- rpart(f1,data=dfTrain,method="class", minbucket=25)
prp(treeTrain)

# Make predictions
treePredict <- predict(treeTrain, newdata = dfTest, type = "class")
cm <-table(dfTest$reverse, treePredict)
thres=0.5
addmargins(cm)
getstats(cm)

# Pull out mistakes
#subset(dfTest, treePredict >= thres & reverse == 0)
#subset(dfTest, treePredict <= thres & get(depvar) == 1)

# Build Receiver Operator Charastics ROC
library(ROCR)

# Prediction function
predictROC <- predict(treeTrain, newdata = dfTest)
ROCRpredTest = prediction(predictROC[,2],dfTest$reverse)

# Performance function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))

# Random Forfest
library(randomForest)
set.seed(200)

# Convert outcome to factor
dfTrain$reverse = as.factor(dfTrain$reverse)
dfTest$reverse = as.factor(dfTest$reverse)

#Train Model
forestTrain = randomForest(reverse ~ circuit + issue + petitioner + respondent + lowercourt + unconst, data = dfTrain, ntree=200, nodesize=25 )

# Make predictions
forestPredict = predict(forestTrain, newdata = dfTest)
cm <-table(dfTest$reverse, forestPredict)
thres=0.5
addmargins(cm)
getstats(cm)

# Pull out mistakes
#subset(dfTest, treePredict >= thres & reverse == 0)
#subset(dfTest, treePredict <= thres & get(depvar) == 1)

#Determine cp value
library(caret)
library(e1071)
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(reverse ~ circuit + issue + petitioner + respondent + lowercourt + unconst, data=dfTrain,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

# Train using CV
treeTrainCV <- rpart(f1,data=dfTrain,method="class", cp=0.2)
prp(treeTrainCV)

# Predict using CV
treePredictCV <- predict(treeTrainCV, newdata = dfTest, type = "class")
cm <-table(dfTest$reverse, treePredictCV)
thres=0.5
addmargins(cm)
getstats(cm)
