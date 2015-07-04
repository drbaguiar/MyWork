#Logistic Regression Problem
# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/letters_ABPR.csv")
df<-cleanit(df)
summary(df)

# count blanks remove blanks
barplot(colSums(!is.na(df)))
#df <- na.omit(df)
colSums(!is.na(df))

# Create factor is B
df$isB = as.factor(df$letter == "B")

# Percentage of voters 
x<-  table(df$isB)
addmargins(x)
prop.table(x)

# Split the data
library(caTools)
set.seed(1000)
split = sample.split(df$isB, SplitRatio = 0.5)
dfTrain = subset(df, split==TRUE)
dfTest = subset(df, split==FALSE)
rm(df,split)

# Percentage of voters 
x<-  table(dfTest$isB)
addmargins(x)
prop.table(x)

# Baseline on Training data 
# Determine the Majority
bl <-table(dfTrain$isB)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority
predictTrainBase <-rep(majority,nrow(dfTrain))
#Compare
cm <- table(dfTrain$isB,predictTrainBase, exclude=NULL)
addmargins(cm)
getstats(cm)

# Dep and Independent Vars define columns we will be working with
depvar <- 'isB'
indepvars <-c('.')
exclude <- c('letter') # numerical variables to exclude from using all
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
f2 <- paste(f1,paste(exclude,collapse=' - '),sep=' - ')

# Load CART packages
library(rpart)
library(rpart.plot)

#treeFit <-rpart(isB ~ . - letter, data=dfTrain, method="class")
treeFit <-rpart(f2, data=dfTrain, method="class")
prp(treeFit, digits = 6)

# Predict on Testing
predictTest = predict(treeFit, newdata = dfTest, type = "class")
cm <-table(dfTest$isB, predictTest)
#cm <- table(df$voting,predictTrain>thres)
addmargins(cm)
getstats(cm)

# Random Forest
library(randomForest)
set.seed(1000)
forestFit = randomForest(isB~. -letter, data = dfTrain)

# Make predictions
PredictForest = predict(forestFit, newdata = dfTest)
cm <- table(dfTest$isB, PredictForest)
addmargins(cm)
getstats(cm)

# Predict all the letters
# Load the data
df<-read.csv("D:/Data/letters_ABPR.csv")
df<-cleanit(df)

# Convert the letter column to a factor
df$letter = as.factor(df$letter)

# Split the data
library(caTools)
set.seed(2000)
split = sample.split(df$letter, SplitRatio = 0.5)
dfTrain = subset(df, split==TRUE)
dfTest = subset(df, split==FALSE)
rm(df,split)

# Percentage of voters 
x<-  table(dfTest$letter)
addmargins(x)
prop.table(x)

# Baseline on Testing data 
# Determine the Majority
bl <-table(dfTest$letter)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority
predictTestBase <-rep(majority,nrow(dfTest))
#Compare
cm <- table(dfTest$letter,predictTestBase, exclude=NULL)
addmargins(cm)
getstats(cm)

# Dep and Independent Vars define columns we will be working with
depvar <- 'letter'
indepvars <-c('.')
exclude <- c('') # numerical variables to exclude from using all
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
f2 <- paste(f1,paste(exclude,collapse=' - '),sep=' - ')

# Load CART packages
library(rpart)
library(rpart.plot)

#treeFit <-rpart(isB ~ . - letter, data=dfTrain, method="class")
treeFit <-rpart(f1, data=dfTrain, method="class")
prp(treeFit, digits = 6)

# Predict on Testing
predictTest = predict(treeFit, newdata = dfTest, type = "class")
cm <-table(dfTest$letter, predictTest)
#cm <- table(df$voting,predictTrain>thres)
addmargins(cm)
getstats(cm)

# Random Forest
library(randomForest)
set.seed(1000)
forestFit = randomForest(letter~., data = dfTrain)

# Make predictions
PredictForest = predict(forestFit, newdata = dfTest)
cm <- table(dfTest$letter, PredictForest)
addmargins(cm)
getstats(cm)
