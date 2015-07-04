#Logistic Regression Problem
# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/ClaimsData.csv")
df<-cleanit(df)
summary(df)

# Percentage of patients in each cost bucket
x<-  table(df$bucket2009)
addmargins(x)
prop.table(x)

# count blanks remove blanks
barplot(colSums(!is.na(df)))
#df <- na.omit(df)
colSums(!is.na(df))
                                                                                                                                                                                                                                                                                                                                                    Impute Values
# Split the data
library(caTools)
set.seed(88)
split = sample.split(df$bucket2009, SplitRatio = 0.6)
dfTrain = subset(df, split==TRUE)
dfTest = subset(df, split==FALSE)
rm(df,split)

# Baseline on Testing data 
# Determine the Majority
bl <-table(dfTrain$bucket2009)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority
predictTrainBase <-rep(majority,nrow(dfTrain))
#Compare
cm <- table(dfTrain$bucket2009,predictTrainBase, exclude=NULL)

# Advanced model
#cm<-table(dfTest$bucket2009,dfTest$bucket2008)

#Add margins and compute stats
addmargins(cm)
getstats(cm)

# Make penalty matrix
#pm = matrix(c(6,4,2,0,1,8,6,4,2,0,0,0), byrow=TRUE, nrow=6)
pm <- matrix(c(0,1,2,2,4,2,6,4,8,6,0,0), byrow=TRUE, nrow=6)
# Multiple cm * pm
sum(as.matrix(cm)*pm/nrow(dfTest))


# Dep and Independent Vars define columns we will be working with
depvar <- 'bucket2009'
indepvars <-c('.')
exclude <- c('reimbursement2009') # numerical variables to exclude from using all
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
f2 <- paste(f1,paste(exclude,collapse=' - '),sep=' - ')

# Load necessary libraries
library(rpart)
library(rpart.plot)

# CART model
trainTree = rpart(f2, data=dfTrain, method="class", cp=0.00005)
prp(trainTree)

# Make predictions
predictTest = predict(trainTree, newdata = dfTest, type = "class")

cm=table(dfTest$bucket2009, predictTest)
#Add margins and compute stats
addmargins(cm)
getstats(cm)

# Penalty Matrix
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)

# Penalty Error
as.matrix(table(dfTest$bucket2009, predictTest))*PenaltyMatrix
sum(as.matrix(table(dfTest$bucket2009, predictTest))*PenaltyMatrix)/nrow(dfTest)

# revised CART model with penalty matric
trainTree = rpart(f2, data=dfTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))
prp(trainTree)

# Make predictions
predictTest = predict(trainTree, newdata = dfTest, type = "class")

cm=table(dfTest$bucket2009, predictTest)
#Add margins and compute stats
addmargins(cm)
getstats(cm)

# Penalty Error
as.matrix(table(dfTest$bucket2009, predictTest))*PenaltyMatrix
sum(as.matrix(table(dfTest$bucket2009, predictTest))*PenaltyMatrix)/nrow(dfTest)

