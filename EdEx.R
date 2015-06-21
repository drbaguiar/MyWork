# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/quality.csv")
df<-cleanit(df)

# count blanks remove blanks
colSums(!is.na(df))
#df <- na.omit(df)
#colSums(!is.na(df))

# Load a library and split the data
library(caTools)
set.seed(88)
split = sample.split(df$poorcare, SplitRatio = 0.75)
dfTrain = subset(df, split == TRUE)
dfTest = subset(df, split == FALSE)
rm(df)

# Locate record with highest ili
dfTrain[which(dfTrain$ili==max(dfTrain$ili)),]

# Histogram of ili
hist(dfTrain$ili)

# Because of the skew, log the dep variabe
dfTrain$logili<-log(dfTrain$ili)

# Histogram and scatter plot of log(ili)
hist(log(dfTrain$logili))
plot(dfTrain$logili,dfTrain$queries)

# Dep and Independent Vars define columns we will be working with
depvar <- 'logili'
indepvars <-c('queries')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

#Fit the model
# fit<-glm(f1,data=dfTrain,family=binomial)
fit<-lm(f1,data=dfTrain)
summary(fit)

# Make predictions (probabilities) on training set
predictTest = exp(predict(fit,newdata=dfTest))

# Analyze predictions
summary(predictTest)

# locate record for a specfic week
which(dfTest$week=="2012-03-11 - 2012-03-17")

# Determine predictions for that week
predictTest[11]

# Calculate relative error
(dfTest[11,2] - predictTest[11])/dfTest[11,2]

# Compute out-of-sample R^2
SSE = sum((predictTest - dfTest$ili)^2)
SST = sum((mean(dfTrain$ili) - dfTest$ili)^2)
R2 = 1 - SSE/SST
R2

# Compute the RMSE
RMSE = sqrt(SSE/nrow(dfTest))
RMSE

# Time series
library(zoo)

#Create a lag 
dfTrain$ililag2 = lag(zoo(dfTrain$ili), -2, na.pad=TRUE)
dfTrain$ililag2 = coredata(dfTrain$ililag2)
dfTest$ililag2 = lag(zoo(dfTest$ili), -2, na.pad=TRUE)
dfTrain$ililag2 = coredata(dfTrain$ililag2)

# Fill in 2 missing values in Test 
dfTest$ililag2[1] = dfTrain$ili[416]
dfTest$ililag2[2] = dfTrain$ili[417]

# Plot lag with log(ili)
plot(log(dfTrain$ililag2), log(dfTrain$ili))

# Histogram and scatter plot of log(ili)
hist(log(dfTrain$logili))
plot(dfTrain$logili,dfTrain$queries)

# Dep and Independent Vars define columns we will be working with
depvar <- 'logili'
indepvars <-c('queries','ililag2')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

#Fit the model
# fit<-glm(f1,data=dfTrain,family=binomial)
fit<-lm(f1,data=dfTrain)
summary(fit)

# Make predictions (probabilities) on training set
predictTest = exp(predict(fit,newdata=dfTest))

# Analyze predictions
summary(predictTest)

# Compute out-of-sample R^2
SSE = sum((predictTest - dfTest$ili)^2)
SST = sum((mean(dfTrain$ili) - dfTest$ili)^2)
R2 = 1 - SSE/SST
R2

# Compute the RMSE
RMSE = sqrt(SSE/nrow(dfTest))
RMSE

