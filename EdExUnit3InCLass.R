# Logistic Regression Problem
# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/quality.csv")
df<-cleanit(df)

# count blanks remove blanks
colSums(!is.na(df))
df <- na.omit(df)
colSums(!is.na(df))

# Load a library and split the data
library(caTools)
set.seed(88)
split = sample.split(df$poorcare, SplitRatio = 0.75)
dfTrain = subset(df, split == TRUE)
dfTest = subset(df, split == FALSE)
rm(df, split)

# Table of outcome
table(dfTrain$poorcare)

# Plot 2 independent variables by outcome color (green when y = 0)
plot(dfTrain$officevisits,dfTrain$narcotics,pch=15, col = rgb(0,1,dfTrain$poorcare))

# Dep and Independent Vars define columns we will be working with
depvar <- 'poorcare'
indepvars <-c('officevisits', 'narcotics')
#indepvars <-c('startedoncombination', 'providercount')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

#Fit the model
# fit<-glm(f1,data=dfTrain,family=binomial)
fit<-glm(f1,data=dfTrain,family=binomial)
#summary(fit)
reviewit(fit)

# Make predictions on training set
predictTrain = predict(fit, type="response")

# Analyze predictions
summary(predictTrain)
tapply(predictTrain, dfTrain$poorcare, mean)

# Confusion matrix for threshold
thres<-0.5
cm<-table(dfTrain$poorcare, predictTrain > thres)
addmargins(cm)

#Sensititvity a.k.a TPR
tpr <-cm[2,2]/(cm[2,2]+cm[2,1])

# Specificity a.k.a. TNR
tnr <- cm[1,1]/(cm[1,1]+cm[1,2])

# Calculate accuracy
acc = (cm[2,2]+cm[1,1])/sum(cm)

rbind(Sensitivity=tpr, Specificity=tnr, Accuracy = acc)

# Build Receiver Operator Charastics ROC
library(ROCR)
# Prediction function
ROCRpredTrain = prediction(predictTrain, dfTrain$poorcare)

# Performance function
ROCRperfTrain = performance(ROCRpredTrain, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTrain, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))

# The AUC of a model has the following nice interpretation: given a random patient from the dataset who actually received 
# poor care, and a random patient from the dataset who actually received good care, the AUC is the perecentage of time that 
# our model will classify which is which correctly.

# Compute test
# Make predictions on testing set
predictTest= predict(fit, type="response", newdata=dfTest)

# Analyze predictions
summary(predictTest)
tapply(predictTest, dfTest$poorcare, mean)

# Confusion matrix for threshold
cm<-table(dfTest$poorcare, predictTest > thres)
addmargins(cm)

#Sensititvity a.k.a TPR
tpr <-cm[2,2]/(cm[2,2]+cm[2,1])

# Specificity a.k.a. TNR
tnr <- cm[1,1]/(cm[1,1]+cm[1,2])

# Calculate accuracy
acc = (cm[2,2]+cm[1,1])/sum(cm)

rbind(Sensitivity=tpr, Specificity=tnr, Accuracy = acc)

# Prediction function
ROCRpredTest = prediction(predictTest,dfTest$poorcare)


# Performance function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))

# Compute out-of-sample R^2
SSE = sum((predictTest - dfTest$poorcare)^2)
SST = sum((mean(dfTrain$poorcare) - dfTest$poorcare)^2)
R2 = 1 - SSE/SST
R2

# Compute the RMSE
RMSE = sqrt(SSE/nrow(dfTest))
RMSE

# Make a prediction for 
test2<-data.frame(narcotics=2,officevisits=14)
fitpred<-predict(fit,test2,se.fit=TRUE)
pi <- cbind(Prob=fitpred$fit,LCL=fitpred$fit - fitpred$se.fit*1.96,UCL=fitpred$fit + fitpred$se.fit*1.96)
pi2 <- cbind(Prob=exp(pi[,1])/(1+exp(pi[,1])),LCL=exp(pi[,2])/(1+exp(pi[,2])),UCL=exp(pi[,3])/(1+exp(pi[,3])))
pi
pi2


## Next problem
# Load the data
df<-read.csv("D:/Data/framingham.csv")
df<-cleanit(df)

# count blanks remove blanks
colSums(!is.na(df))
#df <- na.omit(df)
colSums(!is.na(df))

# Load a library and split the data
library(caTools)
set.seed(1000)
split = sample.split(df$tenyearchd, SplitRatio = 0.65)
dfTrain = subset(df, split == TRUE)
dfTest = subset(df, split == FALSE)
rm(df, split)

# Dep and Independent Vars define columns we will be working with
depvar <- 'tenyearchd'
indepvars <-c('.')
#indepvars <-c('startedoncombination', 'providercount')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

#Fit the model
# fit<-glm(f1,data=dfTrain,family=binomial)
fit<-glm(f1,data=dfTrain,family=binomial)
#summary(fit)
reviewit(fit)

# Compute test
# Make predictions on testing set
predictTest= predict(fit, type="response", newdata=dfTest)

# Analyze predictions
summary(predictTest)
tapply(predictTest, dfTest$tenyearchd, mean)

# Confusion matrix for threshold
thres <-.5
cm<-table(dfTest$tenyearchd, predictTest > thres)
addmargins(cm)

# Establish baseline
bl<-table(dfTest$tenyearchd)
addmargins(bl)

#Sensititvity a.k.a TPR
tpr <-cm[2,2]/(cm[2,2]+cm[2,1])
fpr <-cm[1,2]/(cm[1,2]+cm[1,1])

# Specificity a.k.a. TNR
tnr <- cm[1,1]/(cm[1,1]+cm[1,2])
fnr <- cm[2,1]/(cm[2,1]+cm[2,2])

# Calculate accuracy
acc <-(cm[2,2]+cm[1,1])/sum(cm)
err <-(cm[1,2]+cm[2,1])/sum(cm)
  
#Precision - Positive Predictive Value
ppv <- cm[2,2]/(cm[2,2]+cm[1,2])

#Negative Predictive Value
npv <- cm[1,1]/(cm[1,1]+cm[2,1])

rbind(TruePos=tpr, FalsePos=fpr, TrueNeg=tnr, FalseNeg=fnr, PositivePredictiveValue=ppv, NegativePredictiveValue=npv, Accuracy = acc, Error = err)

# Prediction function
ROCRpredTest = prediction(predictTest,dfTest$tenyearchd)

# Performance function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))

# Compute out-of-sample R^2
SSE = sum((predictTest - dfTest$tenyearchd)^2)
SST = sum((mean(dfTrain$tenyearchd) - dfTest$tenyearchd)^2)
R2 = 1 - SSE/SST
R2

# Compute the RMSE
RMSE = sqrt(SSE/nrow(dfTest))
RMSE

##prediction <- ifelse(predict(model, data, type='response') > 0.5, TRUE, FALSE)
# predictTrain<- predict(fit, type="response", newdata=dfTrain) 
