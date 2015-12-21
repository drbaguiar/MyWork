##Clear the environment
rm(list=ls())

##Turn off scientific notations for numbers
options(scipen = 999)  

##Set locale
Sys.setlocale("LC_ALL", "English") 

##Set seed for reproducibility
set.seed(2345)

getstats <- function(cm){
  # Sensititvity a.k.a TPR
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
  
  # Negative Predictive Value
  npv <- cm[1,1]/(cm[1,1]+cm[2,1])
  
  rbind(TruePosSensitivity=tpr, FalsePos=fpr, TrueNegSpecificty=tnr, FalseNeg=fnr, PositivePredictiveValue=ppv, NegativePredictiveValue=npv, Accuracy = acc, Error = err)
}





##Build dataframe
flt<-c(1:24)
temp<-c(66,70,69,80,68,67,72,73,70,57,63,70,78,67,53,67,75,70,81,76,79,75,76,58)
damage<-c(0,1,0,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,1)
df<-as.data.frame(cbind(flt,temp,damage))
rm(temp)
rm(damage)
rm(flt)

#Plot
plot (df$temp,df$damage)

#Fit the simple model
fit<-glm(damage~temp,df,family=binomial())

#Review Output
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predictTrain<-predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals


thres=.1
# Predict
cm <- table(df$damage,predictTrain>thres)
addmargins(cm)
getstats(cm)

#Get all zeros correct 100% (TRUE NEGATIVE RATE)
#Get 4 out of 7 ones correct 57% (TRUE POSITVE RATE)
#Accuracy got 21 out of 24 correct 87.5%
#Error got 3 out of 24 incorrect 12.5%
#REMEMBER ACCuray is not a good measure of the woth of a model

# Build Receiver Operator Charastics ROC
library(ROCR)

# Prediction function
ROCRpredTest = prediction(predictTrain,df$damage)

# Performance function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))



library(rpart)
library(rpart.plot)
treeTrain <- rpart(damage~temp,data=df,method="class")
prp(treeTrain)