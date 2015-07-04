#Logistic Regression Problem
# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/pollingdata.csv")
df<-cleanit(df)

summary(df)
# count blanks remove blanks
colSums(!is.na(df))
#df <- na.omit(df)
#colSums(!is.na(df))

## Need to deal with missing values 
# multiple imputation
library(mice)

# make a df with just 4 of the variables 
simple <- df[c("rasmussen","surveyusa","propr","diffcount")]
set.seed(144)

# Impute values
imputed<- complete(mice(simple))
summary(imputed)

# Copy imputed columns back to df
df$rasmussen<- imputed$rasmussen 
df$surveyusa <- imputed$surveyusa
summary(df)
colSums(!is.na(df))

# Split the data based on election year
dfTrain = subset(df, year == 2004 | year == 2008)
dfTest = subset(df, year == 2012)
rm(df)

# Baseline based on Majority a.k.a. Naive Baseline
# Determine the Majority
table(dfTrain$republican)
# Fill in a prediction
predictTestBase <-rep(11,nrow(dfTrain))
#Compare
cm <- table(dfTrain$republican,predictTestBase, exclude=NULL)
addmargins(cm)
getstats(cm)

# Baseline based on using one poll (Rasmussen)
thres<-0.5
cm <- table(dfTrain$republican,sign(dfTrain$rasmussen)>thres)
addmargins(cm)
getstats(cm)

# Pull out mistakes
subset(dfTrain, rasmussen >= thres & republican == 0)
subset(dfTrain, rasmussen <= thres & republican == 1)

# Dep and Independent Vars define columns we will be working with
depvar <- 'republican'
indepvars <-c('rasmussen', 'surveyusa','diffcount','propr')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

# Check to see if indepent vars are correlated
cor(dfTrain[c(indepvars)])

# Fit the model
fit<-step(glm(f1,data=dfTrain,family=binomial))
summary(fit)

# Predict on Training
predictTrain <- predict(fit, type="response")
cm <- table(dfTrain$republican,predictTrain>thres)
addmargins(cm)
getstats(cm)

# Pull out mistakes
subset(dfTrain, predictTrain >= thres & republican == 0)
subset(dfTrain, predictTrain <= thres & republican == 1)

# Predict on Testing
predictTest <- predict(fit, type="response", newdata=dfTest)
cm <- table(dfTest$republican,predictTest>thres)
addmargins(cm)
getstats(cm)

# Pull out mistakes
subset(dfTest, predictTest >= thres & republican == 0)
subset(dfTest, predictTest <= thres & republican == 1)

# Baseline based on Majority a.k.a. Naive Baseline
# Determine the Majority
table(dfTest$republican)
# Fill in a prediction
predictTestBase <-rep(0,nrow(dfTest))
#Compare
cm <- table(dfTest$republican,predictTestBase, exclude=NULL)
addmargins(cm)
getstats(cm)


# Build Receiver Operator Charastics ROC
library(ROCR)

# Prediction function
ROCRpredTest = prediction(predictTest,dfTest$republican)

# Performance function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))

# Classification tree
# Rpart
library(rpart)
library(rpart.plot)
treeTrain <- rpart(f1,data=dfTrain,method="class")
prp(treeTrain)