#Logistic Regression Problem
# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/loans.csv")
df<-cleanit(df)
summary(df)

x<-  table(df$notfullypaid)
addmargins(x)
prop.table(x)

# count blanks remove blanks
barplot(colSums(!is.na(df)))
#df <- na.omit(df)
                                                                                                                                                                                                                                                                                                                                                    Impute Values
# Note that to do this imputation, we set vars.for.imputation to all variables in the data frame except for
# not.fully.paid, to impute the values using all of the other independent variables.
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(df), "not.fully.paid")
imputed = complete(mice(df[vars.for.imputation]))
df[vars.for.imputation] = imputed

# Split the data
library(caTools)
set.seed(144)
split = sample.split(df$notfullypaid, SplitRatio = 0.7)
dfTrain = subset(df, split==TRUE)
dfTest = subset(df, split==FALSE)
rm(df,split,imputed)

# Baseline on Training data 
# Determine the Majority
bl <-table(dfTrain$notfullypaid)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority
predictTestBase <-rep(majority,nrow(dfTrain))
#Compare
cm <- table(dfTrain$notfullypaid,predictTestBase, exclude=NULL)
addmargins(cm)
getstats(cm)

# multicolinearity
# Exclude dep var column and factor column
cor(dfTrain[,c(-2,-14)])

# Dep and Independent Vars define columns we will be working with
depvar <- 'notfullypaid'
indepvars <-c('.')
exclude <- c('') # numerical variables to exclude from using all
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
f2 <- paste(f1,paste(exclude,collapse=' - '),sep=' - ')

# Fit the model
fit<-glm(f1,data=dfTrain,family=binomial)
summary(fit)

# Fit model excluding variables
fit2<-glm(f2,data=dfTrain,family=binomial)
summary(fit2)

# step the model
fitstep<-step(glm(f1,data=dfTrain,family=binomial))
summary(fitstep)

# Predict on Training
thres = .5
predictTrain <- predict(fitstep, type="response")
cm <- table(dfTrain$notfullypaid,predictTrain>thres)
addmargins(cm)
getstats(cm)

# Pull out mistakes
subset(dfTrain, predictTrain >= thres & notfullypaid == 0)
subset(dfTrain, predictTrain <= thres & notfullypaid == 1)

# Build Receiver Operator Charastics ROC
library(ROCR)

# Predict on Testing
predictTest <- predict(fitstep, type="response", newdata=dfTest)
cm <- table(dfTest$notfullypaid,predictTest>thres)
addmargins(cm)
getstats(cm)

# Pull out mistakes
subset(dfTest, predictTest >= thres & notfullypaid == 0)
subset(dfTest, predictTest <= thres & notfullyaid == 1)

#Add predicted risk to table
dfTest$predictedrisk<-predictTest

# Build Receiver Operator Charastics ROC
library(ROCR)

# Prediction function
ROCRpredTest = prediction(predictTest,dfTest$notfullypaid)

# Performance function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))

# Calculate orofit and loss
#c<-10 # Amount
#r<-.06 # Interst rate per time period (years)
#t<-3 # Time period (years)
#total<- c * exp(r*t)
#profit <- c * (exp(r*t) - 1) 
#rbind (Loan=c,TotalPayments=total,Profit=profit)

#Add profit column to test set
dfTest$profit = exp(dfTest$intrate*3) - 1
dfTest$profit[dfTest$notfullypaid == 1] = -1
max(dfTest$profit)*10

# Find high interst loans
dfTestHigh <-subset(dfTest, intrate> .15)

# Average profit from high interest gropu
mean(dfTestHigh$profit)

# Percents
x<-table(dfTestHigh$notfullypaid)
addmargins(x)
prop.table(x)

cutoff <-sort(dfTestHigh$predictedrisk, decreasing=FALSE)[100]
dfTestFinal<-subset(dfTestHigh, predictedrisk<=cutoff)
sum(dfTestFinal$profit)
# Percents
x<-table(dfTestFinal$notfullypaid)
addmargins(x)
prop.table(x)
cutoff
