#Logistic Regression Problem
# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/Gerber.csv")
df<-cleanit(df)
summary(df)

# count blanks remove blanks
barplot(colSums(!is.na(df)))
#df <- na.omit(df)
colSums(!is.na(df))

# Percentage of voters 
x<-  table(df$voting)
addmargins(x)
prop.table(x)

# Which group highest turnout
ftable(xtabs(df$voting~df$hawthorne+df$civicduty+df$neighbors+df$self))

# Dep and Independent Vars define columns we will be working with
depvar <- 'voting'
indepvars <-c('hawthorne','civicduty','neighbors','self')
exclude <- c() # numerical variables to exclude from using all
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
f2 <- paste(f1,paste(exclude,collapse=' - '),sep=' - ')

# Fit the model
fit<-glm(f1,data=df,family=binomial)
summary(fit)

# Baseline on Training data 
# Determine the Majority
bl <-table(df$voting)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority
predictTrainBase <-rep(majority,nrow(df))
#Compare
cm <- table(df$voting,predictTrainBase, exclude=NULL)
addmargins(cm)
getstats(cm)

# Predict on Training
thres = .30
predictTrain <- predict(fit, type="response")
cm <- table(df$voting,predictTrain>thres)
addmargins(cm)
getstats(cm)

# Build Receiver Operator Charastics ROC
library(ROCR)

# Prediction function
ROCRpredTrain = prediction(predictTrain,df$voting)

# Performance function
ROCRperfTrain = performance(ROCRpredTrain, "tpr", "fpr")

# Plot ROC curve and add AUC 
plot(ROCRperfTrain, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))
auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))

# Load CART packages
library(rpart)
library(rpart.plot)

# Build a regression tree
CARTmodel01 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=df)
prp(CARTmodel01)

CARTmodel02 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=df,cp=0.0)
prp(CARTmodel02)

CARTmodel03 = rpart(voting ~ civicduty + hawthorne + self + neighbors+ sex, data=df,cp=0.0)
prp(CARTmodel03)

CARTmodel04 = rpart(voting ~ civicduty + hawthorne + self + neighbors + control+sex, data=df,cp=0.0)
prp(CARTmodel04)

CARTmodel05 = rpart(voting ~control, data=df,cp=0.0)
prp(CARTmodel05, digits = 6)

CARTmodel06 = rpart(voting ~ control+sex, data=df,cp=0.0)
prp(CARTmodel06,digits = 6)

# Fit the model
fit2<-glm(voting ~ control+sex,data=df,family=binomial)
summary(fit2)

# We can quantify this precisely. Create the following dataframe (this contains all of the possible values of 
# sex and control), and evaluate your logistic regression using the predict function (where "LogModelSex" is 
#the name of your logistic regression model that uses both control and sex):
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(fit2, newdata=Possibilities, type="response")

# The four values in the results correspond to the four possibilities in the order they are stated above 
# ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) ). What is the absolute difference 
# between the tree and the logistic regression for the (Woman, Control) case? Give an answer with five numbers after 
# absthe decimal point.

# Fit the model
fit3<-glm(voting ~ control+sex+sex:control,data=df,family=binomial)
summary(fit3)

predict(fit3, newdata=Possibilities, type="response")
