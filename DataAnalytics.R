##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
        source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
        source('C:/GitHub/MyWork/StdOpen.R')   
}

##Regression
call("MASS")
call("car")
datafile <- paste(datadir,"house-prices.csv",sep = "")
houseprices <-read.csv(file=datafile)

##Prep data
houseprices$brick_d<-ifelse(houseprices$Brick=="Yes",1,0)
houseprices$east<-ifelse(houseprices$Neighborhood=="East",1,0)
houseprices$north<-ifelse(houseprices$Neighborhood=="North",1,0)

##Split Data
set.seed(110)
sub <- sample(nrow(houseprices), floor(nrow(houseprices) * 0.6))
training_data <- houseprices[sub,]
validation_data <- houseprices[-sub,]

##Linear model 1
lm.1 <- lm(Price ~ SqFt, data=training_data)
summary(lm.1)
plot(houseprices$SqFt, houseprices$Price, main="Scatter plot", xlab="Square feet", ylab="Price")
abline(lm.1,col="red",lwd=3)

##Linear Model 2
lm.fit1 <- lm(Price ~ SqFt+Bathrooms+Bedrooms+Offers+north+east+brick_d, data=training_data)
summary(lm.fit1)

##Can we drop
lm.fit1.step <- stepAIC(lm.fit1)
summary(lm.fit1.step)

##Check for multicollinearity LOOK for VIF being less than 10
vif(lm.fit1)

##Predict values on training set
training_data$predict.price <- predict(lm.fit1)
training_data$error <- residuals(lm.fit1)

##Predict values on validation set
validation_data$predict.price <- predict(lm.fit1,newdata=validation_data)
validation_data$error <- validation_data$predict.price - validation_data$Price

##Check residual plots
hist(training_data$error)
hist(validation_data$error)

##Correlation
a<-cor(training_data$Price,training_data$predict.price)
b<-cor(validation_data$Price,validation_data$predict.price)
a*a
b*b

##Logistic Regression
library(ISLR)
library(MASS)
attach(Default)
summary(Default)
#regression
model.1 <- glm(default~income, data=Default,family=binomial())
summary(model.1)

model.1.pred.prob <- predict(model.1,type="response")
plot(model.1.pred.prob,default)
plot(model.1.pred.prob,jitter(as.numeric(default)))

# 3. Split data
set.seed(110)
sub <- sample(nrow(Default), floor(nrow(Default) * 0.6))
training_data <- Default[sub,]
validation_data <- Default[-sub,]

model.stock <- glm(default~.,data=training_data,family=binomial())
summary(model.stock)
model.step <- stepAIC(model.stock)
summary(model.step)

training_data$pred.prob <- predict(model.step,type="response")
validation_data$pred.prob <- predict(model.step,type="response",newdata=validation_data)
plot(training_data$pred.prob,jitter(as.numeric(training_data$default)))
plot(validation_data$pred.prob,jitter(as.numeric(validation_data$default)))

# 6.1 Confusion matrix
training_data$class <- ifelse(training_data$pred.prob>.07,1,0)
validation_data$class <- ifelse(validation_data$pred.prob>.07,1,0)
table(training_data$default,training_data$class)
table(validation_data$default,validation_data$class)

#ROC

library(ROCR)
train_pred <- prediction(training_data$pred.prob, training_data$default)
train_perf <- performance(train_pred, measure = "tpr", x.measure = "fpr")
plot(train_perf, col=rainbow(5), main="training")
val_pred <- prediction(validation_data$pred.prob, validation_data$default)
val_perf <- performance(val_pred, measure = "tpr", x.measure = "fpr")
plot(val_perf, col=rainbow(10), main="validation")

##CLuster

data <- iris[,-5]
class <- iris[,5]
results <- kmeans(data,3)
table(class,results$cluster)

plot(data$Petal.Length,data$Petal.Width,col=results$cluster)
plot(data$Petal.Length,data$Petal.Width,col=class)
plot(data$Sepal.Length, data$Sepal.Width,col=results$cluster)
plot(data$Sepal.Length, data$Sepal.Width,col=class)

##Factor Analysis
datafile <- paste(datadir,"employee-factor.csv",sep = "")
emp_factor <- read.csv(datafile)
names(emp_factor)
emp_factor <- emp_factor[,-1] ##Remove employee id

#How many factors?
pca <- princomp(emp_factor)
summary(pca)
plot(pca)

fact <- factanal(emp_factor,5,rotation="varimax")
fact

fact.final<-factanal(emp_factor,5, rotation="varimax",score="regression")
fact.final
