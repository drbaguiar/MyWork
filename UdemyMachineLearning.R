# Reporducibility
set.seed(23)

library(ggplot2)

#take first 500 records
mydf <- diamonds[1:500,]
attach (mydf)

# 200 observations from 500 in the df
dfTrain = sample(x = 500, size = 200) 

# fitting a simple lm
mylm = lm(data =mydf, subset = dfTrain, x ~ y + z) 

# mean standard error
mean ((x - predict(mylm, mydf))[-dfTrain]^2) 

# for the cv functions
library(boot) 

# LOOCV
myglm = glm(data = mydf, x ~ y + z)
myglm.error = cv.glm(data = mydf, myglm)
# delta is the cv estimate or the error rate - raw and adjusted
myglm.error$delta

# Cross Validation K=5
myglm.error2 = cv.glm(data = mydf, myglm, K = 5)
# using K to adjust the group number
myglm.error2$delta

# EXERCISE
df<- faithful
plot(df$waiting,df$eruptions)

#Run Model
myglm = glm(data = df, waiting ~  eruptions)

# First we use leave one out cross validation
myglm.error = cv.glm(data = df, myglm)
# delta is the cv estimate or the error rate - raw and adjusted
myglm.error$delta

# Cross Validation K=5
myglm.error2 = cv.glm(data = df, myglm, K = 5)
# using K to adjust the group number
myglm.error2$delta

# Simple cross validation
mylm = glm(data =df[1:136,], waiting ~ eruptions) 
mylmPred<- predict(mylm, df[137:272,])

# mean standard error
mean((df[137:272,"waiting"]- mylmPred)^2) 

# we want to create a model to classify the number of cylinders

attach(mtcars)
library(lattice)

# according to weight and mpg
with(mtcars, xyplot(wt ~ mpg, group=cyl, auto.key=T, pch=20, cex=3))

# for KNN we need to get the library class
library(class)

# data frame for training
train <- cbind(mpg, wt)

# test data 
test4 <- c(26,2.2)
test6 <- c(20,3.0)
test8 <- c(12,5.2)

# Set K
nn = 2

# Predict the class
knn(train, test4, cl = cyl, k=nn, prob = T)
knn(train, test6, cl = cyl, k=nn, prob = T)
knn(train, test8, cl = cyl, k=nn, prob = T)

# LDA Classification

# we need MASS for the lda function
library(MASS)

# similar to lm and glm
mylda = lda(data=mtcars, cyl ~ wt + mpg)
mylda

# prior probabilities give the proportions of a class in the dataset
# we see the group means for each independent variable and class
# coefficients are calculated to define the areas of each class
plot(mylda)

# to use the predict function, it is useful to create a data frame with the test vectors
# test data as data frame
wt= c(2.2, 4, 1.1,5)
mpg= c(26, 20, 27, 15)
class=c(4, 6, 4, 8)
test = data.frame(wt, mpg,class)

#Make Predictions.  we specify that we want the class as output
mylda.prediction = predict(object = mylda, newdata = test[,c(1,2)])$class

#Table
mylda.prediction
table(mylda.prediction, test[,3])

# Logistic Regression

# in this case we want to model a binary outcome am, with wt, mpg and drat
head(mtcars)
mymtcars=data.frame(am = as.factor(mtcars$am),wt = mtcars$wt,mpg = mtcars$mpg,drat = mtcars$drat)

# since we are performing a logistic regression on a classification, we check if our outcome variable is a factor (class)
class(mymtcars$am)

# glm with family = binomial is the classic way of logistic regression in R
mylog = glm(data = mymtcars, am ~ wt + mpg + drat, family = "binomial")
summary(glm(data = mymtcars, am ~ wt + mpg + drat, family = "binomial"))

# in this case I decide to keep all three predictors in the model we are going to run the model on the training data itself
testprediction <- predict(mylog, type="response")
testprediction
# prob <= 0.5 means 0 or automatic

# we can get a character vector of the 2 transmission types
predicted.classes = rep( "automatic" ,32)
predicted.classes[testprediction > .5]="manual"
predicted.classes

table(predicted.classes, mymtcars$am)
# the table tells us that we had 2 misclassifications 

#now we see what the model would predict for our test add-on for the predict function it is best to use a data frame
addon = data.frame(wt = 4.500, mpg = 30.2 , drat = 4.9)
predict(mylog, addon, type="response")

## Exercise
Petal.Width <-c(.7,2.5)
Petal.Length<-c(2.4,7)
Species <-c("setosa","virginica")
test = data.frame(Petal.Width,Petal.Length,Species)

# similar to lm and glm
mylda = lda(data=iris, Species ~ Petal.Width+Petal.Length)
mylda
plot(mylda)

#Make Predictions.  we specify that we want the class as output
mylda.prediction = predict(object = mylda, newdata = test[,c(1,2)])$class

#Table
mylda.prediction
table(mylda.prediction, test[,3])

# Logistic Regression

# glm with family = binomial is the classic way of logistic regression in R
mylog = glm(data = iris, Species ~ Petal.Width+Petal.Length, family = "binomial")
summary(glm(data = iris, Species ~ Petal.Width+Petal.Length, family = "binomial"))

# in this case I decide to keep all three predictors in the model we are going to run the model on the training data itself
testprediction <- predict(mylog, type="response")
testprediction
table(testprediction, iris$Species)

predict(mylog, test, type="response")

# KNN
train <- cbind(iris$Petal.Width, iris$Petal.Length)
test = matrix(c(0.7, 2.5, 2.4, 7), nrow=2)
Species <-c("setosa","virginica")
knnPred<-knn(train, test, cl=iris$Species, k=3, prob=T)
table(knnPred, Species)


# Tree
library(tree)
attach(mtcars)

# load lattice so we can use xyplot
library(lattice)
with(mtcars, xyplot(mpg~wt, group=am, auto.key = T,pch=20,cex=3))

mytree <- tree(am~wt+mpg,data=mtcars)
plot(mytree)
text(mytree)
title("Psuedo treee ", sub="Auto V Manual")
summary(mytree)

#  Note the tree above gives means.  This is a regression tree versus a classification tree.  Need am to be a factor
# for a classification tree
mytree2 <- tree(as.factor(am)~wt+mpg,data=mtcars)
plot(mytree2)
text(mytree2)
title("Psuedo treee ", sub="Auto V Manual")
summary(mytree2)

# Lets split up the data
mtcars.new <-data.frame(am.new = as.factor(am),wt,mpg)
dfTrain<-mtcars.new[1:16,]
dfTest<-mtcars.new[17:32,]

# Make a tree with train
myTreeTrain<- tree(am.new~wt+mpg,data=dfTrain)
plot(myTreeTrain)
text(myTreeTrain)
summary(myTreeTrain)

# Predict
myTreePred<- predict(myTreeTrain,dfTest,type="class")

# 
treeTable<-table(myTreePred,dfTest$am.new)
(sum(diag(treeTable)))/16

# Random Forest and Bagging
library(randomForest)
library(ggplot2)

set.seed(123)

bagging <- randomForest(formula=color~.,data=diamonds[1:500,],mtr=9)
plot(bagging)

rf <- randomForest(formula=color~.,data=diamonds[1:500,],mtr=3)
importance(rf)
varImpPlot(rf)

#Test models
dfTest<-diamonds[501:800,]
predict.bagging<-predict(newdata=dfTest,bagging,type="class")
treebagging<-table(predict.bagging,dfTest$color)
(sum(diag(treebagging)))/300

predict.rf <-predict(newdata=dfTest,rf,type="class")
treerf<-table(predict.rf,dfTest$color)
(sum(diag(treerf)))/300

# USe with previous data
bagging <- randomForest(formula=am.new~wt+mpg,data=dfTrain,mtr=1)
plot(bagging)
dfTest<-mtcars.new[17:32,]
predict.bagging<-predict(newdata=dfTest,bagging,type="class")
treebagging<-table(predict.bagging,dfTest$am.new)
(sum(diag(treebagging)))/16

# Exercise
attach(diamonds)
dfTest<-diamonds[1:500,]
dfTest<-diamonds[501:1000,]

# Make a tree with train
myTreeTrain<- tree(color~price+x,data=dfTrain)
plot(myTreeTrain)
text(myTreeTrain)
summary(myTreeTrain)

# Predict
myTreePred<- predict(myTreeTrain,dfTest,type="class")

# 
treeTable<-table(myTreePred,dfTest$color)
(sum(diag(treeTable)))/500

# Clustering 
plot(rivers)

# Kmeans
kclust<-kmeans(rivers,centers=3,nstart=30)
plot(rivers, col=kclust$cluster)

# Heiracr

#Example of calcilating eculidian distance
a <-mtcars[1,]
b <-mtcars[11,]
dist(rbind(a,b))

#Get distance matrix for part of mtcars database
dm<-dist(as.matrix(mtcars[1:16,]))
hcluster<- hclust(dm)
plot(hcluster)

# Exerice
dfTrain <-iris[,1:3]
head(dfTrain)
library(rgl)
kclust3<-kmeans(dfTrain,centers=3,nstart=35)
kclust5<-kmeans(dfTrain,centers=5,nstart=35)
kclust8<-kmeans(dfTrain,centers=8,nstart=35)

cluster3vector<-which(kclust3$cluster==3)
cluster5vector<-which(kclust5$cluster==5)
cluster8vector<-which(kclust8$cluster==8)

plot3d(dfTrain,size=6,col=kclust3$cluster,xlab="",ylab="",zlab="",sub="3 cluster")
plot3d(dfTrain,size=6,col=kclust5$cluster,xlab="",ylab="",zlab="",sub="5 cluster")
plot3d(dfTrain,size=6,col=kclust8$cluster,xlab="",ylab="",zlab="",sub="8 cluster")

