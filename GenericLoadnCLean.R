# Clear the environment
rm(list=ls())

# Turn off scientific notations for numbers
options(scipen = 999)  

# Set locale
Sys.setlocale("LC_ALL", "English") 

# Set seed for reproducibility
set.seed(2345)

# Load the libraries
library(psych)
library(e1071)
library(caret)

# Load the data
#df<-read.csv(file.choose())
df <-read.csv("d:/data/diabetes.csv")

# count blanks remove blanks
colSums(!is.na(df))
#df <- na.omit(df)
#colSums(!is.na(df))

# clean the data names and data
names(df) <-tolower(names(df))
names(df) <- gsub("\\(","",names(df))
names(df) <- gsub("\\)","",names(df))
names(df) <- gsub("\\.","",names(df))
names(df) <- gsub("_","",names(df))
names(df) <- gsub("-","",names(df))
names(df) <- gsub(",","",names(df))

# remove a column
#df$obsno <-NULL
  
# REQUIRES caret package to split
# separate data into test and train sets, 75/25 split in this case
splitIndex <- createDataPartition(df$diabetes, p = 0.75, list = FALSE)
train <- df[splitIndex, ]
test <- df[-splitIndex, ]

# Make some data frames to use
testInd <- test[ ,!colnames(test) %in% "diabetes"]
testDep <- as.factor(test[, names(test) == "diabetes"]) 
trainInd <- train[ ,!colnames(train) %in% "diabetes"]
trainDep <- as.factor(train[, names(train) == "diabetes"]) 

# do the random split (25% held out for test), put the label back into the data frame
df$istest <- runif(nrow(df))<0.25
df$datalabel <- ifelse(df$istest,"test data","train data")
dftrain = df[!df$istest,]
dftest = df[df$istest,]

# remove unneeded columns
cols<-c("istest","datalabel")
dftrain<-dftrain[,!names(dftrain) %in% cols]
dftest<-dftest[,!names(dftest) %in% cols]

#remove uneeded variables and dataframes
rm(df,splitIndex,test,train,cols)

# attach for working
attach(dftrain)

# Explore the data
str(dftrain)
summary(dftrain)

# Statistics
sapply(dftrain,mean)
sapply(dftrain,median)
sapply(dftrain,sd)

# Continous data use type=6
quantile(plasmaglucose, probs = c(25, 50, 75)/100, type=6)
IQR(plasmaglucose,type=6)
fivenum(plasmaglucose)

# REQUIRES psych package
describe(plasmaglucose, type=1)
describeBy(plasmaglucose,diabetes, type=1)

#aggreate
aggregate(plasmaglucose~diabetes,mean,data=dftrain)
aggregate(plasmaglucose~diabetes,fivenum,data=dftrain)
aggregate(plasmaglucose~diabetes,median,data=dftrain)
aggregate(plasmaglucose~diabetes,sd,data=dftrain)
aggregate(plasmaglucose~diabetes,IQR,type=6,data=dftrain)
aggregate(plasmaglucose~diabetes,skewness,type=1,data=dftrain)
aggregate(plasmaglucose~diabetes,kurtosis,type=1,data=dftrain)

# REQUIRES e1071 package
# Use type 1
skewness(plasmaglucose,type=1)
# Uses excecess kurtisis (should be 0)
kurtosis(plasmaglucose, type=1)

# View some basic boxplots
boxplot(plasmaglucose~diabetes, xlab="Diabetes", ylab="Plasma Glucose")
boxplot(bmi~diabetes, xlab="Diabetes", ylab="Body Mass Index")
boxplot(diastolic~diabetes, xlab="Diabetes", ylab="Diastolic Blood Pressure")
boxplot(x2hourseruminsulin~diabetes, xlab="Diabetes", ylab="2 Hour Serum Insulin")

# View a dotplot
dotchart(plasmaglucose, xlab="Plasma Glucose")

# View Stripchart
stripchart(plasmaglucose~diabetes,method="jitter", pch=c(1,2), col=c("red","blue"), xlab="Plasma Glucose", ylab="Diabetes", main="Diabetes vs. Plasma Glucose",offset=0.5)
stripchart(plasmaglucose~diabetes,method="stack", pch=c(1,2), col=c("red","blue"), xlab="Plasma Glucose", ylab="Diabetes", main="Diabetes vs. Plasma Glucose",offset=0.5)

# two-way contingency table of categorical outcome and predictors we want
#  to make sure there are not 0 cells
xtabs(~diabetes + timespregnant, data = dftrain)










# Hierarchical Cluster Analysis
hc <- hclust(dist(dftrain))   # apply hirarchical clustering 
plot(hc)                      # Print Dendrogram

# Hierarchical Cluster Analysis 
# set nbr to determine how many to cut
nbr=3
di <- dist(dftrain, method="euclidean")
tree <- hclust(di, method="ward.D2")
dftrain$hcluster <- as.factor((cutree(tree, k=nbr)-2) %% 3 +1)

detach(dftrain)
attach(dftrain)

# that modulo business just makes the coming table look nicer
plot(tree, xlab="")
rect.hclust(tree, k=nbr, border="red")

#Pull out records by a value in a column 
extracted <- dftrain[dftrain$hcluster==3,]
summary(extracted)

# aggregate group by
aggregate(dftrain, by=list(hcluster),FUN=mean, na.rm=TRUE)

# Compare
column <- timespregnant
with(dftrain, table(hcluster, column))
barplot(with(dftrain, table(hcluster, column)),col=c("red","green","blue"),beside = TRUE)

# Measures of Central Tendency
mean(dftrain$age,na.rm=TRUE)
median(dftrain$age,na.rm=TRUE)
# REQUIRES psych package
geometric.mean(dftrain$age,na.rm=TRUE) #not useful if zero in data
harmonic.mean(dftrain$age,na.rm=TRUE)

##Classification Tree
library(rpart)
fit <- rpart(diabetes ~ ., method="class", data=dftrain)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", title = "Classification Tree")

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE,   main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps", title = "Pruned Classification Tree")

# Regression Tree 
library(rpart)

# grow tree 
fit <- rpart(diabetes ~ .,method="anova", data=dftrain)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
rsq.rpart(fit) # visualize cross-validation results    

# plot tree 
plot(fit, uniform=TRUE, main="Regression Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", title = "Regression Tree ")

# prune the tree 
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, main="Pruned Regression Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree2.ps",title = "Pruned Regression Tree")

# Random Forest prediction 
library(randomForest)
fit <- randomForest(as.factor(diabetes) ~ .,   data=dftrain)
print(fit) # view results 
importance(fit) # importance of each predictor
varImpPlot(fit)
plot(fit)