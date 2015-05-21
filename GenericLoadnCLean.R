# Load functions
source('functions.R')

# Load the libraries
library(psych)
library(e1071)
library(caret)
library(fBasics)

# Load the data
df<-read.csv(file.choose()) #lowbwt.csv
#df <-read.csv("d:/data/diabetes.csv")

# count blanks remove blanks
colSums(!is.na(df))
df <- na.omit(df)
#colSums(!is.na(df))

# clean the data names and data
df<-cleanit(df)

# remove a column
df$id <-NULL

# For more efficient analysis, transform the following 5 variables into factors:
df$race<- factor(df$race,levels=c(1,2,3),labels=c("White","Black","Other"))
df$smoke<- factor(df$smoke,levels=c(0,1),labels=c("Nonsmoker","Smoker")) 
df$ht<- factor(df$ht,levels=c(0,1),labels=c("NormalBP","HighBP"))
df$ui<- factor(df$ui,levels=c(0,1),labels=c("NoInfection","Infection"))

# REQUIRES caret package to split
# separate data into test and train sets, 75/25 split in this case
splitIndex <- createDataPartition(df$low, p = 0.75, list = FALSE)
train <- df[splitIndex, ]
test <- df[-splitIndex, ]

# Make some data frames to use
testInd <- test[ ,!colnames(test) %in% "low"]
testDep <- as.factor(test[, names(test) == "low"]) 
trainInd <- train[ ,!colnames(train) %in% "low"]
trainDep <- as.factor(train[, names(train) == "low"]) 

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

# Explore the data
str(dftrain)
summary(dftrain)

# Statistics
sapply(dftrain,mean)
sapply(dftrain,median)
sapply(dftrain,sd)

# REQUIRES fBasics package
basicStats(dftrain) #Computed basic stats
colStats(dftrain)
colMeans(dftrain) # Computes sample mean by col
colVars(dftrain) # Computes sample variance  by col
colSds(dftrain) # Computes sample standard deviation  by col
colSkewness (dftrain$age) # Computes sample skewness by col
colKurtosis (dftrain) # Computes sample kurtosis by col
colMaxs (dftrain) # Computes maximum values in each col
colMins (dftrain) # Computes minimum values in each col
colProds (dftrain) # Computes product of values in each col
colQuantiles(dftrain, prob=.10)#Computes product of values in each col

# Continous data use type=6
quantile(bwt, probs = c(5,10,25,50,75)/100, type=6)
IQR(bwt,type=6)
fivenum(bwt)

# REQUIRES psych package
describe(dftrain$bwt, type=1)
describeBy(dftrain$bwt,dftrain$low, type=1)

#aggreate
aggregate(age~low,mean,data=dftrain)
aggregate(age~low,fivenum,data=dftrain)
aggregate(age~low,median,data=dftrain)
aggregate(age~low,sd,data=dftrain)
aggregate(age~low,IQR,type=6,data=dftrain)
aggregate(age~low,skewness,type=1,data=dftrain)
aggregate(age~low,kurtosis,type=1,data=dftrain)

# REQUIRES e1071 package
# Use type 1
skewness(age,type=1)
# Uses excecess kurtisis (should be 0)
kurtosis(age, type=1)

# View some basic boxplots
boxplot(dftrain$age~dftrain$low, xlab="Survived", ylab="Age")
boxplot(dftrain$lwt~dftrain$low, xlab="Survived", ylab="Log Fare")
boxplot(dftrain$ftv~dftrain$low, xlab="Diabetes", ylab="Diastolic Blood Pressure")

# View a dotplot
dotchart(dftrain$age, xlab="Plasma Glucose")

# View Stripchart
stripchart(dftrain$age~dftrain$low,method="jitter", pch=c(1,2), col=c("red","blue"), xlab="Age", ylab="Survived", main="Age vs. Survived",offset=0.5)
stripchart(dftrain$age~dftrain$low,method="stack", pch=c(1,2), col=c("red","blue"), xlab="Plasma Glucose", ylab="Diabetes", main="Diabetes vs. Plasma Glucose",offset=0.5)

# two-way contingency table of categorical outcome and predictors we want
#  to make sure there are not 0 cells
xtabs(~low + age, data = dftrain)
summary(xtabs(~low + age, data = dftrain))

xtabs(~low + lwt, data = dftrain)
summary(xtabs(~low + lwt, data = dftrain))

xtabs(~ smoke + low, data = dftrain)
summary(xtabs(~smoke + low, data = dftrain))

xtabs(~ ui + low, data = dftrain)
summary(xtabs(~ui + low, data = dftrain))

# 2 way Freq Tables
# mytable <- table(A,B) # A will be rows(Dependent), B will be columns (Independent)
table(dftrain$low, dftrain$age)
sum(table(dftrain$low, dftrain$age))

margin.table(table(dftrain$low, dftrain$age),1) # Row frequencies (summed over columns) 
margin.table(table(dftrain$low, dftrain$age),2) # Column frequencies (summed over rows)

prop.table(table(dftrain$low, dftrain$age))# cell percentages relative frequences accross all
prop.table(table(dftrain$low, dftrain$age),1)# row percentages 
prop.table(table(dftrain$low, dftrain$age),2) # Column percentages

addmargins(table(dftrain$low, dftrain$age), margin=2) # Margin containing row sums
addmargins(table(dftrain$low, dftrain$age), margin=1) # Margin containing column sums
addmargins(table(dftrain$low, dftrain$age), FUN=sum) # Marginal sums
addmargins(table(dftrain$low, dftrain$age), FUN=mean) # Marginal means 
addmargins(table(dftrain$low, dftrain$age), FUN = list(Sum = sum, list(Min = min, Max = max))) # Marginal 
addmargins(table(dftrain$low, dftrain$age),  FUN = list(list(Min = min, Max = max), Sum = sum)) # Marginal
addmargins(table(dftrain$low, dftrain$age),  FUN = list(list(Sum = sum), Sum = sum)) # Marginal


# View Indiviudal Numerial variables Grouped by all the factor variables
cbind("SampleSize"=aggregate(age ~ race+smoke+ht+ui, dftest, length), "mean"=aggregate(age ~ race+smoke+ht+ui, dftest, mean)[,5],"StdErr"=aggregate(age ~ race+smoke+ht+ui, dftest, st.err)[,5])
cbind("SampleSize"=aggregate(lwt ~ race+smoke+ht+ui, dftest, length), "mean"=aggregate(lwt ~ race+smoke+ht+ui, dftest, mean)[,5],"StdErr"=aggregate(lwt ~ race+smoke+ht+ui, dftest, st.err)[,5])
cbind("SampleSize"=aggregate(bwt ~ race+smoke+ht+ui, dftest, length), "mean"=aggregate(bwt ~ race+smoke+ht+ui, dftest, mean)[,5],"StdErr"=aggregate(bwt ~ race+smoke+ht+ui, dftest, st.err)[,5])


qplot(age, low, color=factor(race), data=dftrain, geom=c("point", "smooth"))
qplot(lwt, low, color=factor(race), data=dftrain, geom=c("point", "smooth"))
qplot(age, ftv, color=factor(race), data=dftrain, geom=c("point", "smooth"))
qplot(age, bwt, color=factor(race), data=dftrain, geom=c("point", "smooth"))

## histogram of outcome
hist(dftrain$bwt)
rug(dftrain$bwt)

## simple data plot of outcome
plot (sort(dftrain$bwt))

## density plot of outcome
plot(density(dftrain$bwt,na.rm=TRUE))

#Check Outliers of outcome
outliers(dftrain$bwt)
plot(outliers(dftrain$bwt)$Z)
```
