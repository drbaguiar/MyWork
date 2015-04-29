# Clear the environment
rm(list=ls())

# Turn off scientific notations for numbers
options(scipen = 999)  

# Set locale
Sys.setlocale("LC_ALL", "English") 

# Set seed for reproducibility
set.seed(2345)

df<- read.csv("D:/Data/traintitanic.csv")

# clean names
names(df) <-tolower(names(df))
names(df) <- gsub("\\(","",names(df))
names(df) <- gsub("\\)","",names(df))
names(df) <- gsub("-","",names(df))
names(df) <- gsub(",","",names(df))
names(df) <- gsub("\\.","",names(df))

# count blanks remove blanks
colSums(!is.na(df))
df <- na.omit(df)
colSums(!is.na(df))

# do the random split (25% held out for test), put the label back into the data frame
df$istest <- runif(nrow(df))<0.25
df$datalabel <- ifelse(df$istest,"test data","train data")
dftrain = df[!df$istest,]
dftest = df[df$istest,]

# View summary data
summary(dftest)

# Visualize the data
plot(dftrain)
boxplot(age~survived, data=dftrain)
boxplot(pclass~survived, data=dftrain)
hist(dftrain$age, xlab="Age", main="Age Histogram")
dotchart(dftrain$age, xlab="Age")
stripchart(dftrain$age~dftrain$survived,method="stack", pch=c(1,2), col=c("red","blue"), xlab="Age", ylab="Survived", main="Survived vs. Age",offset=0.5)
