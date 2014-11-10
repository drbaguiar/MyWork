##Clear the workspace and enivronment
rm(list=ls())

#Check for the File. If not there, download the data 
if (!file.exists("C:/Users/bryan_000/Documents/GitHub/Data/diabetes.csv")) {
        url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data'
        download.file(url, destfile = "C:/Users/bryan_000/Documents/GitHub/Data/diabetes.csv")
}

##Read the data
mydata <- read.table("C:/Users/bryan_000/Documents/GitHub/Data/diabetes.csv",sep = ",",header=FALSE)

##For Each Attribute: (all numeric-valued)
##1. Number of times pregnant
##2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test
##3. Diastolic blood pressure (mm Hg)
##4. Triceps skin fold thickness (mm)
##5. 2-Hour serum insulin (mu U/ml)
##6. Body mass index (weight in kg/(height in m)^2)
##7. Diabetes pedigree function
##8. Age (years)
##9. Class variable (0 or 1)

##Change the column names
colnames(mydata) <- c("timespregnant", "twohourglucose","diastolicbp","tricepskinfold","twohourinsulin","bmi","pedigree","age","diabetestest")

#Remove na's from the data
mydata <- na.omit(mydata)

##attach the data
attach(mydata)

#Convert diabetestest to a factor
##aagemydata$diabetestest <- factor(mydata$diabetestest,levels=c(0,1),labels=c("Negative","Positive"))

responseY <- mydata[,dim(mydata)[2]]
predictorX <- mydata[,1:(dim(mydata)[2]-1)]

##For the convenience of visualization, we take the first two principle components as the new feature variables and conduct k-means only on these two dimensional data.
pca <- princomp(predictorX, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)

##In R, kmeans performs the K-means clustering analysis, ()$cluster provides the clustering results and ()$centers provides the centroid vector (i.e., the mean) for each cluster.
##Take k = 13 as the number of clusters in K-means analysis. Figure 1 shows the resulting scatter plot with different clusters in different colors. The solid black circles are the centers of the clusters.
X <- cbind(pc.comp1, pc.comp2)
cl <- kmeans(X,13)
cl$cluster
plot(pc.comp1, pc.comp2,col=cl$cluster)
points(cl$centers, pch=16)
abline (v=0)
abline(h=0)

##Draw some boxplots
boxplot(age~diabetestest, main="Age")
boxplot(diastolicbp~diabetestest, main="Diastolic Blood Pressure")
boxplot(twohourglucose~diabetestest, main="Two Hour Glucose")
boxplot(timespregnant~diabetestest, main="Times Pregnant")

##Create a linear model
fit <- lm(diabetestest~., data=mydata)


