##Use my standard openning including call function
source('C:/GitHub/MyWork/StdOpen.R')

#Set name of datafile
datafile=paste(datadir,"diabetes.csv",sep = "")

#Check for the File. If not there, download the data 
if (!file.exists(datafile)) {
        url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data'
        download.file(url, destfile = datafile)
}

##Read the data
df <- read.table(datafile,sep = ",",header=FALSE)

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
colnames(df) <- c("timespregnant", "twohourglucose","diastolicbp","tricepskinfold","twohourinsulin","bmi","pedigree","age","diabetestest")

#Remove na's from the data
df <- na.omit(df)

##Copy the dataframe 
dftemp <-df

#Convert diabetestest to a factor
##aagemydata$diabetestest <- factor(mydata$diabetestest,levels=c(0,1),labels=c("Negative","Positive"))

responseY <- df[,dim(df)[2]]
predictorX <- df[,1:(dim(df)[2]-1)]

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
boxplot(age~diabetestest, main="Age",data=df)
boxplot(diastolicbp~diabetestest, main="Diastolic Blood Pressure",data=df)
boxplot(twohourglucose~diabetestest, main="Two Hour Glucose",data=df)
boxplot(timespregnant~diabetestest, main="Times Pregnant",data=df)

##Create a linear model
fit <- lm(diabetestest~., data=df)

##K-means cluster unscaled data
##Number of Clusters
nc=4

###remove extra columns
df <-df[,1:2]
cl <- kmeans(df,nc)
plot(df[,1],df[,2],col=cl$cluster, main="Unscaled")
points(cl$centers, pch=19)
abline (v=0, h=0)

##K-means cluster scaled data
###scale the data
dfscaled <-scale(df[,1:2])
clscaled <- kmeans(dfscaled,nc)
plot(dfscaled[,1],dfscaled[,2],col=clscaled$cluster, main="Scaled")
points(clscaled$centers, pch=19)
abline (v=0, h=0)

##Compare the cluster assignment of the scaled and unscaled data.
errortable<-table(cl$cluster,clscaled$cluster) ##CL in row CLSCALED in Column
errortable
table(cl$cluster - clscaled$cluster)
randIndex(errortable)
CrossTable(cl$cluster, clscaled$cluster)

##Cluster plot
clusplot(df, cl$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

##Cluster plot
clusplot(dfscaled, cl$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

##Compare the diabetes test results to the clusters
dftemp[,9] <- gsub("1","2",dftemp[,9]) ##change 1 to 2
dftemp[,9] <- gsub("0","1",dftemp[,9]) ##change 0 to 1
table(dftemp[,9],cl$cluster) ##Row is postivite test 1 no 2 yes
CrossTable(dftemp[,9], clscaled$cluster)
