##Use my standard openning including call function
source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')

call("cluster")
call("fpc")
call("flexclust")
call("NbClust")

##Set printing to 1 row of 3 columns
par(mfrow=c(1,3))

#Set name of datafile
datafile=paste(datadir,"Credit_Scoring.csv",sep = "")

#Read  the File. 
if (file.exists(datafile)) {
        df <- read.csv(datafile,sep = ",",header=T)
}

nbrgrp=2

##View means of speed, cycle, and wear
aggregate(. ~ Credit, data = df, mean)

##View sd of speed, cycle, and wear
aggregate(. ~ Credit, data = df, sd)

##View correlation matrix (not including score)
cor(df[,-1])

##View boxplots
#boxplot(speed~group, data=df, main="Speed", xlab="Group")
#boxplot(cycle~group, data=df, main="Cycle", xlab="Group")
#boxplot(wear~group, data=df, main="Wear", xlab="Group")

##K-Means Cluster (not including the group column)
cl <-kmeans(df[,-1],nbrgrp)

## Compare Group to Cluster assigned using adjusted rank index
## The adjusted Rand index provides a measure of the agreement between two partitions, adjusted for chance. 
## It ranges from -1 (no agreement) to 1 (perfect agreement). 
errortable<-table(df[,1],cl$cluster)
randIndex(errortable)

##Plot the values of speed, cycle, and wear colored by cluster
#plot(df[,2], col=cl$cluster,main="Speed",)
#abline(v=c(nbr,nbr+nbr))

#plot(df[,2], col=cl$cluster, main="Cycle")
#abline(v=c(nbr,nbr+nbr))
       
#plot(df[,6], col=cl$cluster, main="Wear")
#abline(v=c(nbr,nbr+nbr))
 
##Reset to one row row column
par(mfrow=c(1,1))

##
plotcluster(df, cl$cluster)

##Cluster plot
clusplot(df, cl$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

##Pairs graph
#with(df, pairs(df[,-1], col=c(1:3)[cl$cluster])) 

responseY <- df[,1]
predictorX <- df[,2:21]

##For the convenience of visualization, we take the first two principle components as the new feature variables and conduct k-means only on these two dimensional data.
pca <- princomp(predictorX, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)

X <- cbind(pc.comp1, pc.comp2)
cl <- kmeans(X,2)
cl$cluster
plot(pc.comp1, pc.comp2,col=cl$cluster)
points(cl$centers, pch=16)
abline (v=0)
abline(h=0)

fit <-glm(Credit~., data=df, family=binomial)

