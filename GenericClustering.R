rm(list=ls())
library(caret)
library(RWeka)
set.seed(1234)

# separate data into test and train sets, 70/30 split in this case
splitIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train <- iris[splitIndex, ]
test <- iris[-splitIndex, ]
testInd <- test[ ,!colnames(test) %in% "Species"]
testDep <- as.factor(test[, names(test) == "Species"]) 

TrainData <- iris[,1:4]
TrainClasses <- iris[,5]


#First Model
jripFit1 <- train(TrainData, TrainClasses,method = "JRip")
jripFit1
plot(jripFit1)

#Second Model
jripFit2 <- train(TrainData, TrainClasses,method = "JRip",preProcess = c("center", "scale"),tuneLength = 10,trControl = trainControl(method = "cv"))
jripFit2
plot(jripFit2)

# K means
neighborCount=2
modelKNN <- knn3(Species ~ ., data = train, k = neighborCount, prob = TRUE)
predKNN <- predict(modelKNN, testInd, type = "prob")
confKNN <- confusionMatrix(testDep, predKNN)

#Another Round
km <- kmeans(iris[,1:4], 3)
plot(iris[,1], iris[,2], col=km$cluster)
points(km$centers[,c(1,2)], col=1:3, pch=19, cex=2)
table(km$cluster, iris$Species)

#Another Way
km2 <- kmeans(iris[,1:4], 3)
plot(iris[,1], iris[,2], col=km2$cluster)
points(km2$centers[,c(1,2)], col=1:3, pch=19, cex=2)
table(km2$cluster, iris$Species)

#heir
m <- matrix(1:15,5,3)
dist(m) # computes the distance between rows of m (since there are 3 columns, it is the euclidian distance between tri-dimensional points)
dist(m,method="manhattan") # using the manhattan metric
sampleiris <- iris[sample(1:150, 40),] # get samples from iris dataset
# each observation has 4 variables, ie, they are interpreted as 4-D points
distance   <- dist(sampleiris[,-5], method="euclidean") 
cluster    <- hclust(distance, method="average")
plot(cluster, hang=-1, label=sampleiris$Species)
plot(as.dendrogram(cluster), edgePar=list(col="darkgreen", lwd=2), horiz=T) 
str(as.dendrogram(cluster)) # Prints dendrogram structure as text.
cluster$labels[cluster$order] # Prints the row labels in the order they appear in the tree.
#Prune by cluster
par(mfrow=c(1,2))
group.3 <- cutree(cluster, k = 3)  # prune the tree by 3 clusters
table(group.3, sampleiris$Species) # compare with known classes
plot(sampleiris[,c(1,2)], col=group.3, pch=19, cex=2.5, main="3 clusters")
points(sampleiris[,c(1,2)], col=sampleiris$Species, pch=19, cex=1)
group.6 <- cutree(cluster, k = 6)  # we can prune by more clusters
table(group.6, sampleiris$Species)
plot(sampleiris[,c(1,2)], col=group.6, pch=19, cex=2.5, main="6 clusters")
points(sampleiris[,c(1,2)], col=sampleiris$Species, pch=19, cex=1) # the little points are the true classes

par(mfrow=c(1,1))
plot(cluster, hang=-1, label=sampleiris$Species)
abline(h=0.9,lty=3,col="red")
height.0.9 <- cutree(cluster, h = 0.9)
table(height.0.9, sampleiris$Species) # compare with known classes

plot(sampleiris[,c(1,2)], col=height.0.9, pch=19, cex=2.5, main="3 clusters")
points(sampleiris[,c(1,2)], col=sampleiris$Species, pch=19, cex=1)

# Calculate the dissimilarity between observations using the Euclidean distance 
dist.iris <- dist(iris, method="euclidean")
# Compute a hierarchical cluster analysis on the distance matrix using the complete linkage method 
h.iris <- hclust(dist.iris, method="complete") 
h.iris
head(h.iris$merge, n=10)
plot(h.iris)
h.iris.heights <- h.iris$height # height values
h.iris.heights[1:10]
subs <- round(h.iris.heights - c(0,h.iris.heights[-length(h.iris.heights)]), 3) # subtract next height
which.max(subs)
# Cuts dendrogram at specified level and draws rectangles around the resulting clusters
plot(cluster); rect.hclust(cluster, k=6, border="red")
