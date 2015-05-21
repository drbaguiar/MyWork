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
column <- married
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
fit <- rpart(hcluster ~ ., method="class", data=dftrain)

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
fit <- rpart(hcluster ~ .,method="anova", data=dftrain)

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
fit <- randomForest(as.factor(hcluster) ~ .,   data=dftrain)
print(fit) # view results 
importance(fit) # importance of each predictor
varImpPlot(fit)
plot(fit)