##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/Documents/GitHub/MyWork/StdOpen.R')   
}

# grow tree 
ctfit <- rpart(Kyphosis ~ Age + Number + Start,method="class", data=kyphosis)

printcp(ctfit) # display the results 
plotcp(ctfit) # visualize cross-validation results 
summary(ctfit) # detailed summary of splits

# plot tree 
plot(ctfit, uniform=TRUE, main="Classification Tree for Kyphosis")
text(ctfit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
pctfit<- prune(ctfit, cp=ctfit$cptable[which.min(ctfit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pctfit, uniform=TRUE, main="Pruned Classification Tree for Kyphosis")
text(pctfit, use.n=TRUE, all=TRUE, cex=.8)

# Regression Tree Example
# grow tree 
rtfit <- rpart(Mileage~Price + Country + Reliability + Type,method="anova", data=cu.summary)

printcp(rtfit) # display the results 
plotcp(rtfit) # visualize cross-validation results 
summary(rtfit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(rtfit) # visualize cross-validation results    

# plot tree 
plot(rtfit, uniform=TRUE,main="Regression Tree for Mileage ")
text(rtfit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
prtfit<- prune(rtfit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(prtfit, uniform=TRUE,main="Pruned Regression Tree for Mileage")
text(prtfit, use.n=TRUE, all=TRUE, cex=.8)
