##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}

datafile <-paste(datadir,"UniversalBank.csv",sep = "")
call("rpart.plot")

#Load the data
df <- read.csv(datafile)

#Clean the names using the clean function from std opening
dfclean <-clean(df)
df <- dfclean
rm(dfclean)

#Convert a categorical field to dummy
#education has values 1, 2, 3 
df<-dummy.data.frame(df,names="education")

#Split the data 
dftrain <-subset(df,partitioningvariable=="t")
dftest <-subset(df,partitioningvariable=="s")
dfvalidate <-subset(df,partitioningvariable=="v")
rm(df)

#Save the split data as separate files
datafile <-paste(datadir,"UniversalBankTraining.csv", sep="")
write.csv(dftrain, datafile,row.names=FALSE)
datafile <-paste(datadir,"UniversalBankTesting.csv", sep="")
write.csv(dftest, datafile,row.names=FALSE)
datafile <-paste(datadir,"UniversalBankValidating.csv", sep="")
write.csv(dfvalidate, datafile,row.names=FALSE)

#Classification tree
ctfit <- rpart(personalloan~age+experience+income+family+ccavg+education1+education2+education3+mortgage+cdaccount, method="class", data=dftrain)

# plot tree 
plot(ctfit)
text(ctfit, use.n=TRUE, all=TRUE,cex=.8)

prp(ctfit)
fancyRpartPlot(ctfit)

fitknn <- train(personalloan ~ .,
                  data = dftrain,
                  method = "knn",
                  trControl = trainControl(method = "cv"),
                  preProc = c("center", "scale")) 

predictknn<-predict(fitknn, dftest, na.action = na.pass)
