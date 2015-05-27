# Load functions
source('functions.R')


# load the library
#library(mlbench)
library(caret)

#Parallize
#library(doSMP)
#w <- startWorkers()
#registerDoSMP(w)


# Load the data
#Load the birthweight data (lowbw.csv)
#df<-read.csv(file.choose())
df<-read.csv("D:/Data/diabetes.csv")

# clean
df<- cleanit(df)

# count blanks remove blanks
colSums(!is.na(df))
#df <- na.omit(df)
#colSums(!is.na(df))

#make Y a factor
df$diabetes <- factor(df$diabetes,levels=c(1,0), labels=c("Yes","No"))

#SPlit into train and testing datasets
sub = sample(nrow(df), floor(nrow(df) * 0.9))
train = df[sub,]
test = df[-sub,]

# break apart columns into dependent and independent variables
xTrain = train[,-9]
yTrain = train$diabetes
xTest = test[,-9]
yTest = test$diabetes

# prepare training scheme
fitControl <- trainControl(method = "repeatedcv",number =10,repeats = 3)

# train the LVQ model
modelLvq <- train(diabetes~., data=train, method="lvq", trControl=fitControl)

# make classifications
predLvq<-predict(modelLvq,xTest)
confusionMatrix(yTest, predLvq)

# train the GBM model
modelGbm <- train(diabetes~., data=train, method="gbm", trControl=fitControl, verbose=FALSE)

# make classifications
predGbm<-predict(modelGbm,xTest)
confusionMatrix(yTest, predGbm)

# train the SVM model
modelSvm <- train(diabetes~., data=train, method="svmRadial", trControl=fitControl)

# make classifications
predSvm<-predict(modelSvm,xTest)
confusionMatrix(yTest, predSvm)

# collect resamples
results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm))

# summarize the distributions
summary(results)

# boxplots of results
bwplot(results)

# dot plots of results
dotplot(results)

results2<-cbind(LVQ=predLvq, GBM=predGbm, SVM=predSvm)
results2
diff(results2)
