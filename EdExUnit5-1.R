# Load functions
source('functions.R')

# Load the data
tweets = read.csv("D:/Data/tweets.csv", stringsAsFactors=FALSE)
str(tweets)

# Create a dependent variable
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

library(tm)
library(SnowballC)

# Preprocess
corpus = Corpus(VectorSource(tweets$Tweet)) # Create corpus
corpus # Look at corpus
writeLines(as.character(corpus[[1]])) # Look at corpus
corpus <-tm_map(corpus, content_transformer(tolower)) # Convert to lower-case
corpus = tm_map(corpus, removePunctuation) # Remove punctuation
#stopwords("english")[1:200] # Look at stop words 
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english"))) # Remove stopwords and apple
corpus = tm_map(corpus, stemDocument) # Stem document 
writeLines(as.character(corpus[[1]]))

# Bag of words
frequencies = DocumentTermMatrix(corpus) # Create matrix
frequencies # view  matrix
inspect(frequencies[1000:1005,505:515])# Look at matrix [docs,terms]
findFreqTerms(frequencies, lowfreq=20) # Check for sparsity (20 terms)
sparse = removeSparseTerms(frequencies, 0.995) # Remove sparse terms (remove 99.5 of the terms%)
sparse # view sparse terms

# Make DataFrame
tweetsSparse = as.data.frame(as.matrix(sparse)) #Convert to a data frame
colnames(tweetsSparse) = make.names(colnames(tweetsSparse)) # Make all variable names R-friendly
tweetsSparse$Negative = tweets$Negative # Add dependent variable
tweetsSparse <-cleanit(tweetsSparse)
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Split the data to  build models
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$negative, SplitRatio = 0.7)
dfTrain = subset(tweetsSparse, split==TRUE)
dfTest = subset(tweetsSparse, split==FALSE)
colnames(dfTrain) = make.names(colnames(dfTrain))
colnames(dfTest) = make.names(colnames(dfTest))
# clean things up
rm(tweetsSparse, tweets,corpus,frequencies,sparse,split)

# Build a CART model
library(rpart)
library(rpart.plot)
CARTTrain = rpart(negative ~ ., data=dfTrain, method="class")
prp(CARTTrain)

# Baseline on Training data 
# Determine the Majority
bl <-table(dfTrain$negative)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority of the training set
predictTrainBase <-rep(majority,nrow(dfTrain))
#Compare
cm <- table(dfTrain$negative,predictTrainBase,exclude=NULL)
addmargins(cm)
getstats(cm)

# Evaluate the performance of the model on Training SET
predictCARTTrain = predict(CARTTrain,type="class")
cm<-table(dfTrain$negative, predictCARTTrain)
addmargins(cm)
getstats(cm)

# Baseline on Testing data 
# Determine the Majority
bl <-table(dfTest$negative)
majority<-ifelse(bl[1]>bl[2],0,1)
# Fill in a prediction for the majority
predictTestBase <-rep(majority,nrow(dfTest))
#Compare
cm <- table(dfTest$negative,predictTestBase,exclude=NULL)
addmargins(cm)
getstats(cm)

# Evaluate the performance of the model on TEST SET
predictCARTTest = predict(CARTTrain, newdata=dfTest, type="class")
cm<-table(dfTest$negative, predictCARTTest)
thres=0.5
addmargins(cm)
getstats(cm)

# Random Forest
library(randomForest)
set.seed(123)

#Build the model
rfTrain = randomForest(negative~., data=dfTrain)

# Evaluate the performance of the model on Training SET
predictRFTrain = predict(rfTrain)
cm<-table(dfTrain$negative, predictRFTrain)
addmargins(cm)
getstats(cm)

# Make predictions on Testing set:
predictRFTest = predict(rfTrain, newdata=dfTest)
cm<-table(dfTest$negative, predictRFTest)
addmargins(cm)
getstats(cm)

# Logistic Regression Model
# Fit the model
fit<-glm(negative~.,data=dfTrain,family=binomial)

# Make predictions on testing set:
predictLRTrain = predict(fit, type="response")
thres = .50
cm <- table(dfTrain$negative,predictLRTrain>thres)
addmargins(cm)
getstats(cm)

# Make predictions on testing set:
predictLRTest = predict(fit, newdata=dfTest, type="response")
thres = .50
cm <- table(dfTest$negative,predictLRTest>thres)
addmargins(cm)
getstats(cm)