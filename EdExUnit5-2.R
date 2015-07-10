# Load functions
source('functions.R')

# Load the data
emails = read.csv("D:/Data/energy_bids.csv", stringsAsFactors=FALSE)
str(emails)

# Look at first email
emails$email[1]

# Better way to look
strwrap(emails$email[2])

# Responsive emails 
table(emails$responsive)

library(tm)
library(SnowballC)

# Preprocess
corpus = Corpus(VectorSource(emails$email)) # Create corpus
corpus # Look at corpus
writeLines(as.character(corpus[[1]])) # Look at corpus
corpus <-tm_map(corpus, content_transformer(tolower)) # Convert to lower-case
corpus = tm_map(corpus, removePunctuation) # Remove punctuation
#stopwords("english")[1:200] # Look at stop words 
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english"))) # Remove stopwords and apple
corpus = tm_map(corpus, stemDocument) # Stem document 
writeLines(as.character(strwrap(corpus[[1]])))

# Bag of words
dtm <- DocumentTermMatrix(corpus) # Create matrix
dtm # view  matrix
dtm <- removeSparseTerms(dtm, 0.97) # Remove sparse terms (remove 97 of the terms%)
dtm # view sparse terms

# Make DataFrame
df <- as.data.frame(as.matrix(dtm)) #Convert to a data frame
colnames(df) <- make.names(colnames(df)) # Make all variable names R-friendly
df$responsive <-emails$responsive # Add dependent variable
df <-cleanit(df)
colnames(df) = make.names(colnames(df))

# Split the data to  build models
library(caTools)
set.seed(144)
split = sample.split(df$responsive, SplitRatio = 0.7)
dfTrain = subset(df, split==TRUE)
dfTest = subset(df, split==FALSE)
colnames(dfTrain) = make.names(colnames(dfTrain))
colnames(dfTest) = make.names(colnames(dfTest))
# clean things up
rm(df, emails,corpus,dtm,split)

# Build a CART model
library(rpart)
library(rpart.plot)
CARTTrain = rpart(responsive ~ ., data=dfTrain, method="class")
prp(CARTTrain)

# Baseline on Training data 
# Determine the Majority
bl <-table(dfTrain$responsive)
majority<-ifelse(bl[1]>bl[2],0,1)

# Fill in a prediction for the majority of the training set
predictTrainBase <-rep(majority,nrow(dfTrain))
#Compare
cm <- table(dfTrain$responsive,predictTrainBase,exclude=NULL)
addmargins(cm)
getstats(cm)

# Evaluate the performance of the model on Training SET
predictCARTTrain = predict(CARTTrain,type="class")
cm<-table(dfTrain$responsive, predictCARTTrain)
addmargins(cm)
getstats(cm)

# Baseline on Testing data 
# Determine the Majority
bl <-table(dfTest$responsive)
majority<-ifelse(bl[1]>bl[2],0,1)
# Fill in a prediction for the majority
predictTestBase <-rep(majority,nrow(dfTest))
#Compare
cm <- table(dfTest$responsive,predictTestBase,exclude=NULL)
addmargins(cm)
getstats(cm)

# Evaluate the performance of the model on TEST SET
predictCARTTest = predict(CARTTrain, newdata=dfTest)
cm<-table(dfTest$responsive, predictCARTTest)
thres=0.15
addmargins(cm)
getstats(cm)

# ROC curve
library(ROCR)
predROCR = prediction(predictCARTTest[,2], dfTest$responsive)
plot(, colorize=TRUE)
plot(perfROCR, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(coef=c(0,1))

# Compute AUC
auc = as.numeric(performance(predROCR, "auc")@y.values)
text(0.5, 1, "AUC:")
text(0.6,1, round(auc,4))