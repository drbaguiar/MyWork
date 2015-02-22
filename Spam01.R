#Download Data Files:
source('C:/GitHub/MyWork/StdOpen.R')

call("kernlab")

##Set destination file for download 
spamfile <-paste(datadir,"spamdata.csv",sep = "")
spamnames <-paste(datadir,"spamnames.csv",sep = "")

#Check for the File. If not there, download the data 
if (!file.exists(spamfile)) {
        file.url <- 'http://thinktostart.com/data/data.csv'
        download.file(file.url, spamfile)
}

if (!file.exists(spamnames)) {
        file.url <- 'http://thinktostart.com/data/names.csv'
        download.file(file.url, spamnames)
}

#Load the two files into R:
spamdataset <- read.csv(spamfile,header=FALSE,sep=";")
spamnames <- read.csv(spamnames,header=FALSE,sep=";")

#Set the names of the spamdataset dataframe:
names(spamdataset) <- sapply((1:nrow(spamnames)),function(i) toString(spamnames[i,1]))

#make column y a factor variable for binary classification (spam or non-spam)
spamdataset$y <- as.factor(spamdataset$y)

#get a sample of 1000 rows
sample <- spamdataset[sample(nrow(spamdataset), 1000),]

#Split the data in dataTrain and dataTest
trainIndex <- createDataPartition(sample$y, p = .8, list = FALSE, times = 1)
dataTrain <- sample[ trainIndex,]
dataTest  <- sample[-trainIndex,]

#Create the SVM model:
### finding optimal value of a tuning parameter
sigDist <- sigest(y ~ ., data = dataTrain, frac = 1)

### creating a grid of two tuning parameters, .sigma comes from the earlier line. we are trying to find best value of .C
svmTuneGrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:7))

x <- train(y ~ .,
           data = dataTrain,
           method = "svmRadial",
           preProc = c("center", "scale"),
           tuneGrid = svmTuneGrid,
           trControl = trainControl(method = "repeatedcv", repeats = 5, 
                                    classProbs =  TRUE))
#Evaluate the model
pred <- predict(x,dataTest[,1:57])

acc <- confusionMatrix(pred,dataTest$y)