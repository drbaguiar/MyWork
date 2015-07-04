#Logistic Regression Problem
# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/songs.csv")
df<-cleanit(df)
summary(df)
# count blanks remove blanks
barplot(colSums(!is.na(df)))
#df <- na.omit(df)
#colSums(!is.na(df))

subset(df, artistname=="Michael Jackson"& top10==1)
table(df$timesignature)
which(df$tempo==244.307)
max(df$tempo)
df[6206,]

# Split the data based on election year
dfTest = subset(df, year == 2010)
dfTrain = subset(df, year < 2010)
rm(df)

#Remove columns from the dataframe we do not want to use
nonvars = c("year", "songtitle", "artistname", "songid", "artistid")
dfTrain = dfTrain[ , !(names(dfTrain) %in% nonvars) ]
dfTest = dfTest[ , !(names(dfTest) %in% nonvars) ]

# Baseline on Test
# Determine the Majority
table(dfTrain$top10)
# Fill in a prediction
predictTestBase <-rep(0,nrow(dfTrain))
#Compare
cm <- table(dfTrain$top10,predictTestBase, exclude=NULL)
addmargins(cm)
getstats(cm)

#multicolinearity
cor(dfTrain$loudness,dfTrain$energy)

# Dep and Independent Vars define columns we will be working with
depvar <- 'top10'
indepvars <-c('.')
exclude <- c('energy') # numerical variables to exclude
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
f2 <- paste(f1,paste(exclude,collapse=' - '),sep=' - ')



# Fit the model
fit<-glm(f1,data=dfTrain,family=binomial)
summary(fit)

# Fit model excluding variables
fit2<-glm(f2,data=dfTrain,family=binomial)
summary(fit2)

# Predict on Training
thres = .45
predictTrain <- predict(fit, type="response")
cm <- table(dfTrain$top10,predictTrain>thres)
addmargins(cm)
getstats(cm)

# Pull out mistakes
subset(dfTrain, predictTrain >= thres & top10 == 0)
subset(dfTrain, predictTrain <= thres & top10 == 1)

# Predict on Testing
predictTest <- predict(fit2, type="response", newdata=dfTest)
cm <- table(dfTest$top10,predictTest>thres)
addmargins(cm)
getstats(cm)

# Pull out mistakes
subset(dfTest, predictTest >= thres & republican == 0)
subset(dfTest, predictTest <= thres & republican == 1)

# Compare Baseline on Test
# Determine the Majority
table(dfTest$top10)
# Fill in a prediction
predictTestBase <-rep(0,nrow(dfTest))
#Compare
cm <- table(dfTest$top10,predictTestBase, exclude=NULL)
addmargins(cm)
getstats(cm)
