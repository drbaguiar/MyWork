# Load functions
source('functions.R')

# Load the data
dfTrain<-read.csv("D:/Data/flutrain.csv")
dfTest<-read.csv("D:/Data/flutest.csv")

# Clean
dfTrain<-cleanit(dfTrain)
dfTest<-cleanit(dfTest)

# Average reading score males
tapply(pisaTrain$readingscore, pisaTrain$male==1, mean)

#Count Blanks
colSums(!is.na(dfTrain))
colSums(!is.na(dfTest))

# Remove blanks
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# Set the reference level of the factor by typing the following two lines in your R console:
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

#Dep and Independent Vars
# define columns we will be working with
depvar <- 'readingscore'
indepvars <-c('.')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

#Fit the model
fit<-lm(f1,data=pisaTrain)
fit
summary(fit)

# Check correations of independent variables
# Remove factor column and dep var
myvars <- names(pisaTrain) %in% c("raceeth", "readingscore") 
newdata <- pisaTrain[!myvars]
cor(newdata)

#vif
library(car)
vif(fit)
sqrt(vif(fit)) > 2 # problem?

# Predict
predTest<-predict(fit, pisaTest)
max(predTest) - min(predTest)

# Compute out-of-sample R^2
SSE = sum((predTest - pisaTest$readingscore)^2)
SST = sum((mean(pisaTrain$readingscore) - pisaTest$readingscore)^2)
R2 = 1 - SSE/SST
R2

# Compute the RMSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

# Compute baseline 
mean(pisaTrain$readingscore)
