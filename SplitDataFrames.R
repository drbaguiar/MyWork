##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}

##Copy the original dataset
#df<- read.csv("C:/Users/bryan_000/Documents/GitHub/Data/ccFraud.csv")
df <-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data", header=F, sep=",")
colnames (df) <- c("npreg", "glucose","bp", "triceps", "insulin", "bmi", "diabetes", "age", "class")

##define % of training and test set 
##(use 2 then 1 for 50%, 4 then 3 for 75%, 5 then 4 for 80%, 5 than 3 for 60%, 5 than 4.5 for 90%)
bound <- floor((nrow(df)/5)*4.5)         

##sample rows 
df <- df[sample(nrow(df)), ]  

##get training set
df.train <- df[1:bound, ]   

##get test set
df.test <- df[(bound+1):nrow(df), ]    

##Another way to split the data

##Function to split the data in half
# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
        if (!is.null(seed)) set.seed(seed)
        index <- 1:nrow(dataframe)
        trainindex <- sample(index, trunc(length(index)/2))
        trainset <- dataframe[trainindex, ]
        testset <- dataframe[-trainindex, ]
        list(trainset=trainset,testset=testset)
}

##Apply the Function
splits <- splitdf(df, seed=808)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

##Split a datafram
dfsplit <-function(dataframe){
        ##define % of training and test set 
        ##(use 2 then 1 for 50%, 4 then 3 for 75%, 5 then 4 for 80%, 5 than 3 for 60%, 5 than 4.5 for 90%)
        bound <- floor((nrow(df)/4)*3)         
        ##sample rows 
        df <- df[sample(nrow(df)), ]  
        ##get training set
        df.train <- df[1:bound, ]   
        ##get test set
        df.test <- df[(bound+1):nrow(df), ]  
        list(trainset=df.train,testset=df.test)
}

## splits <- dfsplit(dataframe)
##df.train<- splits$trainset
##df.test <- splits$testset

