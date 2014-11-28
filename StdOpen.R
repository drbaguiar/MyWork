##Clear the environment
rm(list=ls())

##Turn off scientific notations for numbers
options(scipen = 999)  

##Set locale
Sys.setlocale("LC_ALL", "English") 

##Load libraries
call <- function(x)
{
        if (!require(x,character.only = TRUE))
        {
                install.packages(x,dep=TRUE)
        }
}

##Use
#call("Hmisc")

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

##Use
## splits <- dfsplit(dataframe)
##df.train<- splits$trainset
##df.test <- splits$testset



##Set seed for reproducibility
set.seed(2345)

datadir=('C:/Users/bryan_000/Documents/GitHub/Data/')

