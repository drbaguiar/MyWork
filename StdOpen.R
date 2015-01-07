##Clear the environment
rm(list=ls())

##Turn off scientific notations for numbers
options(scipen = 999)  

##Set locale
Sys.setlocale("LC_ALL", "English") 

##Load libraries
call <- function(x){
        if (!require(x,character.only = TRUE)){
                install.packages(x,dep=TRUE)
        }
}

##Use
call("dplyr")
call("Hmisc")
call("doBy")
call("rattle")
call("lattice")
call("data.table")
call("devtools")
call("ggplot2")
call("knitr")
call("reshape2")
call("adabag")
call("ada")
call("caret")
call("survival")
call("rCharts")
call("cluster")
call("NbClust")
call("psych")
call("RColorBrewer")
call("ROCR")

##Split a datafram
dfsplit <-function(dataframe,nbr1=2,nbr2=1){
        ##define % of training and test set 
        ##(use 2 then 1 for 50%, 4 then 3 for 75%, 5 then 4 for 80%, 5 than 3 for 60%, 5 than 4.5 for 90%)
        bound <- floor((nrow(dataframe)/nbr1)*nbr2)         
        ##sample rows 
        dataframe <- dataframe[sample(nrow(dataframe)), ]  
        ##get training set
        df.train <- dataframe[1:bound, ]   
        ##get test set
        df.test <- dataframe[(bound+1):nrow(dataframe), ]  
        list(trainset=df.train,testset=df.test)
}

##Use
## splits <- dfsplit(dataframe,4,3)
##df.train<- splits$trainset
##df.test <- splits$testset

##Set seed for reproducibility
set.seed(2345)

##Set data directory
##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
        datadir=('/home/bryan/GitHub/Data/')    
}else{
        datadir=('C:/Users/bryan_000/Documents/GitHub/Data/') 
}
