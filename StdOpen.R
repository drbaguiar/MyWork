##Clear the environment
rm(list=ls())

##Turn off scientific notations for numbers
options(scipen = 999)  

##Set locale
Sys.setlocale("LC_ALL", "English") 

##Set seed for reproducibility
set.seed(2345)

##Load libraries
call <- function(x){
        if (!require(x,character.only = TRUE)){
                install.packages(x,dep=TRUE)
        }
}

##Use
call("adabag")
call("ada")
call("caret")
call("cluster")
call("data.table")
call("devtools")
call("doBy")
call("dplyr")
call("dummies")
call("flexclust")
call("fpc")
call("gdata")
call("ggplot2")
call("gmodels")
call("Hmisc")
call("knitr")
call("lattice")
call("neuralnet")
call("NbClust")
call("plotrix")
call("plotly")
call("psych")
call("rattle")
call("rCharts")
call("RColorBrewer")
call("reshape2")
call("rgl")
call("ROCR")
call("survival")

#Function to clean the data frame
clean <- function(df){
  #Clean the data names
  names(df) <-tolower(names(df))  
  names(df) <- gsub("\\.","",names(df))
  names(df) <- gsub("\\(","",names(df))
  names(df) <- gsub("\\)","",names(df))
  names(df) <- gsub("-","",names(df))
  names(df) <- gsub("_","",names(df))
  names(df) <- gsub(",","",names(df))
  return(df)
}

permwo = function(n, x) {
  ##Ordered without replacement
  return(factorial(n) / factorial(n-x))
}

permw = function(n, x) {
  ##Ordered with replacement
  return (n**x)
}

comb = function(n, x) {
  ##Unordered (sample or subset) Can use choose(n,x)
  return(factorial(n) / (factorial(x) * factorial(n-x)))
}


##Set data directory
##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
        datadir=('/home/bryan/GitHub/Data/')    
}else{
        datadir=('G:/Data/') 
}
