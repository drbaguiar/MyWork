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
call("dummies")
call("flexclust")
call("gmodels")
call("rgl")
call("fpc")
call("plotrix")
call("plotly")
call("gdata")
call("neuralnet")

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
  return (df)
}

##Set seed for reproducibility
set.seed(2345)

##Set data directory
##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
        datadir=('/home/bryan/GitHub/Data/')    
}else{
        datadir=('G:/Data/') 
}
