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
call("prob")
call("psych")
call("rattle")
call("rCharts")
call("RColorBrewer")
call("reshape2")
call("rgl")
call("ROCR")
call("SixSigma")
call("survival")

##Set data directory
if (Sys.info()["sysname"]=="Linux"){
        datadir=('/home/bryan/GitHub/Data/')    
}else{
        datadir=('G:/Data/') 
}

##Use my standard openning2
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen2.R')   
}