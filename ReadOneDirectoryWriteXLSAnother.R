library(dplyr)
rm(list=ls())
setwd("C:/Users/bryan_000/Downloads/Class - SEU/Raw")
file_list <- list.files()
library(gdata)  
library(WriteXLS)
for (file in file_list){
setwd("C:/Users/bryan_000/Downloads/Class - SEU/Raw")
dataset <- read.csv(file)
setwd("C:/Users/bryan_000/Downloads/Class - SEU/Processed")
writefile<-paste(file,"xls",sep = ".")

WriteXLS("dataset", ExcelFileName = writefile, SheetNames = NULL, perl ="C:/Strawberry/perl/bin/perl.exe",
         verbose = FALSE, Encoding = c("UTF-8", "latin1"),
         row.names = FALSE, col.names = TRUE,
         AdjWidth = FALSE, AutoFilter = FALSE, BoldHeaderRow = FALSE,
         FreezeRow = 0, FreezeCol = 0,
         envir = parent.frame())

}
      