library(dplyr)
rm(list=ls())
setwd("C:/GradeSheets")
file_list <- list.files()
library(gdata)  
for (file in file_list){
        # if the merged dataset doesn't exist, create it
        if (!exists("dataset")){
                dataset <- read.xls(file,perl="C:/Strawberry/perl/bin/perl.exe")
        }
        # if the merged dataset does exist, append to it
        if (exists("dataset")){
                temp_dataset <-read.xls(file,perl="C:/Strawberry/perl/bin/perl.exe")
                dataset<-rbind(dataset, temp_dataset)
                rm(temp_dataset)
        }
        
}

dataset$Total<-as.numeric(dataset$Total)

##Remove Names
dataset2 <- dataset[,3:5]

##Back up the data
write.csv(dataset, file = "MyDataComplete.csv")
write.csv(dataset2, file = "MyData.csv")

##Removethe full dataset
rm(dataset)

aggrades <- aggregate(Total~Course, data=dataset2,mean)

aggrades$Total <-round(aggrades$Total,0)

#Number of students in each course
nbrpercourse<-data.frame(table(dataset2$Course))

#Number of students (based on userid)
nbrstudents <-data.frame(table(dataset2$Username))

##Query
subset(dataset2, Username=="s130080499")
subset(dataset2, Course=="201202-ECN-500-1")
