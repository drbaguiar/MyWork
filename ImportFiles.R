##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}

setwd("G:/DataWorking/")

file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    ##Read text or csv files
    #dataset <- read.table(file, header=TRUE, sep=",")  
    ##Read xls file (need perl loaded)
    dataset <- read.xls(file,perl="C:/Strawberry/perl/bin/perl.exe")
  }
  
  # if the merged dataset does exist, append to it
  else if (exists("dataset")){
    ##Read text or csv files
    #temp_dataset <-read.table(file, header=TRUE, sep=",") 
    ##Read xls file (need perl loaded)
    temp_dataset <- read.xls(file,perl="C:/Strawberry/perl/bin/perl.exe")
        
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}
dataset <- clean(dataset)

##Back up the data combined data
datafile <-paste(datadir,"MyDataComplete.csv",sep = "")
write.csv(dataset, file = datafile)


# ##Removethe full dataset
# rm(dataset)
# 
# aggrades <- aggregate(Total~Course, data=dataset2,mean)
# 
# aggrades$Total <-round(aggrades$Total,0)
# 
# #Number of students in each course
# nbrpercourse<-data.frame(table(dataset2$Course))
# 
# #Number of students (based on userid)
# nbrstudents <-data.frame(table(dataset2$Username))
# 
# ##Query
# subset(dataset2, Username=="s130080499")
# subset(dataset2, Course=="201202-ECN-500-1")
