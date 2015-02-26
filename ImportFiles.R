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
    if ((length(Sys.glob("*.txt") > 0)) || (length(Sys.glob("*.TXT") > 0))){
      ##Read text or csv files
      dataset <- read.table(file, header=TRUE, sep=",")  
    }
    else if ((length(Sys.glob("*.csv") > 0)) || (length(Sys.glob("*.CSV") > 0))){
      ##Read text or csv files
      dataset <- read.table(file, header=TRUE, sep=",")  
    }
    else if ((length(Sys.glob("*.xls") > 0)) || (length(Sys.glob("*.XLS") > 0))){
      ##Read xls file (need perl loaded)
      dataset <- read.xls(file,perl="C:/Strawberry/perl/bin/perl.exe")
      
    }
  }
  
  # if the merged dataset does exist, append to it
  else if (exists("dataset")){
    if ((length(Sys.glob("*.txt") > 0)) || (length(Sys.glob("*.TXT") > 0))){
      ##Read text or csv files
      temp_dataset <- read.table(file, header=TRUE, sep=",") 
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
    else if ((length(Sys.glob("*.csv") > 0)) || (length(Sys.glob("*.CSV") > 0))){
      ##Read text or csv files
      temp_dataset <- read.table(file, header=TRUE, sep=",")  
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
    else if((length(Sys.glob("*.xls") > 0)) || (length(Sys.glob("*.XLS") > 0))){
      ##Read xls file (need perl loaded)
      temp_dataset <- read.xls(file,perl="C:/Strawberry/perl/bin/perl.exe")
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  
}
dataset <- clean(dataset)

##Back up the data combined data
datafile <-paste(datadir,"MyDataComplete.csv",sep = "")
write.csv(dataset, file = datafile)