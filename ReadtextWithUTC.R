##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}

#datafile <-paste(datadir,"LaptopSales.txt",sep = "")
datafile <-paste(datadir,"meteors.CSV",sep = "")
tbl <-read.csv(datafile)

#call("corrgram")
#tbl <- read.delim(datafile, fileEncoding="UCS-2LE")

tbl<-clean(tbl)
#corrgram(tbl) 
names(tbl) <- gsub("_","",names(tbl))
