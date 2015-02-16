##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}
datafile <-paste(datadir,"LaptopSales.txt",sep = "")

call("corrgram")
tbl <- read.delim(datafile, fileEncoding="UCS-2LE")
corrgram(tbl) 