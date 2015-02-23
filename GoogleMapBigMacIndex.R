##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}

#call("googleVis")
suppressPackageStartupMessages(library(googleVis))
#Set name of datafile
datafile <- paste(datadir,"bigmacindex.csv",sep = "")
##Data from http://www.economist.com/content/big-mac-index
input<- read.csv(datafile)
#select<- input[which(input$Subgroup=="Total 5-14"),]
#select<- input[which(input$Subgroup=="Total 5-14 yr"),]

Map<- data.frame(input$Country, input$dollar_price)
names(Map)<- c("Country", "Price")
Geo=gvisGeoMap(Map, locationvar="Country", numvar="Price",options=list(height=350, dataMode='regions'))
plot(Geo)