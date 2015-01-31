##Use my standard openning including call function
source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')
data=paste(datadir,"meteors.csv",sep = "")
train <- read.csv(data, sep = ",", na.strings = c("", "NA"))
train$latlong <-paste(train$latitude,train$longitude,sep = ":")
train <-subset(train, type_of_meteorite =="Iron")
call("googleVis")
call("shiny")

plotData<-data.frame(name=train$place,latLong=train$latlong)
rm (train)
sites <- gvisMap(plotData,locationvar="latLong",tipvar="name", 
                 options=list(displayMode = "Markers", mapType='normal', colorAxis = "{colors:['red', 'grey']}",
                              useMapTypeControl=TRUE, enableScrollWheel='TRUE'))
plot(sites)
