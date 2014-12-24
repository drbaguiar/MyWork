#Use my standard openning including call function
source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')
call("googleVis")
#Set name of datafile
datafile <- paste(datadir,"nygdp.csv",sep = "")
##Data from http://databank.worldbank.org/data/views/reports/tableview.aspx
input<- read.csv(datafile)
#select<- input[which(input$Subgroup=="Total 5-14"),]
#select<- input[which(input$Subgroup=="Total 5-14 yr"),]

Map<- data.frame(input$Country.Name, round(input$X2013/1000000000),2)
names(Map)<- c("Country", "GDP")
Geo=gvisGeoMap(Map, locationvar="Country", numvar="GDP",options=list(height=350, dataMode='regions'))
plot(Geo)