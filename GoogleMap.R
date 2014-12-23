##Use my standard openning including call function
source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')
call("googleVis")
#Set name of datafile
datafile <- paste(datadir,"undata.csv",sep = "")
##Data from http://data.un.org/Data.aspx?d=SOWC&f=inID:86
input<- read.csv(datafile)
select<- input[which(input$Subgroup=="Total 5-14"),]
select<- input[which(input$Subgroup=="Total 5-14 yr"),]
Map<- data.frame(select$Country.or.Area, select$Value)
names(Map)<- c("Country", "Percentage")
Geo=gvisGeoMap(Map, locationvar="Country", numvar="Percentage",options=list(height=350, dataMode='regions'))
plot(Geo)