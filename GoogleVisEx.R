source('C:/GitHub/MyWork/StdOpen.R')
library(googleVis)
op <- options(gvis.plot.tag = "chart")

CityPopularity$Mean = mean(CityPopularity$Popularity)
cc <- gvisComboChart(CityPopularity, xvar = "City", yvar = c("Mean", "Popularity"), 
                     options = list(seriesType = "bars", width = 450, height = 300, title = "City Popularity", 
                                    series = "{1:{type:\"line\"}}"))
plot(cc)

