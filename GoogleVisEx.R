##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}

call("googleVis")
op <- options(gvis.plot.tag = "chart")

CityPopularity$Mean = mean(CityPopularity$Popularity)
cc <- gvisComboChart(CityPopularity, xvar = "City", yvar = c("Mean", "Popularity"), 
                     options = list(seriesType = "bars", width = 450, height = 300, title = "City Popularity", 
                                    series = "{1:{type:\"line\"}}"))
plot(cc)