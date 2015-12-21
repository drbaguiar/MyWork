library(ggplot2)
library(plotly)
p <- plot_ly(midwest, x = percollege, color = state, type = "box")
p

#Create online
#Sys.setenv("plotly_username"="drbaguiar")
#Sys.setenv("plotly_api_key"="oie4hxhc5i")
#p <- plot_ly(midwest, x = percollege, color = state, type = "box")
#plotly_POST(p, filename = "r-docs/midwest-boxplots", world_readable=TRUE)