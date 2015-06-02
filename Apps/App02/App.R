library(shiny)
library(rUnemploymentData)
data(df_county_unemployment)

ui <- fluidPage(
  selectInput("num", 
              label = "Choose a Year",
              choices = c(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013),
              selected = 2000),
    plotOutput("map")
  
  )

server <- function(input, output) {
  output$map <- renderPlot({
    county_unemployment_choropleth(year=as.numeric(input$num))
  })
 

}

shinyApp(ui = ui, server = server)

