library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Hello Shiny!"),
  sidebarPanel(selectInput("Vector", "Select Mean of Distribution", c(0,1,2,3,4), selected = 0, multiple = TRUE)),
  mainPanel( plotOutput("main_plot"))
))