library(shiny)
shinyServer(function(input, output) {
  v<- function() {
    return(rnorm(100,mean=as.numeric(input$Vector)))  
  }
  output$main_plot <- 
    renderPlot( 
      hist(v(), breaks=10, xlab="",
           main="Histogram of 100 Samples\n taken from: N[mean, sd=1]")) 
})