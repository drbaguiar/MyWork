py <- plotly()
trace0 <- list(
  x = c(1, 2, 3, 4),
  y = c(10, 15, 13, 17)
)
trace1 <- list(
  x = c(1, 2, 3, 4),
  y = c(16, 5, 11, 9)
)
response <- py$plotly(trace0, trace1, kwargs=list(filename="basic-line", fileopt="overwrite"))

response$url
