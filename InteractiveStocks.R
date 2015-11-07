# This is a library that comes shipped with RStudio, 
# and facilitates interactive plotting
# 
# NOTE: Please don't attempt to install this!
library(manipulate)

# Source what you have created earlier
# Make sure all of your variables are declared
source("~/GitHub/MyWork/FourStocks.R")

# If you encounter any errors, this is due to
# some of the steps not being executed correctly!

# Reset the plotting arrangement
par(mfrow=c(1,1))

# Function to plot the time series of all sectors
# from the indicated start year to the end year
windowAll <- function(startYear, endYear) {
  
  ts.plot(window(ts.PharmaAvg,
                 start=c(startYear, 1),
                 end=c(endYear, 12)),
          window(ts.FMCGAvg,
                 start=c(startYear, 1),
                 end=c(endYear, 12)),
          window(ts.PowerAvg,
                 start=c(startYear, 1),
                 end=c(endYear, 12)),
          window(ts.SteelAvg,
                 start=c(startYear, 1),
                 end=c(endYear, 12)),
          main=paste("Stocks from", startYear,
                     "through", endYear),
          gpars=list(xlab="Year",
                     ylab="Stock Price",
                     col=c("darkred", "navy",
                           "darkgreen", "darkorange"),
                     lwd=2))
  legend("topleft",
         bty="n",
         c("Pharma", "FMCG", "Power", "Steel"),
         lty=c(1, 1, 1, 1),
         lwd=c(2, 2, 2, 2),
         col=c("darkred", "navy", 
               "darkgreen", "darkorange"))
}

# Plot a time series of adjusted close prices 
# between lowYr and upYr. Interact with it.
# What do you notice?
lowYr <- 1995; upYr <- 2014
manipulate(
  windowAll(lowYr, upYr),
  lowYr = slider(1995, 2014,
                 step=1,
                 initial=1995),
  upYr = slider(lowYr, 2014,
                step=1,
                initial=2014))