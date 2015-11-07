# Install these packages before you hit Source
library(car)      # For normality plots
library(moments)  # For skew calculations
library(lattice)  # Use trellis graphics
library(lubridate)

# Read in the dataset
Stocks <- read.csv("D:/Data/FourStocks.csv", header=TRUE)

#Prints means
print(colMeans(Stocks[ , 2:5]))

#Prints STD
print(apply(Stocks[ , 2:5], MARGIN=2, FUN=sd))


#Uses moments library for skew
print(skewness(Stocks[ , 2:5]))
hist(Stocks$Pharma)
hist(Stocks$FMCG)
hist(Stocks$Power)
hist(Stocks$Steel)

# Format the Date field based on what is 
# contained in the Date column
Stocks$Date <- as.Date(Stocks$Date, "%d-%b-%y")

# Capture the month and year data in different columns. 
# The "accessor" functions are courtesy the lubridate library!
Stocks$Month <- month(Stocks$Date)
Stocks$Year <- year(Stocks$Date)

# View the newly formatted data
View(Stocks)

# Write the contents of Stocks to a file
write.csv(Stocks, "D:/Data/NewStocks.csv")
cat("Created NewStocks.csv in",
    getwd(), "\n",
    "You may open it as a spreadsheet!\n")

# Here's an easy way to construct a table
# of average values by month
monthYearAvePharma <- aggregate(Pharma ~ Month + Year,data=Stocks,FUN=mean)
View(monthYearAvePharma)

# Here's an easy way to construct a table
# of average values by month
monthYearAveFMCG <- aggregate(FMCG ~ Month + Year,data=Stocks,FUN=mean)
View(monthYearAveFMCG)

# Here's an easy way to construct a table
# of average values by month
monthYearAvePower <- aggregate(Power ~ Month + Year,data=Stocks,FUN=mean)
View(monthYearAvePower)

# Here's an easy way to construct a table
# of average values by month
monthYearAveStell <- aggregate(Steel ~ Month + Year,data=Stocks,FUN=mean)
View(monthYearAveStell)

