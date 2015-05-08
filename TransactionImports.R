# Clear the environment
rm(list=ls())

# Turn off scientific notations for numbers
options(scipen = 999)  

# Set locale
Sys.setlocale("LC_ALL", "English") 

# Set seed for reproducibility
set.seed(2345)

# Load the libraries
library(arules)
library(arulesViz)

# Load the Data
data(Groceries)
df <- read.csv("D:/Data/groceries.csv", header=FALSE)
trans <- as(df, "transactions")
# inspect(trans)


# Create an item frequency plot for the top 20 items
itemFrequencyPlot(trans,topN=20,type="absolute")
itemFrequencyPlot(Groceries,topN=20,type="absolute")

