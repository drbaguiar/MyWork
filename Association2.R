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
data(Adult)

## Mine itemsets with minimum support of 0.1.
fsets <- eclat(Adult, parameter = list(supp = 0.5))

## Display the 5 itemsets with the highest support.
fsets.top5<-sort(fsets)[1:5]
inspect(fsets.top5)

## Get the itemsets as a list
as(items(fsets.top5), "list")


## Get the itemsets as a binary matrix
bm<-as(items(fsets.top5), "matrix")

## Get the itemsets as a sparse matrix, a ngCMatrix from package Matrix.
## Warning: for efficiency reasons, the ngCMatrix you get is transposed
f<-as(items(fsets.top5), "ngCMatrix")
image(f)
