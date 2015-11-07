# Install these packages before you hit Source
library(car)      # For normality plots
library(moments)  # For skew calculations
library(lattice)  # Use trellis graphics
library(lubridate)

# Read in the dataset
Codes<- read.csv("D:/Data/PinCodes.csv", header=TRUE)

# Find offices associated with a specific pin
print(Codes$officename[which(Codes$pincode==855108)])

# To extract a digit we use the function 
Leading.Digit<-substring(Codes$pincode,1, 1)

table(Leading.Digit)

barplot(table(Leading.Digit),legend.text = '123')


# Read in the dataset
Census<- read.csv("D:/Data/CensusIncomeForR.csv", header=TRUE)
# Grab the samples from the top row
Sample100 <- Census[1:100, ]
Sample400 <- Census[1:400, ]

# Place 3 plots side by side
par(mfrow=c(1, 3)) 

# Gender distributions
lbls <- c("Female", "Male")

# Pie charts galore!
pie(table(Sample100$Sex), labels=lbls)
pie(table(Sample400$Sex), labels=lbls)
pie(table(Census$Sex), labels=lbls)

par(mfrow=c(1, 3))
hist(runif(100,1, 10), breaks=20, col=colors()[30:50],  xlab="Number")
hist(runif(1000,1, 10), breaks=20, col=colors()[30:50], xlab="Number")
hist(runif(10000,1, 10), breaks=20, col=colors()[30:50], xlab="Number")