# Analyze 0/1 matrix from R-Files directory
# This path just works on Jim's computer -- tailor to your computer by changing intro path
# This is for a data file in which rows are cases of within-case data
# The input file should have row and column labels and be a tab delimited text file
# There should be no missing data
# The R syntax for getting a file on a PC uses a path like this: 
# "C:\\Documents and Settings\\jimlewis\\My Documents\\R-Files\\table501.txt"
#
analyze.01matrix.fromfile <- function(filename) {
f <- filename
data <- read.table(f,header=TRUE,row.names=1)
rows <- nrow(data)
cols <- ncol(data)
cells <- rows * cols
cellsum <- 0
colsum <- 0
Nonce <- 0
N <- cols
n <- rows
x <- 1
y <- 1
while (y <= cols) {
   while (x <= rows) {
   cellsum <- cellsum + data[x,y]
   colsum <- colsum + data[x,y]
   x <- x+1
   }
y <- y+1
x <- 1
if (colsum == 1) Nonce = Nonce + 1
colsum <- 0
}
pest <- cellsum/cells
GTadj <- Nonce/N
pGT <- pest/(1+GTadj)
pdef <- (pest - 1/n)*(1 - 1/n)
padj <- (pGT + pdef)/2
discoverysofar <- 1 - (1-padj)^n
problemsavailablefordiscovery <- N/discoverysofar
estimatedremaining <- problemsavailablefordiscovery - N
cat("\nRESULTS\n\n")
cat("Number of participants:",n,"\n")
cat("Number of known problems:",N,"\n")
cat("Number of known problems that occurred just once:",Nonce,"\n")
cat("Observed p:",pest,"\n")
cat("pdef:",pdef,"\n")
cat("pGT:",pGT,"\n")
cat("Adjusted p:",padj,"\n")
cat("Estimated proportion of discovery (so far):",discoverysofar,"\n")
cat("Estimated number of problems available for discovery:",ceiling(problemsavailablefordiscovery),"\n")
cat("Estimated number of undiscovered problems:",ceiling(estimatedremaining),"\n")
cat("\n")
}

# Analyze 0/1 matrix from Jim's Web directory
# Get data from a stored matrix of data from Jim's website
# This is for a data file in which rows are cases of within-case data
# The input file should have row and column labels and be a tab delimited text file
# There should be no missing data
#
analyze.01matrix.fromweb <- function(filename) {
intro <- "http://drjim.0catch.com/PracStatPack/"
f <- paste(intro,filename,sep="")
data <- read.table(f,header=TRUE,row.names=1)
rows <- nrow(data)
cols <- ncol(data)
cells <- rows * cols
cellsum <- 0
colsum <- 0
Nonce <- 0
N <- cols
n <- rows
x <- 1
y <- 1
while (y <= cols) {
   while (x <= rows) {
   cellsum <- cellsum + data[x,y]
   colsum <- colsum + data[x,y]
   x <- x+1
   }
y <- y+1
x <- 1
if (colsum == 1) Nonce = Nonce + 1
colsum <- 0
}
pest <- cellsum/cells
GTadj <- Nonce/N
pGT <- pest/(1+GTadj)
pdef <- (pest - 1/n)*(1 - 1/n)
padj <- (pGT + pdef)/2
discoverysofar <- 1 - (1-padj)^n
problemsavailablefordiscovery <- N/discoverysofar
estimatedremaining <- problemsavailablefordiscovery - N
cat("\nRESULTS\n\n")
cat("Number of participants:",n,"\n")
cat("Number of known problems:",N,"\n")
cat("Number of known problems that occurred just once:",Nonce,"\n")
cat("Observed p:",pest,"\n")
cat("pdef:",pdef,"\n")
cat("pGT:",pGT,"\n")
cat("Adjusted p:",padj,"\n")
cat("Estimated proportion of discovery (so far):",discoverysofar,"\n")
cat("Estimated number of problems available for discovery:",ceiling(problemsavailablefordiscovery),"\n")
cat("Estimated number of undiscovered problems:",ceiling(estimatedremaining),"\n")
cat("\n")
}

# Analyze PSSUQ matrix from R-Files directory (only for Version 3, with 16 items)
# This path just works on Jim's computer -- tailor to your computer by changing intro path
# This is for a data file in which rows are cases of within-case data
# Also need to provide desired level for confidence intervals (e.g., .95 or 95 for 95% confidence)
# The input file should have row and column labels and be a tab delimited text file
# Any missing data should be denoted NA and will be ignored when computing means
# Sysuse is Items 1-6; InfoQual is Items 7-12; IntQual is Items 13-15
# If an entire scale is missing, delete the case from the input file before running this function
# The R syntax for getting a file on a PC uses a path like this: 
# "C:\\Documents and Settings\\jimlewis\\My Documents\\R-Files\\table501.txt"
#
analyze.pssuq.fromfile <- function(filename,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
f <- filename
origdata <- read.table(f,header=TRUE,row.names=1)
data <- origdata
attach(data)
rows <- nrow(data)
cols <- ncol(data)
n <- rows
x <- 1
y <- 1
# PSSUQ Version 3 Norms from Lewis (2002, 2012), Sauro & Lewis (2012)
# Values for norms are 99% lower limit, mean, 99% uppper limit for each item 1-16; 
# 17-20 are for scales, respectively, SysUse, InfoQual, IntQual, Overall
pssuqnorms <- array(0,c(3,20))
pssuqnorms[,1] <- c(2.6,2.85,3.09)
pssuqnorms[,2] <- c(2.45,2.69,2.93)
pssuqnorms[,3] <- c(2.86,3.16,3.45)
pssuqnorms[,4] <- c(2.40,2.66,2.91)
pssuqnorms[,5] <- c(2.07,2.27,2.48)
pssuqnorms[,6] <- c(2.54,2.86,3.17)
pssuqnorms[,7] <- c(3.36,3.70,4.05)
pssuqnorms[,8] <- c(2.93,3.21,3.49)
pssuqnorms[,9] <- c(2.65,2.96,3.27)
pssuqnorms[,10] <- c(2.79,3.09,3.38)
pssuqnorms[,11] <- c(2.46,2.74,3.01)
pssuqnorms[,12] <- c(2.41,2.66,2.92)
pssuqnorms[,13] <- c(2.06,2.28,2.49)
pssuqnorms[,14] <- c(2.18,2.42,2.66)
pssuqnorms[,15] <- c(2.51,2.79,3.07)
pssuqnorms[,16] <- c(2.55,2.80,3.02)
pssuqnorms[,17] <- c(2.57,2.80,3.02)
pssuqnorms[,18] <- c(2.79,3.02,3.24)
pssuqnorms[,19] <- c(2.28,2.49,2.71)
pssuqnorms[,20] <- c(2.62,2.82,3.02)
# Building item/scale arrays
itemarray <- array(0,c(n,20))
while (y <= cols) {
   while (x <= rows) {
   itemarray[x,y] <- data[x,y]
   x <- x+1
   }
y <- y+1
x <- 1
}
while (x <= rows) {
   itemarray[x,17] <- mean(itemarray[x,1:6], na.rm=TRUE)
   itemarray[x,18] <- mean(itemarray[x,7:12], na.rm=TRUE)
   itemarray[x,19] <- mean(itemarray[x,13:15], na.rm=TRUE)
   itemarray[x,20] <- mean(itemarray[x,1:16], na.rm=TRUE)
   x <- x+1
}
# Confidence intervals for items/scales
y <- 1
x <- 1
itemconf <- array(0,c(3,20))
itemsderr <- array(0,c(20))
d <- array(0,c(20))
u <- array(0,c(20))
upper <- array(0,c(20))
lower <- array(0,c(20))
df = n-1
t <- abs(qt((1-conf)/2,df))
while (y <= 20) {
   itemsderr[y] <- sd(itemarray[,y], na.rm=TRUE)/n^.5
   d[y] <- t*itemsderr[y]
   u[y] <- mean(itemarray[,y], na.rm=TRUE)
   lower[y] <- u[y]-d[y]
   upper[y] <- u[y]+d[y]
   itemconf[1,y] <- lower[y]
   itemconf[2,y] <- u[y]
   itemconf[3,y] <- upper[y]
   y <- y+1
   }
# Comparison of observed confidence intervals with norms
y <- 1
x <- 1
compci <- array("",c(20))
while (y <= 20) {
   if (itemconf[3,y] < pssuqnorms[1,y]) compci[y] <- "interval lower than norm"
   if (itemconf[1,y] > pssuqnorms[3,y]) compci[y] <- "interval higher than norm"
   if (itemconf[3,y] >= pssuqnorms[1,y] && itemconf[1,y] <= pssuqnorms[3,y]) compci[y] <- "interval consistent with norm"
   y <- y+1
   }
# Comparison of observed means with norms
y <- 1
x <- 1
compmean <- array("",c(20))
while (y <= 20) {
   if (itemconf[2,y] < pssuqnorms[1,y]) compmean[y] <- "Mean lower than norm;"
   if (itemconf[2,y] > pssuqnorms[3,y]) compmean[y] <- "Mean higher than norm;"
   if (itemconf[2,y] >= pssuqnorms[1,y] && itemconf[1,y] <= pssuqnorms[3,y]) compmean[y] <- "Mean consistent with norm;"
   y <- y+1
   }
# Print the results
cat("RESULTS OF PSSUQ ANALYSIS \n\n")
cat("Scales \n\n")
cat("Overall mean:",format(itemconf[2,20],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,20],digits=3),"-",format(itemconf[3,20],digits=3),"\t  Comparisons:",compmean[20],compci[20],"\n")
cat("SysUse  mean:",format(itemconf[2,17],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,17],digits=3),"-",format(itemconf[3,17],digits=3),"\t  Comparisons:",compmean[17],compci[17],"\n")
cat("InfoQual mean:",format(itemconf[2,18],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,18],digits=3),"-",format(itemconf[3,18],digits=3),"\t  Comparisons:",compmean[18],compci[18],"\n")
cat("IntQual mean:",format(itemconf[2,19],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,19],digits=3),"-",format(itemconf[3,19],digits=3),"\t  Comparisons:",compmean[19],compci[19],"\n\n")
cat("Items \n\n")
cat("Item 01 mean:",format(itemconf[2,1],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,1],digits=3),"-",format(itemconf[3,1],digits=3),"\t  Comparisons:",compmean[1],compci[1],"\n")
cat("Item 02 mean:",format(itemconf[2,2],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,2],digits=3),"-",format(itemconf[3,2],digits=3),"\t  Comparisons:",compmean[2],compci[2],"\n")
cat("Item 03 mean:",format(itemconf[2,3],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,3],digits=3),"-",format(itemconf[3,3],digits=3),"\t  Comparisons:",compmean[3],compci[3],"\n")
cat("Item 04 mean:",format(itemconf[2,4],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,4],digits=3),"-",format(itemconf[3,4],digits=3),"\t  Comparisons:",compmean[4],compci[4],"\n")
cat("Item 05 mean:",format(itemconf[2,5],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,5],digits=3),"-",format(itemconf[3,5],digits=3),"\t  Comparisons:",compmean[5],compci[5],"\n")
cat("Item 06 mean:",format(itemconf[2,6],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,6],digits=3),"-",format(itemconf[3,6],digits=3),"\t  Comparisons:",compmean[6],compci[6],"\n")
cat("Item 07 mean:",format(itemconf[2,7],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,7],digits=3),"-",format(itemconf[3,7],digits=3),"\t  Comparisons:",compmean[7],compci[7],"\n")
cat("Item 08 mean:",format(itemconf[2,8],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,8],digits=3),"-",format(itemconf[3,8],digits=3),"\t  Comparisons:",compmean[8],compci[8],"\n")
cat("Item 09 mean:",format(itemconf[2,9],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,9],digits=3),"-",format(itemconf[3,9],digits=3),"\t  Comparisons:",compmean[9],compci[9],"\n")
cat("Item 10 mean:",format(itemconf[2,10],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,10],digits=3),"-",format(itemconf[3,10],digits=3),"\t  Comparisons:",compmean[10],compci[10],"\n")
cat("Item 11 mean:",format(itemconf[2,11],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,11],digits=3),"-",format(itemconf[3,11],digits=3),"\t  Comparisons:",compmean[11],compci[11],"\n")
cat("Item 12 mean:",format(itemconf[2,12],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,12],digits=3),"-",format(itemconf[3,12],digits=3),"\t  Comparisons:",compmean[12],compci[12],"\n")
cat("Item 13 mean:",format(itemconf[2,13],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,13],digits=3),"-",format(itemconf[3,13],digits=3),"\t  Comparisons:",compmean[13],compci[13],"\n")
cat("Item 14 mean:",format(itemconf[2,14],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,14],digits=3),"-",format(itemconf[3,14],digits=3),"\t  Comparisons:",compmean[14],compci[14],"\n")
cat("Item 15 mean:",format(itemconf[2,15],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,15],digits=3),"-",format(itemconf[3,15],digits=3),"\t  Comparisons:",compmean[15],compci[15],"\n")
cat("Item 16 mean:",format(itemconf[2,16],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,16],digits=3),"-",format(itemconf[3,16],digits=3),"\t  Comparisons:",compmean[16],compci[16],"\n\n")
}

# Analyze PSSUQ matrix from Jim's Web directory (only for Version 3, with 16 items)
# This is for a data file in which rows are cases of within-case data
# Also need to provide desired level for confidence intervals (e.g., .95 or 95 for 95% confidence)
# The input file should have row and column labels and be a tab delimited text file
# Any missing data should be denoted NA and will be ignored when computing means
# Sysuse is Items 1-6; InfoQual is Items 7-12; IntQual is Items 13-15
# If an entire scale is missing, delete the case from the input file before running this function
#
analyze.pssuq.fromweb <- function(filename,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
intro <- "http://drjim.0catch.com/PracStatPack/"
f <- paste(intro,filename,sep="")
origdata <- read.table(f,header=TRUE,row.names=1)
data <- origdata
attach(data)
rows <- nrow(data)
cols <- ncol(data)
n <- rows
x <- 1
y <- 1
# PSSUQ Version 3 Norms from Lewis (2002, 2012), Sauro & Lewis (2012)
# Values for norms are 99% lower limit, mean, 99% uppper limit for each item 1-16; 
# 17-20 are for scales, respectively, SysUse, InfoQual, IntQual, Overall
pssuqnorms <- array(0,c(3,20))
pssuqnorms[,1] <- c(2.6,2.85,3.09)
pssuqnorms[,2] <- c(2.45,2.69,2.93)
pssuqnorms[,3] <- c(2.86,3.16,3.45)
pssuqnorms[,4] <- c(2.40,2.66,2.91)
pssuqnorms[,5] <- c(2.07,2.27,2.48)
pssuqnorms[,6] <- c(2.54,2.86,3.17)
pssuqnorms[,7] <- c(3.36,3.70,4.05)
pssuqnorms[,8] <- c(2.93,3.21,3.49)
pssuqnorms[,9] <- c(2.65,2.96,3.27)
pssuqnorms[,10] <- c(2.79,3.09,3.38)
pssuqnorms[,11] <- c(2.46,2.74,3.01)
pssuqnorms[,12] <- c(2.41,2.66,2.92)
pssuqnorms[,13] <- c(2.06,2.28,2.49)
pssuqnorms[,14] <- c(2.18,2.42,2.66)
pssuqnorms[,15] <- c(2.51,2.79,3.07)
pssuqnorms[,16] <- c(2.55,2.80,3.02)
pssuqnorms[,17] <- c(2.57,2.80,3.02)
pssuqnorms[,18] <- c(2.79,3.02,3.24)
pssuqnorms[,19] <- c(2.28,2.49,2.71)
pssuqnorms[,20] <- c(2.62,2.82,3.02)
# Building item/scale arrays
itemarray <- array(0,c(n,20))
while (y <= cols) {
   while (x <= rows) {
   itemarray[x,y] <- data[x,y]
   x <- x+1
   }
y <- y+1
x <- 1
}
while (x <= rows) {
   itemarray[x,17] <- mean(itemarray[x,1:6], na.rm=TRUE)
   itemarray[x,18] <- mean(itemarray[x,7:12], na.rm=TRUE)
   itemarray[x,19] <- mean(itemarray[x,13:15], na.rm=TRUE)
   itemarray[x,20] <- mean(itemarray[x,1:16], na.rm=TRUE)
   x <- x+1
}
# Confidence intervals for items/scales
y <- 1
x <- 1
itemconf <- array(0,c(3,20))
itemsderr <- array(0,c(20))
d <- array(0,c(20))
u <- array(0,c(20))
upper <- array(0,c(20))
lower <- array(0,c(20))
df = n-1
t <- abs(qt((1-conf)/2,df))
while (y <= 20) {
   itemsderr[y] <- sd(itemarray[,y], na.rm=TRUE)/n^.5
   d[y] <- t*itemsderr[y]
   u[y] <- mean(itemarray[,y], na.rm=TRUE)
   lower[y] <- u[y]-d[y]
   upper[y] <- u[y]+d[y]
   itemconf[1,y] <- lower[y]
   itemconf[2,y] <- u[y]
   itemconf[3,y] <- upper[y]
   y <- y+1
   }
# Comparison of observed confidence intervals with norms
y <- 1
x <- 1
compci <- array("",c(20))
while (y <= 20) {
   if (itemconf[3,y] < pssuqnorms[1,y]) compci[y] <- "interval lower than norm"
   if (itemconf[1,y] > pssuqnorms[3,y]) compci[y] <- "interval higher than norm"
   if (itemconf[3,y] >= pssuqnorms[1,y] && itemconf[1,y] <= pssuqnorms[3,y]) compci[y] <- "interval consistent with norm"
   y <- y+1
   }
# Comparison of observed means with norms
y <- 1
x <- 1
compmean <- array("",c(20))
while (y <= 20) {
   if (itemconf[2,y] < pssuqnorms[1,y]) compmean[y] <- "Mean lower than norm;"
   if (itemconf[2,y] > pssuqnorms[3,y]) compmean[y] <- "Mean higher than norm;"
   if (itemconf[2,y] >= pssuqnorms[1,y] && itemconf[1,y] <= pssuqnorms[3,y]) compmean[y] <- "Mean consistent with norm;"
   y <- y+1
   }
# Print the results
cat("RESULTS OF PSSUQ ANALYSIS \n\n")
cat("Scales \n\n")
cat("Overall mean:",format(itemconf[2,20],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,20],digits=3),"-",format(itemconf[3,20],digits=3),"\t  Comparisons:",compmean[20],compci[20],"\n")
cat("SysUse  mean:",format(itemconf[2,17],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,17],digits=3),"-",format(itemconf[3,17],digits=3),"\t  Comparisons:",compmean[17],compci[17],"\n")
cat("InfoQual mean:",format(itemconf[2,18],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,18],digits=3),"-",format(itemconf[3,18],digits=3),"\t  Comparisons:",compmean[18],compci[18],"\n")
cat("IntQual mean:",format(itemconf[2,19],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,19],digits=3),"-",format(itemconf[3,19],digits=3),"\t  Comparisons:",compmean[19],compci[19],"\n\n")
cat("Items \n\n")
cat("Item 01 mean:",format(itemconf[2,1],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,1],digits=3),"-",format(itemconf[3,1],digits=3),"\t  Comparisons:",compmean[1],compci[1],"\n")
cat("Item 02 mean:",format(itemconf[2,2],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,2],digits=3),"-",format(itemconf[3,2],digits=3),"\t  Comparisons:",compmean[2],compci[2],"\n")
cat("Item 03 mean:",format(itemconf[2,3],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,3],digits=3),"-",format(itemconf[3,3],digits=3),"\t  Comparisons:",compmean[3],compci[3],"\n")
cat("Item 04 mean:",format(itemconf[2,4],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,4],digits=3),"-",format(itemconf[3,4],digits=3),"\t  Comparisons:",compmean[4],compci[4],"\n")
cat("Item 05 mean:",format(itemconf[2,5],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,5],digits=3),"-",format(itemconf[3,5],digits=3),"\t  Comparisons:",compmean[5],compci[5],"\n")
cat("Item 06 mean:",format(itemconf[2,6],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,6],digits=3),"-",format(itemconf[3,6],digits=3),"\t  Comparisons:",compmean[6],compci[6],"\n")
cat("Item 07 mean:",format(itemconf[2,7],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,7],digits=3),"-",format(itemconf[3,7],digits=3),"\t  Comparisons:",compmean[7],compci[7],"\n")
cat("Item 08 mean:",format(itemconf[2,8],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,8],digits=3),"-",format(itemconf[3,8],digits=3),"\t  Comparisons:",compmean[8],compci[8],"\n")
cat("Item 09 mean:",format(itemconf[2,9],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,9],digits=3),"-",format(itemconf[3,9],digits=3),"\t  Comparisons:",compmean[9],compci[9],"\n")
cat("Item 10 mean:",format(itemconf[2,10],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,10],digits=3),"-",format(itemconf[3,10],digits=3),"\t  Comparisons:",compmean[10],compci[10],"\n")
cat("Item 11 mean:",format(itemconf[2,11],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,11],digits=3),"-",format(itemconf[3,11],digits=3),"\t  Comparisons:",compmean[11],compci[11],"\n")
cat("Item 12 mean:",format(itemconf[2,12],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,12],digits=3),"-",format(itemconf[3,12],digits=3),"\t  Comparisons:",compmean[12],compci[12],"\n")
cat("Item 13 mean:",format(itemconf[2,13],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,13],digits=3),"-",format(itemconf[3,13],digits=3),"\t  Comparisons:",compmean[13],compci[13],"\n")
cat("Item 14 mean:",format(itemconf[2,14],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,14],digits=3),"-",format(itemconf[3,14],digits=3),"\t  Comparisons:",compmean[14],compci[14],"\n")
cat("Item 15 mean:",format(itemconf[2,15],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,15],digits=3),"-",format(itemconf[3,15],digits=3),"\t  Comparisons:",compmean[15],compci[15],"\n")
cat("Item 16 mean:",format(itemconf[2,16],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,16],digits=3),"-",format(itemconf[3,16],digits=3),"\t  Comparisons:",compmean[16],compci[16],"\n\n")
}

# Analyze SUS matrix from R-Files directory (standard version of SUS with varied tone, raw untransformed data)
# This path just works on Jim's computer -- tailor to your computer by changing intro path
# This is for a data file in which rows are cases of within-case data
# Also need to provide desired level for confidence intervals as proportion or percentage (e.g., .95 or 95 for 95% confidence)
# The input file should have row and column labels and be a tab delimited text file
# Missing data should be denoted NA and the entire case will be deleted
# The R syntax for getting a file on a PC uses a path like this: 
# "C:\\Documents and Settings\\jimlewis\\My Documents\\R-Files\\table501.txt"
#
analyze.sus.fromfile <- function(filename,conf) {
f <- filename
origdata <- read.table(f,header=TRUE,row.names=1)
data <- na.omit(origdata)
norig <- nrow(origdata)
nleft <- nrow(data)
nmissing <- norig - nleft
attach(data)
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
rows <- nrow(data)
cols <- ncol(data)
n <- rows
x <- 1
y <- 1
# Defining odd and even items
oddeven <- array("",c(10))
oddeven[1] <- "odd"
oddeven[2] <- "even"
oddeven[3] <- "odd"
oddeven[4] <- "even"
oddeven[5] <- "odd"
oddeven[6] <- "even"
oddeven[7] <- "odd"
oddeven[8] <- "even"
oddeven[9] <- "odd"
oddeven[10] <- "even"
# Building item/scale arrays and recoding scores
itemarray <- array(0,c(n,13))
while (y <= cols) {
   while (x <= rows) {
   if (oddeven[y] == "odd") itemarray[x,y] <- data[x,y] - 1
   if (oddeven[y] == "even") itemarray[x,y] <- 5 - data[x,y]
   x <- x+1
   }
y <- y+1
x <- 1
}
while (x <= rows) {
   itemarray[x,11] <- (sum(itemarray[x,1:3]) + sum(itemarray[x,5:9]))*3.125
   itemarray[x,12] <- (sum(itemarray[x,4]) + sum(itemarray[x,10]))*12.5
   itemarray[x,13] <- sum(itemarray[x,1:10])*2.5
   x <- x+1
}
# Confidence intervals for items/scales
y <- 1
x <- 1
itemconf <- array(0,c(3,13))
itemsderr <- array(0,c(13))
d <- array(0,c(13))
u <- array(0,c(13))
upper <- array(0,c(13))
lower <- array(0,c(13))
df = n-1
t <- abs(qt((1-conf)/2,df))
while (y <= 13) {
   itemsderr[y] <- sd(itemarray[,y])/n^.5
   d[y] <- t*itemsderr[y]
   u[y] <- mean(itemarray[,y])
   lower[y] <- u[y]-d[y]
   upper[y] <- u[y]+d[y]
   itemconf[1,y] <- lower[y]
   itemconf[2,y] <- u[y]
   itemconf[3,y] <- upper[y]
   y <- y+1
   }
# SUS Percentiles and from Sauro 2011
suspercentiles <- array(0,c(1,100))
suspercentiles[1] <- c(0.2)
suspercentiles[2] <- c(0.2)
suspercentiles[3] <- c(0.3)
suspercentiles[4] <- c(0.3)
suspercentiles[5] <- c(0.3)
suspercentiles[6] <- c(0.3)
suspercentiles[7] <- c(0.4)
suspercentiles[8] <- c(0.4)
suspercentiles[9] <- c(0.4)
suspercentiles[10] <- c(0.5)
suspercentiles[11] <- c(0.5)
suspercentiles[12] <- c(0.6)
suspercentiles[13] <- c(0.6)
suspercentiles[14] <- c(0.7)
suspercentiles[15] <- c(0.7)
suspercentiles[16] <- c(0.8)
suspercentiles[17] <- c(0.8)
suspercentiles[18] <- c(0.9)
suspercentiles[19] <- c(1.0)
suspercentiles[20] <- c(1.1)
suspercentiles[21] <- c(1.2)
suspercentiles[22] <- c(1.3)
suspercentiles[23] <- c(1.4)
suspercentiles[24] <- c(1.5)
suspercentiles[25] <- c(1.6)
suspercentiles[26] <- c(1.8)
suspercentiles[27] <- c(1.9)
suspercentiles[28] <- c(2.1)
suspercentiles[29] <- c(2.3)
suspercentiles[30] <- c(2.5)
suspercentiles[31] <- c(2.7)
suspercentiles[32] <- c(2.9)
suspercentiles[33] <- c(3.2)
suspercentiles[34] <- c(3.4)
suspercentiles[35] <- c(3.8)
suspercentiles[36] <- c(4.1)
suspercentiles[37] <- c(4.4)
suspercentiles[38] <- c(4.8)
suspercentiles[39] <- c(5.3)
suspercentiles[40] <- c(5.7)
suspercentiles[41] <- c(6.2)
suspercentiles[42] <- c(6.8)
suspercentiles[43] <- c(7.3)
suspercentiles[44] <- c(8.0)
suspercentiles[45] <- c(8.7)
suspercentiles[46] <- c(9.4)
suspercentiles[47] <- c(10.2)
suspercentiles[48] <- c(11.1)
suspercentiles[49] <- c(12.1)
suspercentiles[50] <- c(13.1)
suspercentiles[51] <- c(14.2)
suspercentiles[52] <- c(15.4)
suspercentiles[53] <- c(16.7)
suspercentiles[54] <- c(18.1)
suspercentiles[55] <- c(19.6)
suspercentiles[56] <- c(21.2)
suspercentiles[57] <- c(22.9)
suspercentiles[58] <- c(24.7)
suspercentiles[59] <- c(26.7)
suspercentiles[60] <- c(28.8)
suspercentiles[61] <- c(31.0)
suspercentiles[62] <- c(33.3)
suspercentiles[63] <- c(35.8)
suspercentiles[64] <- c(38.4)
suspercentiles[65] <- c(41.1)
suspercentiles[66] <- c(43.9)
suspercentiles[67] <- c(46.9)
suspercentiles[68] <- c(50.0)
suspercentiles[69] <- c(53.2)
suspercentiles[70] <- c(56.4)
suspercentiles[71] <- c(59.8)
suspercentiles[72] <- c(63.1)
suspercentiles[73] <- c(66.5)
suspercentiles[74] <- c(69.9)
suspercentiles[75] <- c(73.2)
suspercentiles[76] <- c(76.5)
suspercentiles[77] <- c(79.7)
suspercentiles[78] <- c(82.7)
suspercentiles[79] <- c(85.5)
suspercentiles[80] <- c(88.1)
suspercentiles[81] <- c(90.5)
suspercentiles[82] <- c(92.6)
suspercentiles[83] <- c(94.4)
suspercentiles[84] <- c(95.9)
suspercentiles[85] <- c(97.2)
suspercentiles[86] <- c(98.1)
suspercentiles[87] <- c(98.8)
suspercentiles[88] <- c(99.3)
suspercentiles[89] <- c(99.6)
suspercentiles[90] <- c(99.8)
suspercentiles[91] <- c(99.9)
suspercentiles[92] <- c(100.0)
suspercentiles[93] <- c(100.0)
suspercentiles[94] <- c(100.0)
suspercentiles[95] <- c(100.0)
suspercentiles[96] <- c(100.0)
suspercentiles[97] <- c(100.0)
suspercentiles[98] <- c(100.0)
suspercentiles[99] <- c(100.0)
suspercentiles[100] <- c(100.0)
# Curved grades from Sauro 2011
suslow <- itemconf[1,13]
susave <- itemconf[2,13]
sushigh <- itemconf[3,13]
while (x <= 3) {
   if (x == 1) susval <- suslow
   if (x == 2) susval <- susave
   if (x == 3) susval <- sushigh
   if (susval >= 84.1) curvedgrade <- "A+"
   if (susval >= 80.8 && susval < 84.1) curvedgrade <- "A"
   if (susval >= 78.9 && susval < 80.8) curvedgrade <- "A-"
   if (susval >= 77.2 && susval < 78.9) curvedgrade <- "B+"
   if (susval >= 74.1 && susval < 77.2) curvedgrade <- "B"
   if (susval >= 72.6 && susval < 74.1) curvedgrade <- "B-"
   if (susval >= 71.1 && susval < 72.6) curvedgrade <- "C+"
   if (susval >= 65.0 && susval < 71.1) curvedgrade <- "C"
   if (susval >= 62.7 && susval < 65.0) curvedgrade <- "C-"
   if (susval >= 51.7 && susval < 62.7) curvedgrade <- "D"
   if (susval < 51.7) curvedgrade <- "F"
   if (x == 1) susgradelow <- curvedgrade
   if (x == 2) susgradeave <- curvedgrade
   if (x == 3) susgradehigh <- curvedgrade
   x <- x+1
   }
# Converting observed results to percentiles (based on Sauro 2011)
y <- 1
x <- 1
susindexlow <- round(suslow)
susindexave <- round(susave)
susindexhigh <- round(sushigh)
susperclow <- suspercentiles[susindexlow]
suspercave <- suspercentiles[susindexave]
susperchigh <- suspercentiles[susindexhigh]
# Predicting likelihood to recommend and associated Net Promoter Score
promoters <- array(0,c(n))
detractors <- array(0,c(n))
passives <- array(0,c(n))
while (x <= n) {
   ltr <- round(0.1*itemarray[x,13])
   if (ltr >= 9) promoters[x] <- promoters[x]+1
   if (ltr <= 6) detractors[x] <- detractors[x]+1
   if (ltr >6 && ltr < 9) passives[x] <- passives[x]+1
   x <- x+1
   }
x <-1
promosum <- sum(promoters)
detractsum <- sum(detractors)
nps <- (promosum/n - detractsum/n)*100
npsper <- paste(nps,"%",sep="")
# Print the results
cat("\n")
cat("RESULTS OF SUS ANALYSIS \n\n")
cat("Overall \n\n")
cat("Overall SUS mean:",format(susave,digits=3),"\t  ",confper,"confidence interval:",format(suslow,digits=3),"-",format(sushigh,digits=3),"\n")
cat("Percentiles:",format(suspercave,digits=3),"\t  ",confper,"confidence interval:",format(susperclow,digits=3),"-",format(susperchigh,digits=3),"\n")
cat("Curve grades:",susgradeave,"\t  ",confper,"confidence interval:",susgradelow,"-",susgradehigh,"\n")
cat("Estimated NPS:",npsper,"\n\n")
cat("Subscales \n\n")
cat("Usable scale:",format(itemconf[2,11],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,11],digits=3),"-",format(itemconf[3,11],digits=3),"\n")
cat("Learnable scale:",format(itemconf[2,12],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,12],digits=3),"-",format(itemconf[3,12],digits=3),"\n\n")
if (nmissing > 0) cat("Number of deleted cases due to missing data:",nmissing,"\n\n")
}

# Analyze SUS matrix from Jim's Web directory (standard version of SUS with varied tone, raw untransformed data)
# This is for a data file in which rows are cases of within-case data
# Also need to provide desired level for confidence intervals as proportion or percentage (e.g., .95 or 95 for 95% confidence)
# The input file should have row and column labels and be a tab delimited text file
# Missing data should be denoted NA and the entire case will be deleted
#
analyze.sus.fromweb <- function(filename,conf) {
intro <- "http://drjim.0catch.com/PracStatPack/"
f <- paste(intro,filename,sep="")
origdata <- read.table(f,header=TRUE,row.names=1)
data <- na.omit(origdata)
norig <- nrow(origdata)
nleft <- nrow(data)
nmissing <- norig - nleft
attach(data)
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
rows <- nrow(data)
cols <- ncol(data)
n <- rows
x <- 1
y <- 1
# Defining odd and even items
oddeven <- array("",c(10))
oddeven[1] <- "odd"
oddeven[2] <- "even"
oddeven[3] <- "odd"
oddeven[4] <- "even"
oddeven[5] <- "odd"
oddeven[6] <- "even"
oddeven[7] <- "odd"
oddeven[8] <- "even"
oddeven[9] <- "odd"
oddeven[10] <- "even"
# Building item/scale arrays and recoding scores
itemarray <- array(0,c(n,13))
while (y <= cols) {
   while (x <= rows) {
   if (oddeven[y] == "odd") itemarray[x,y] <- data[x,y] - 1
   if (oddeven[y] == "even") itemarray[x,y] <- 5 - data[x,y]
   x <- x+1
   }
y <- y+1
x <- 1
}
while (x <= rows) {
   itemarray[x,11] <- (sum(itemarray[x,1:3]) + sum(itemarray[x,5:9]))*3.125
   itemarray[x,12] <- (sum(itemarray[x,4]) + sum(itemarray[x,10]))*12.5
   itemarray[x,13] <- sum(itemarray[x,1:10])*2.5
   x <- x+1
}
# Confidence intervals for items/scales
y <- 1
x <- 1
itemconf <- array(0,c(3,13))
itemsderr <- array(0,c(13))
d <- array(0,c(13))
u <- array(0,c(13))
upper <- array(0,c(13))
lower <- array(0,c(13))
df = n-1
t <- abs(qt((1-conf)/2,df))
while (y <= 13) {
   itemsderr[y] <- sd(itemarray[,y])/n^.5
   d[y] <- t*itemsderr[y]
   u[y] <- mean(itemarray[,y])
   lower[y] <- u[y]-d[y]
   upper[y] <- u[y]+d[y]
   itemconf[1,y] <- lower[y]
   itemconf[2,y] <- u[y]
   itemconf[3,y] <- upper[y]
   y <- y+1
   }
# SUS Percentiles and from Sauro 2011
suspercentiles <- array(0,c(1,100))
suspercentiles[1] <- c(0.2)
suspercentiles[2] <- c(0.2)
suspercentiles[3] <- c(0.3)
suspercentiles[4] <- c(0.3)
suspercentiles[5] <- c(0.3)
suspercentiles[6] <- c(0.3)
suspercentiles[7] <- c(0.4)
suspercentiles[8] <- c(0.4)
suspercentiles[9] <- c(0.4)
suspercentiles[10] <- c(0.5)
suspercentiles[11] <- c(0.5)
suspercentiles[12] <- c(0.6)
suspercentiles[13] <- c(0.6)
suspercentiles[14] <- c(0.7)
suspercentiles[15] <- c(0.7)
suspercentiles[16] <- c(0.8)
suspercentiles[17] <- c(0.8)
suspercentiles[18] <- c(0.9)
suspercentiles[19] <- c(1.0)
suspercentiles[20] <- c(1.1)
suspercentiles[21] <- c(1.2)
suspercentiles[22] <- c(1.3)
suspercentiles[23] <- c(1.4)
suspercentiles[24] <- c(1.5)
suspercentiles[25] <- c(1.6)
suspercentiles[26] <- c(1.8)
suspercentiles[27] <- c(1.9)
suspercentiles[28] <- c(2.1)
suspercentiles[29] <- c(2.3)
suspercentiles[30] <- c(2.5)
suspercentiles[31] <- c(2.7)
suspercentiles[32] <- c(2.9)
suspercentiles[33] <- c(3.2)
suspercentiles[34] <- c(3.4)
suspercentiles[35] <- c(3.8)
suspercentiles[36] <- c(4.1)
suspercentiles[37] <- c(4.4)
suspercentiles[38] <- c(4.8)
suspercentiles[39] <- c(5.3)
suspercentiles[40] <- c(5.7)
suspercentiles[41] <- c(6.2)
suspercentiles[42] <- c(6.8)
suspercentiles[43] <- c(7.3)
suspercentiles[44] <- c(8.0)
suspercentiles[45] <- c(8.7)
suspercentiles[46] <- c(9.4)
suspercentiles[47] <- c(10.2)
suspercentiles[48] <- c(11.1)
suspercentiles[49] <- c(12.1)
suspercentiles[50] <- c(13.1)
suspercentiles[51] <- c(14.2)
suspercentiles[52] <- c(15.4)
suspercentiles[53] <- c(16.7)
suspercentiles[54] <- c(18.1)
suspercentiles[55] <- c(19.6)
suspercentiles[56] <- c(21.2)
suspercentiles[57] <- c(22.9)
suspercentiles[58] <- c(24.7)
suspercentiles[59] <- c(26.7)
suspercentiles[60] <- c(28.8)
suspercentiles[61] <- c(31.0)
suspercentiles[62] <- c(33.3)
suspercentiles[63] <- c(35.8)
suspercentiles[64] <- c(38.4)
suspercentiles[65] <- c(41.1)
suspercentiles[66] <- c(43.9)
suspercentiles[67] <- c(46.9)
suspercentiles[68] <- c(50.0)
suspercentiles[69] <- c(53.2)
suspercentiles[70] <- c(56.4)
suspercentiles[71] <- c(59.8)
suspercentiles[72] <- c(63.1)
suspercentiles[73] <- c(66.5)
suspercentiles[74] <- c(69.9)
suspercentiles[75] <- c(73.2)
suspercentiles[76] <- c(76.5)
suspercentiles[77] <- c(79.7)
suspercentiles[78] <- c(82.7)
suspercentiles[79] <- c(85.5)
suspercentiles[80] <- c(88.1)
suspercentiles[81] <- c(90.5)
suspercentiles[82] <- c(92.6)
suspercentiles[83] <- c(94.4)
suspercentiles[84] <- c(95.9)
suspercentiles[85] <- c(97.2)
suspercentiles[86] <- c(98.1)
suspercentiles[87] <- c(98.8)
suspercentiles[88] <- c(99.3)
suspercentiles[89] <- c(99.6)
suspercentiles[90] <- c(99.8)
suspercentiles[91] <- c(99.9)
suspercentiles[92] <- c(100.0)
suspercentiles[93] <- c(100.0)
suspercentiles[94] <- c(100.0)
suspercentiles[95] <- c(100.0)
suspercentiles[96] <- c(100.0)
suspercentiles[97] <- c(100.0)
suspercentiles[98] <- c(100.0)
suspercentiles[99] <- c(100.0)
suspercentiles[100] <- c(100.0)
# Curved grades from Sauro 2011
suslow <- itemconf[1,13]
susave <- itemconf[2,13]
sushigh <- itemconf[3,13]
while (x <= 3) {
   if (x == 1) susval <- suslow
   if (x == 2) susval <- susave
   if (x == 3) susval <- sushigh
   if (susval >= 84.1) curvedgrade <- "A+"
   if (susval >= 80.8 && susval < 84.1) curvedgrade <- "A"
   if (susval >= 78.9 && susval < 80.8) curvedgrade <- "A-"
   if (susval >= 77.2 && susval < 78.9) curvedgrade <- "B+"
   if (susval >= 74.1 && susval < 77.2) curvedgrade <- "B"
   if (susval >= 72.6 && susval < 74.1) curvedgrade <- "B-"
   if (susval >= 71.1 && susval < 72.6) curvedgrade <- "C+"
   if (susval >= 65.0 && susval < 71.1) curvedgrade <- "C"
   if (susval >= 62.7 && susval < 65.0) curvedgrade <- "C-"
   if (susval >= 51.7 && susval < 62.7) curvedgrade <- "D"
   if (susval < 51.7) curvedgrade <- "F"
   if (x == 1) susgradelow <- curvedgrade
   if (x == 2) susgradeave <- curvedgrade
   if (x == 3) susgradehigh <- curvedgrade
   x <- x+1
   }
# Converting observed results to percentiles (based on Sauro 2011)
y <- 1
x <- 1
susindexlow <- round(suslow)
susindexave <- round(susave)
susindexhigh <- round(sushigh)
susperclow <- suspercentiles[susindexlow]
suspercave <- suspercentiles[susindexave]
susperchigh <- suspercentiles[susindexhigh]
# Predicting likelihood to recommend and associated Net Promoter Score
promoters <- array(0,c(n))
detractors <- array(0,c(n))
passives <- array(0,c(n))
while (x <= n) {
   ltr <- round(.1*itemarray[x,13])
   if (ltr >= 9) promoters[x] <- promoters[x]+1
   if (ltr <= 6) detractors[x] <- detractors[x]+1
   if (ltr >6 && ltr < 9) passives[x] <- passives[x]+1
   x <- x+1
   }
x <-1
promosum <- sum(promoters)
detractsum <- sum(detractors)
nps <- (promosum/n - detractsum/n)*100
npsper <- paste(nps,"%",sep="")
# Print the results
cat("\n")
cat("RESULTS OF SUS ANALYSIS \n\n")
cat("Overall \n\n")
cat("Overall SUS mean:",format(susave,digits=3),"\t  ",confper,"confidence interval:",format(suslow,digits=3),"-",format(sushigh,digits=3),"\n")
cat("Percentiles:",format(suspercave,digits=3),"\t  ",confper,"confidence interval:",format(susperclow,digits=3),"-",format(susperchigh,digits=3),"\n")
cat("Curve grades:",susgradeave,"\t  ",confper,"confidence interval:",susgradelow,"-",susgradehigh,"\n")
cat("Estimated NPS:",npsper,"\n\n")
cat("Subscales \n\n")
cat("Usable scale:",format(itemconf[2,11],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,11],digits=3),"-",format(itemconf[3,11],digits=3),"\n")
cat("Learnable scale:",format(itemconf[2,12],digits=3),"\t  ",confper,"confidence interval:",format(itemconf[1,12],digits=3),"-",format(itemconf[3,12],digits=3),"\n\n")
if (nmissing > 0) cat("Number of deleted cases due to missing data:",nmissing,"\n\n")
}

# bench.rate.largesample: Comparing completion rate to benchmark one-tailed binomial test for large samples (np < 15)
# x is the number of targeted events (e.g., successes), n is the sample size, 
# bench is the targeted benchmark expressed as a number between 0 and 1 
# (e.g., a target of 90% successes would be .90)
# 
bench.rate.largesample <- function(x,n,bench) {
mle <- x/n
d <- mle - bench
np <- n*bench
se <- ((bench * (1-bench))/n)^.5
z <- d/se
appgreaterthanp <- pnorm(z)
applessthanp <- 1-pnorm(z)
cat("\nRESULTS\n\n")
cat("Observed proportion:",mle,"\n")
cat("z:",z,"\n")
cat("Probability of exceeding benchmark:",appgreaterthanp,"(p <",1- appgreaterthanp,")\n")
cat("Probability of being below benchmark:",applessthanp,"(p <",1- applessthanp,")\n")
if (np < 15) cat("Warning: sample size too small -- np < 15 \n")
cat("\n")
}

# Comparing completion rate to benchmark one-tailed binomial test for small samples (np < 15)
# x is the number of targeted events (e.g., successes), n is the sample size, 
# bench is the targeted benchmark expressed as a number between 0 and 1 
# (e.g., a target of 90% successes would be .90)
#
bench.rate.smallsample <- function(x,n,bench) {
mle <- x/n
np <- n*bench
exactexceedsp <- pbinom(x-1,n,bench)
midpexceedsp <- pbinom(x-1,n,bench) + .5*dbinom(x,n,bench)
exactislessthanp <- 1 - pbinom(x,n,bench)
midpislessthanp <- 1 - midpexceedsp
cat("\nRESULTS (mid-p recommended)\n\n")
cat("Observed proportion:",mle,"\n")
cat("Exact probability of exceeding benchmark:",exactexceedsp,"(p <",1- exactexceedsp,")\n")
cat("Mid probability of exceeding benchmark:",midpexceedsp,"(p <",1- midpexceedsp,")\n")
cat("Exact probability of being below benchmark:",exactislessthanp,"(p <",1- exactislessthanp,")\n")
cat("Mid probability of being below benchmark:",midpislessthanp,"(p <",1-midpislessthanp,")\n\n")
}

# Comparing mean to criterion with t with array as input (one-tailed test)
# x is an array of scores, bench is the targeted benchmark
bench.t.fromarray <- function(x,bench) {
mean <- mean(x)
n <- length(x)
df <- n - 1
sd <- sd(x)
t <- (mean - bench)/(sd/(n^.5))
p <- pt(t,df)
cat("\nRESULTS\n\n")
cat("Observed mean:",mean,"\n")
cat("t:",t,"   df:",df,"\n")
cat("Probability of exceeding benchmark:",p,"(p <",1 - p,")\n")
cat("Probability of being below benchmark:",1 - p,"(p <",p,")\n\n")
}

# Comparing mean of logs to log criterion with t with array as input (one-tailed test)
# x is an array of time scores, or any other type of data on which 
# to perform a log transformation; bench is the specified benchmark value
#
bench.t.fromarray.withlogconversion <- function(x,bench) {
logx <- log(x)
logmean <- mean(logx)
geomean <- exp(logmean)
logbench <- log(bench)
regmean <- mean(x)
regmedian <- median(x)
n <- length(logx)
df <- n - 1
sd <- sd(logx)
t <- (logmean - logbench)/(sd/(n^.5))
p <- pt(t,df)
cat("\nRESULTS\n\n")
cat ("Arithmetic mean:",regmean,"\n")
cat ("Median:",regmedian, "\n")
cat("Geometric mean:",geomean,"\n")
cat("t:",t,"   df:",df,"\n")
cat("Probability of exceeding benchmark:",p,"(p <",1 - p,")\n")
cat("Probability of being below benchmark:",1 - p,"(p <",p,")\n\n")
}

# Comparing mean to criterion with t with array as input (one-tailed test)
# mean is the observed mean, bench is the targeted benchmark value, 
# sd is the standard deviation, n is the sample size
#
bench.t.fromsummary <- function(mean,bench,sd,n) {
df <- n - 1
t <- (mean - bench)/(sd/(n^.5))
p <- pt(t,df)
cat("\nRESULTS\n\n")
cat("t:",t,"\n")
cat("df:",df,"\n")
cat("Probability of exceeding benchmark:",p,"(p <",1 - p,")\n")
cat("Probability of being below benchmark:",1 - p,"(p <",p,")\n\n")
}

# Adjusted-Wald binomial confidence interval given array of 0s and 1s
# Scores is an array made up of 0s and 1s; conf is the desired level of confidence
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
# z adjusted to one-sided interval if all x's are 0 or 1
#
ci.adjwald.fromarray <- function(scores,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
n <- length(scores)
x <- sum(scores)
p <- x/n
z <- abs(qnorm((1-conf)/2))
if (mean(scores) == 0) z <- abs(qnorm(1-conf))
if (mean(scores) == 1) z <- abs(qnorm(1-conf))
xadj <- x + (z^2)/2
nadj <- n + z^2
padj <- xadj/nadj
se <- ((padj*(1-padj))/nadj)^.5
d <- se*z
lower <- padj - d
upper <- padj + d
cat ("\nRESULTS \n\n")
cat ("Adjusted p (Wilson):\t",padj, "\n")
cat ("Margin of error:\t",d, "\n")
if (upper <= 1) cat(confper,"upper limit:\t", upper, "\n") else cat(confper,"upper limit:\t>.999999 \n")
cat ("p (maximum likelihood):\t", p, "\n")
if (lower >= 0) cat(confper,"lower limit:\t", lower, "\n") else cat(confper,"lower limit:\t<.000001 \n")
cat ("","\n")
}

# Adjusted-Wald binomial confidence interval given x and n
# x is the number of targeted events (e.g., successes), n is the sample size,
# conf is the desired level of confidence expressed as a number between 0 and 1 or between 1 and 100
# (e.g., 95% confidence would be .95 or 95)
# z adjusted to one-sided interval if all x's are 0 or 1
#
ci.adjwald.fromsummary <- function(x,n,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
z <- abs(qnorm((1-conf)/2))
if (x == 0) z <- abs(qnorm(1-conf))
if (x == n) z <- abs(qnorm(1-conf))
p <- x/n
xadj <- x + (z^2)/2
nadj <- n + z^2
padj <- xadj/nadj
se <- ((padj*(1-padj))/nadj)^.5
d <- se*z
lower <- padj - d
upper <- padj + d
cat ("\nRESULTS \n\n")
cat ("Adjusted p (Wilson):\t",padj, "\n")
cat ("Margin of error:\t",d, "\n")
if (upper <= 1) cat(confper,"upper limit:\t", upper, "\n") else cat(confper,"upper limit:\t>.999999 \n")
cat ("p (maximum likelihood):\t", p, "\n")
if (lower >= 0) cat(confper,"lower limit:\t", lower, "\n") else cat(confper,"lower limit:\t<.000001 \n")
cat ("","\n")
}

# Confidence interval (with test of significance) for N-1 Two-Proportion Test given p and n
# Inputs are p1, n1, p2, n2, and conf
# conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
#
ci.independentproportions.difference <- function(p1,n1,p2,n2,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
x1 <- p1 * n1
x2 <- p2 * n2
N <- n1 + n2
a <- round(x1)
b <- round(n1 - x1)
c <- round(x2)
d <- round(n2 - x2)
m <- a + b
n <- c + d
r <- a + c
s <- b + d
P <- (x1 + x2)/(n1 + n2)
Q <- 1 - P
testznum <- (p1 - p2)*((N-1)/N)^.5
testzden <- (P * Q * (1/n1 + 1/n2))^.5
testz <- testznum/testzden
ptestz <- 2*pnorm(-abs(testz))
z <- abs(qnorm((1-conf)/2))
p1adj <- (a + z^2/4)/(m + z^2/2)
p2adj <- (c + z^2/4)/(n + z^2/2)
n1adj <- m + z^2/2
n2adj <- n + z^2/2
diff <- p1 - p2
diffadj <- p1adj - p2adj
se <- ((p1adj*(1 - p1adj)/n1adj) + (p2adj*(1 - p2adj)/n2adj))^.5
critdiff <- z*se
upper <- diffadj + critdiff
lower <- diffadj - critdiff
cat("\nRESULTS\n\n")
cat (confper,"confidence interval of difference between proportions","\n\n")
cat ("p1:",p1,"\n")
cat ("p2:",p2,"\n")
cat ("Adjusted value of p1:",p1adj,"\n")
cat ("Adjusted value of p2:",p2adj,"\n")
cat ("Adjusted difference:",diffadj,"\n")
cat ("Margin of error:",critdiff,"\n\n")
cat("Upper limit:", upper, "\n")
cat ("Maximum likelihood estimate (observed difference):", diff, "\n")
cat("Lower limit:", lower, "\n\n")
cat ("N-1 Two-Proportion z:",testz,"   p:",ptestz,"\n\n")
}

# Confidence interval for difference in matched proportions (with McNemar test)
# Focuses on McNemar Test (two-tailed) with confidence interval on difference in proportions
# Inputs are p1, p2, p12, p21, n and conf
# p1 and p2 are the overall proportions for the study (e.g., success rates)
# p12 and p21 are the discordance rates (p12: passed A, failed B; p21: passed B, failed A)
# n is the sample size (number of people who experienced both test conditions)
# conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
#
ci.matchedproportions.difference <- function(p1,p2,p12,p21,n,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
N <- n
z <- abs(qnorm((1-conf)/2))
p11 <- p2 - p21
p22 <- (1 - p2) - p12
a <- round(p11 * N)
b <- round(p12 * N)
c <- round(p21 * N)
d <- round(p22 * N)
m <- a + b
n <- c + d
r <- a + c
s <- b + d
adj = z^2/8
aadj <- a + adj
badj <- b + adj
cadj <- c + adj
dadj <- d + adj
madj <- aadj + badj
nadj <- cadj + dadj
radj <- aadj + cadj
sadj <- badj + dadj
Nadj <- aadj + badj + cadj + dadj
p1adj <- madj/Nadj
p2adj <- radj/Nadj
p12adj <- badj/Nadj
p21adj <- cadj/Nadj
diffadj <- p2adj - p1adj
se <- (((p12adj + p21adj) - (p21adj - p12adj)^2)/Nadj)^.5
critdiff <- se*z
upper <- diffadj + critdiff
lower <- diffadj - critdiff
ndiscord <- b + c
smaller <- min(b,c)
bench <- .5
mle <- smaller/ndiscord
exactexceedsp <- pbinom(smaller-1,ndiscord,bench)
midpexceedsp <- pbinom(smaller-1,ndiscord,bench) + .5*dbinom(smaller,ndiscord,bench)
exactislessthanp <- 1 - pbinom(smaller,ndiscord,bench)
midpislessthanp <- 1 - midpexceedsp
mcnemarmidp <- 2*(1-midpislessthanp)
mcnemarexactp <- 2*(1- exactislessthanp)
cat("\nRESULTS\n\n")
cat(confper,"CONFIDENCE INTERVAL \n\n")
cat("p1:",p1,"\n")
cat("p2:",p2,"\n")
cat ("Adjusted value of p1:",p1adj,"\n")
cat ("Adjusted value of p2:",p2adj,"\n")
cat("Adjusted difference:",diffadj,"\n")
cat("Margin of error:",critdiff,"\n")
cat("Upper limit:",upper,"\n")
cat("Observed difference:",p2-p1,"\n")
cat("Lower limit:",lower,"\n\n")
cat("TESTS OF SIGNIFICANCE \n\n")
if (mcnemarmidp <= 1) cat("McNemar Mid Probability (recommended):", mcnemarmidp, "\n\n") else cat("McNemar Mid Probability (recommended): 1 \n\n")
cat("Alternate analyses (not recommended) \n")
if (mcnemarexactp <= 1) cat("McNemar Exact Probability:", mcnemarexactp, "\n") else cat("McNemar Exact Probability: 1 \n")
mcnemarchisquared <- ((c - b)^2)/(c + b)
df <- 1
pmcnemarchisquared <- 1-pchisq(mcnemarchisquared,df)
cat("McNemar Chi-Squared:",mcnemarchisquared,"   df:",df,"   p:",pmcnemarchisquared,"\n")
mcnemaryates <- ((abs(c - b)-1)^2)/(c + b)
pmcnemaryates <- 1-pchisq(mcnemaryates,df)
cat("McNemar Chi-Squared with Yates Correction:",mcnemaryates,"   df:",df,"   p:",pmcnemaryates,"\n\n")
}

# Confidence interval around the median (use for n > 25 scores)
# x is an array of scores; conf is the desired level of confidence
# expressed as a number between 0 and 1 or between 1 and 100 
# (e.g., 95% confidence would be .95 or 95)
#
ci.median.fromarray <- function(x,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
n <- length(x)
m <- median(x)
p <- .5
np <- n*p
z <- abs(qnorm((1-conf)/2))
se <- (np*(1-p))^.5
d <- z*se
sortx <- sort(x)
lowerdatapoint <- ceiling(np-d) #ceiling is roundup function
upperdatapoint <- ceiling(np+d)
lower <- sortx[lowerdatapoint]
upper <- sortx[upperdatapoint]
cat ("\nRESULTS:",confper,"CONFIDENCE INTERVAL \n\n")
cat("Upper limit:", upper, "\n")
cat("Median:", m, "\n")
cat("Lower limit:", lower, "\n")
if (n < 25) cat("Warning: sample size too small -- n < 25")
cat("\n")
}

# Confidence interval around a percentile (use for large samples -- np > 15 -- smaller p needs larger n)
# x is an array of scores, p is the specified percentile,
# conf is the level of confidence expressed as a number between 0 and 1 or between 1 and 100
# (e.g., 95% confidence would be .95 or 95)
#
ci.percentile.fromarray <- function(x,p,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
n <- length(x)
np <- n*p
z <- abs(qnorm((1-conf)/2))
se <- (np*(1-p))^.5
d <- z*se
sortx <- sort(x)
lowerdatapoint <- ceiling(np-d)
upperdatapoint <- ceiling(np+d)
lower <- sortx[lowerdatapoint]
upper <- sortx[upperdatapoint]
percentile <- quantile(x,p)
cat ("\nRESULTS:",confper,"CONFIDENCE INTERVAL \n\n")
cat("Upper limit:", upper, "\n")
cat("Percentile (",p,"):",percentile, "\n")
cat("Lower limit:", lower, "\n")
if (np < 15) cat("Warning: sample size too small -- np < 15 \n")
cat("\n")
}

# Confidence interval using t-score given an array of scores
# x is an array of scores; conf is the desired level of confidence
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
#
ci.t.fromarray <- function(x,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
n <- length(x)
df = n-1
t <- abs(qt((1-conf)/2,df))
se <- sd(x)/n^.5
d <- t*se
u <- mean(x)
lower <- u-d
upper <- u+d
cat ("\nRESULTS:",confper,"CONFIDENCE INTERVAL \n\n")
cat ("Upper limit:", upper, "\n")
cat ("Mean:", u, "\n")
cat ("Lower limit:", lower, "\n")
cat ("Margin of error:",d, "\n\n")
}

# Confidence interval using t-Score given an array of time scores (use for n < 25 scores)
# x is an array of time scores, or any other type of data on which 
# to perform a log transformation;
# conf is the level of confidence expressed as a number between 0 and 1 or between 1 and 100
# (e.g., 95% confidence would be .95 or 95)
#
ci.t.fromarray.withlogconversion <- function(x,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
n <- length(x)
regmean <- mean(x)
regmedian <- median(x)
df = n-1
xlog <- log(x)
t <- abs(qt((1-conf)/2,df))
se <- sd(xlog)/n^.5
d <- t*se
ulog <- mean(xlog)
lowerlog <- ulog-d
upperlog <- ulog+d
u <- exp(ulog)
lower <- exp(lowerlog)
upper <- exp(upperlog)
cat ("\nRESULTS:",confper,"CONFIDENCE INTERVAL \n\n")
cat ("Arithmetic mean:",regmean,"\n")
cat ("Median:",regmedian, "\n")
cat ("Upper limit:", upper, "\n")
cat ("Geometric Mean:", u, "\n")
cat ("Lower limit:", lower, "\n")
cat ("Critical value of t:",t,"\n")
cat ("\n")
}

# Confidence interval using t-Score given summary data
# u is the mean, sd the standard deviation, n the sample size
# conf is the level of confidence expressed as a number between 0 and 1 or between 1 and 100
# (e.g., 95% confidence would be .95 or 95)
#
ci.t.fromsummary <- function(u,sd,n,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
df = n-1
t <- abs(qt((1-conf)/2,df))
d <- t*sd/n^.5
lower <- u-d
upper <- u+d
cat ("\nRESULTS:",confper,"CONFIDENCE INTERVAL \n\n")
cat ("Upper limit:", upper, "\n")
cat ("Mean:", u, "\n")
cat ("Lower limit:", lower, "\n")
cat ("Margin of error:",d, "\n")
cat ("Standard error:",sd/n^.5,"\n")
cat ("Critical value of t:",t,"\n\n")
}

# Compute equivalent confidence given nominal confidence and power and number of tails
#
compute.equivalentconfidence <- function(conf,power,tails) {
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
if (tails != 1) zconf <- abs(qnorm((1-conf)/2))
if (tails == 1) zconf <- abs(qnorm((1-conf)))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
if (tails != 1) equivconf <- 1 - 2*(1-pnorm(z))
if (tails == 1) equivconf <- pnorm(z)
cat("\nRESULTS\n\n")
cat("z(confidence):",zconf,"\n")
cat("z(power):",zpower,"\n")
cat("z(combined):",z,"\n\n")
cat("Equivalent confidence:",equivconf,"\n\n")
}

# Value of critical difference for log data given the arithmetic (non-logged) mean and the critical difference
#
compute.logcritdiff <- function(u,d) {
sum <- u + d
dlog <- log(sum) - log(u)
cat("\nRESULT\n\n")
cat("Value to use for log critical difference:",dlog,"\n\n")
}

# Preprocessing for time data to use as input for sample size estimation
# Input is an array of times (or any other data to undergo log transformation) and a planned critical difference (x and d)
# Output are the arithmetic mean, the median, the geometric mean, the variance and standard deviations of the log data,
# and the value to use for the critical difference 
compute.logsummary.fromarray <- function (x,d) {
n <- length(x)
regmean <- mean(x)
regmedian <- median(x)
xlog <- log(x)
meanlog <- mean(xlog)
geomean <- exp(meanlog)
sdlog <- sd(xlog)
varlog <- sd(xlog)^2
selog <- sdlog/n^.5
sum <- regmean + d
dlog <- log(sum) - log(regmean)
cat("\nRESULTS\n\n")
cat("Arithmetic mean:",regmean,"\n")
cat("Median:",regmedian,"\n")
cat("Geometric Mean:", geomean,"\n")
cat("Mean of log data:",meanlog,"\n")
cat("Standard deviation of log data:", sdlog,"\n")
cat("Variance of log data:", varlog, "\n")
cat("Standard error of the mean of the log data:",selog,"\n")
cat("Sample size:",n,"\n")
cat("Value to use for log critical difference:",dlog,"\n")
cat("\n")
}

# Compute Net Promoter Score from array of raw NPS data
# Raw NPS data are Likelihood to Recommend (LTR) scores -- integers ranging from 0 to 10
#
compute.nps.fromarray <- function(scores) {
n <- length(scores)
promoters <- array(0,c(n))
detractors <- array(0,c(n))
passives <- array(0,c(n))
x <- 1
while (x <= n) {
   if (scores[x] >= 9) promoters[x] <- promoters[x]+1
   if (scores[x] <= 6) detractors[x] <- detractors[x]+1
   if (scores[x] >6 && scores[x] < 9) passives[x] <- passives[x]+1
   x <- x+1
   }
promosum <- sum(promoters)
detractsum <- sum(detractors)
passivesum <- sum(passives)
nps <- (promosum/n - detractsum/n)*100
npsper <- paste(format(nps, digits=4),"%",sep="")
cat("\nRESULTS \n\n")
cat("Number of promoters:",promosum,"\n")
cat("Number of passives:",passivesum,"\n")
cat("Number of detractors:",detractsum,"\n")
cat("Net Promoter Score (NPS):",npsper,"\n\n")
}

# Estimating padj from p (from Jeff's regression equation)
#
compute.padjfromp <- function(p) {
padj <- .9*p - .046
cat("\nRESULT\n\n")
cat("Estimate of adjusted p:",padj,"\n")
cat("\n")
}

# Get case-style data from a stored matrix of data
# This is for a data file in which rows are cases of within-case data
# The names of the dependent variables come from the column headers
# Missing data needs to be represented by NA in file
# This function deletes cases with missing data
# The R syntax for getting a file on a PC uses a path like this: 
# "C:\\Documents and Settings\\jimlewis\\My Documents\\R-Files\\table501.txt"
#
getdata.fromfile <- function(filename) {
f <- filename
origdata <- read.table(f,header=TRUE)
data <- na.omit(origdata)
norig <- nrow(origdata)
nleft <- nrow(data)
nmissing <- norig - nleft
attach(data)
if (nmissing > 0) cat("Number of deleted cases due to missing data:",nmissing,"\n")
}

# Get case-style data from a stored matrix of data from Jim's website
# This is for a data file in which rows are cases of within-case data
# The names of the dependent variables come from the column headers
# Missing data needs to be represented by NA before input
# This function deletes cases with missing data
#
getdata.fromweb <- function(filename) {
intro <- "http://drjim.0catch.com/PracStatPack/"
f <- paste(intro,filename,sep="")
origdata <- read.table(f,header=TRUE)
data <- na.omit(origdata)
norig <- nrow(origdata)
nleft <- nrow(data)
nmissing <- norig - nleft
attach(data)
if (nmissing > 0) cat("Number of deleted cases due to missing data:",nmissing,"\n")
}

# Sample size estimation for "at least once" problem discovery
# Inputs are the problem discovery goal expressed as a proportion between 0 and 1 (e.g., .80 for 80% discovery),
# and the lowest probability of problem occurrence for which you want to achieve the problem discovery goal,
# also expressed as a number between 0 and 1 (e.g., .15)
#
n.atleastonce <- function(discoverygoal,problemprob) {
n <- ceiling(log(1 - discoverygoal)/log(1 - problemprob))
cat("\nRESULT\n\n")
cat("Recommended sample size:",n,"\n")
cat("\n")
}

# Sample size estimation for comparing rate with upper benchmark (iterative method based on adjusted-Wald)
# bench is the target proportion; critd is the minimum (critical) difference to be able to detect
# Desired levels of confidence and power expressed as a number between 0 and 1 or between 1 and 100
# (e.g., 95% confidence would be .95 or 95; 80% power would be .8 or 80)
# Because is test against benchmark, uses one-tailed confidence
#
n.bench.rate <- function(bench,critd,conf,power) {
# Initialize values
tails <- 1
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
zconf <- abs(qnorm((1-conf)))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
if (tails != 1) equivconf <- 1 - 2*(1-pnorm(z))
if (tails == 1) equivconf <- pnorm(z)
if (tails != 1) equivconfper <- paste(format(equivconf*100,digits=7),"% (two-sided)",sep="")
if (tails == 1) equivconfper <- paste(format(equivconf*100,digits=7),"% (two-sided)",sep="")
ncurr <- 2
ninit <- 1
iteration <- 0
p = bench + critd
q = 1 - p
# Standard (Cohen, 1988) estimate of n (nstd)
es1 <- 2*asin(sqrt(p))
es2 <- 2*asin(sqrt(bench))
h <- abs(es1-es2)*sqrt(2)
nstd <- ceiling(((z/h)^2)*2)
# Get initial estimate of n and compute first adjusted-Wald lower bound
ninit <- ceiling((z^2*p*q)/critd^2)
x <- round(ninit*p)
xadj <- x + (z^2)/2
nadj <- ninit + z^2
padj <- xadj/nadj
se <- ((padj*(1-padj))/nadj)^.5
d <- se*z
lower <- padj - d
upper <- padj + d
cat ("\nRESULTS \n\n")
cat("z(confidence):",zconf,"\n")
cat("z(power):",zpower,"\n")
cat("z(combined):",z,"\n")
cat("Equivalent confidence:",equivconf,"\n\n")
cat("Standard estimate of n:",nstd,"\n\n")
cat ("Initial estimate of n:",ninit,"\n")
cat ("Initial adjusted p (Wilson):",padj, "\n")
cat ("Initial margin of error:",d, "\n")
cat ("p (maximum likelihood):", p, "\n")
if (lower >= 0) cat("Initial lower limit:", lower, "\n") else cat("Initial lower limit:<.000001 \n")
cat("\n")
# Search for lowest sample size that has lower bound above criterion for given p (which is sum of bench and critd)
n <- ninit
while ((lower < bench) && (iteration < 10001)) {
   iteration <- iteration + 1
   n <- n +1
   x <- round(n * p)
   mle <- x/n
   xadj <- x + (z^2)/2
   nadj <- n + z^2
   padj <- xadj/nadj
   se <- ((padj*(1-padj))/nadj)^.5
   dadj <- se*z
   lower <- padj - dadj
   upper <- padj + dadj
   cat("Iteration:",iteration,"\tn:",n,"\tx:",x,"\tp:",format(mle,digits=5),"\txadj:",format(xadj,digits=5),"\tnadj:",format(nadj,digits=5))
   cat("\tpadj:",format(padj,digits=5),"\tdadj:",format(dadj,digits=5),"\tlower:",format(lower,digits=5))
   cat("\n")
   }
cat("\n")
if (iteration < 10000) cat("Recommended sample size:",n,"\n") else cat("WARNING: Over 10000 iterations -- problem finding adequate sample size \n")
cat("\n")
# Determine maximum number of errors that can occur and still accomplish test goals
midpexceedsp <- 1
while ((x > 0) && ((1-midpexceedsp) < (1-conf))) {
   midpexceedsp <- pbinom(x-1,n,bench) + .5*dbinom(x,n,bench)
   x <- x - 1
   }
x <- x + 2
midpexceedsp <- pbinom(x-1,n,bench) + .5*dbinom(x,n,bench)
cat("Maximum tolerable number of failures given alpha =",1-conf,"with an observed mid-p significance level of",1-midpexceedsp,"is:",n-x,"\n\n")
}

# Binomial sample size estimation for large samples given confidence, power, p, the critical difference, and number of tails
# conf is the desired level of confidence expressed as a number between 0 and 1 or 1 and 100
# e.g., 95% confidence would be .95 or 95
# power is also a number between 0 and 1 or 1 and 100, always one-sided, 
# e.g., 50% confidence would be .50 or 50 (with z = 0)
#
n.binomial.largesample <- function(conf,power,p,d,tails) {
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
if (tails != 1) zconf <- abs(qnorm((1-conf)/2))
if (tails == 1) zconf <- abs(qnorm((1-conf)))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
q <- 1 - p
n <- ceiling((z^2)*p*q/(d^2))
cat("\nRESULT\n\n")
cat("Recommended sample size:",n,"\n")
cat("\n")
}

# Binomial sample size estimation for small samples given confidence, power, p, the critical difference, and number of tails
# conf is the desired level of confidence expressed as a number between 0 and 1 or 1 and 100
# e.g., 95% confidence would be .95 or 95
# power is also a number between 0 and 1 or 1 and 100, always one-sided, 
# e.g., 50% confidence would be .50 or 50 (with z = 0)
#
n.binomial.smallsample <- function(conf,power,p,d,tails) {
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
if (tails != 1) zconf <- abs(qnorm((1-conf)/2))
if (tails == 1) zconf <- abs(qnorm((1-conf)))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
q <- 1 - p
ninit <- ceiling((z^2)*p*q/(d^2))
padj <- (ninit*p + (z^2)/2)/(ninit + z^2)
qadj <- 1 - padj
n <- ceiling(((z^2)*padj*qadj/(d^2)) - (z^2))
cat("\nRESULT\n\n")
cat("Recommended sample size:",n,"\n")
cat("\n")
}

# Sample size estimation for a McNemar (difference in dependent proportions) test
# p12 is the expected proportion of discordant pairs in which participants succeed with A but fail with B
# p21 is the expected proportion of discordant pairs in which participants succeed with B but fail with A
# d is the difference between p12 and p21
# conf is the desired level of confidence expressed as a number between 0 and 1 or 1 and 100
# e.g., 95% confidence would be .95 or 95
# power is also a number between 0 and 1 or 1 and 100, always one-sided, 
# e.g., 50% confidence would be .50 or 50 (with z = 0)
#
n.mcnemar <- function(conf,power,p12,p21) {
d <- abs(p12 - p21)
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
zconf <- abs(qnorm((1-conf)/2))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
Ninit <- ceiling(((z^2)*(p12 + p21)/(d^2)) - (z^2))
padj12 <- ((p12*Ninit) + (z^2)/8)/(Ninit + (z^2)/2)
padj21 <- ((p21*Ninit) + (z^2)/8)/(Ninit + (z^2)/2)
dadj <- padj21 - padj12
N <- ((z^2)*(padj12 + padj21)/(dadj^2)) - 1.5*(z^2)
n <- ceiling(N)
cat("\nRESULT\n\n")
cat("Recommended sample size:",n,"\n")
cat("\n")
}

# Sample size estimation for two-by-two N-1 Chi-Squared (difference in independent proportions) test
# conf is the desired level of confidence expressed as a number between 0 and 1 or 1 and 100
# e.g., 95% confidence would be .95 or 95
# power is also a number between 0 and 1 or 1 and 100, always one-sided, 
# e.g., 50% confidence would be .50 or 50 (with z = 0)
#
n.nminusonechisquared <- function(conf,power,p1,p2) {
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
d <- abs(p1 - p2)
zconf <- abs(qnorm((1-conf)/2))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
p <- (p1 + p2)/2
q <- 1 - p
n <- ceiling((2*(z^2)*p*q/(d^2)) + .5)
cat("\nRESULTS\n\n")
cat("Recommended sample size per group:",n,"\n")
cat("Recommended total sample size:",2*n,"\n")
cat("\n")
}

# Sample size estimation for one-sample t-test given confidence, power, sd, critical difference, and 1 vs 2 tailed
# conf is the desired level of confidence expressed as a number between 0 and 1 or between 1 and 100
# e.g., 95% confidence would be .95 or 95
# power is also a number between 0 and 1 or 1 and 100, always one-sided, 
# e.g., 50% confidence would be .50 or 50 (with z = 0)
# Uses iteration to estimate required sample size, max of 1000 iterations
#
n.t.onesample.givensd <- function(conf,power,sd,d,tails) {
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
var <- sd^2
ncurr <- -1
nprev1 <- -2
nprev2 <- -3
iteration <- 0
if (tails != 1) zconf <- abs(qnorm((1-conf)/2))
if (tails == 1) zconf <- abs(qnorm((1-conf)))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
nprev1 <- ceiling((z^2)*var/(d^2))
df <- nprev1 - 1
while ((ncurr != nprev1 ) && (ncurr != nprev2) && (iteration < 1001)) {
   iteration <- iteration + 1
   nprev2 <- nprev1
   nprev1 <- ncurr
   if (tails != 1) tconf <- abs(qt((1-conf)/2,df))
   if (tails == 1) tconf <- abs(qt((1-conf),df))
   tpower <- abs(qt((1-power),df))
   t <- tconf + tpower
   ncurr <- ceiling((t^2)*var/(d^2))
   df <- ncurr -1
   }
cat("\nRESULTS\n\n")
if (ncurr == nprev2 && ncurr < nprev1) cat("No convergence, fluctuating between:",ncurr,"and",nprev1,"\n")
if (ncurr == nprev2 && ncurr < nprev1) ncurr <- nprev1
cat("Recommended sample size:",ncurr,"\n")
cat("Number of iterations:",iteration,"\n")
if (iteration > 999) cat("WARNING: Over 1000 iterations -- problem converging \n")
cat("\n")
}

# Sample size estimation for one-sample t-test given confidence, power, variance, critical difference, and 1 vs 2 tailed
# conf is the desired level of confidence expressed as a number between 0 and 1 or between 1 and 100
# e.g., 95% confidence would be .95 or 95
# power is also a number between 0 and 1 or 1 and 100, always one-sided, 
# e.g., 50% confidence would be .50 or 50 (with z = 0)
# Uses iteration to estimate required sample size, max of 1000 iterations
#
n.t.onesample.givenvar <- function(conf,power,var,d,tails) {
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
ncurr <- -1
nprev1 <- -2
nprev2 <- -3
iteration <- 0
if (tails != 1) zconf <- abs(qnorm((1-conf)/2))
if (tails == 1) zconf <- abs(qnorm((1-conf)))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
nprev1 <- ceiling((z^2)*var/(d^2))
df <- nprev1 - 1
while ((ncurr != nprev1 ) && (ncurr != nprev2) && (iteration < 1001)) {
   iteration <- iteration + 1
   nprev2 <- nprev1
   nprev1 <- ncurr
   if (tails != 1) tconf <- abs(qt((1-conf)/2,df))
   if (tails == 1) tconf <- abs(qt((1-conf),df))
   tpower <- abs(qt((1-power),df))
   t <- tconf + tpower
   ncurr <- ceiling((t^2)*var/(d^2))
   df <- ncurr -1
   }
cat("\nRESULTS\n\n")
if (ncurr == nprev2 && ncurr < nprev1) cat("No convergence, fluctuating between:",ncurr,"and",nprev1,"\n")
if (ncurr == nprev2 && ncurr < nprev1) ncurr <- nprev1
cat("Recommended sample size:",ncurr,"\n")
cat("Number of iterations:",iteration,"\n")
if (iteration > 999) cat("WARNING: Over 1000 iterations -- problem converging \n")
cat("\n")
}

# Sample size estimation for two-sample t-test given confidence, power, sd, critical difference, and 1 vs 2 tailed
# conf is the desired level of confidence expressed as a number between 0 and 1 or between 1 and 100
# e.g., 95% confidence would be .95 or 95
# power is also a number between 0 and 1 or 1 and 100, always one-sided, 
# e.g., 50% confidence would be .50 or 50 (with z = 0)
# Uses iteration to estimate required sample size, max of 1000 iterations
#
n.t.twosample.givenequalsd <- function(conf,power,sd,d,tails) {
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
var <- sd^2
ncurr <- -1
nprev1 <- -2
nprev2 <- -3
iteration <- 0
if (tails != 1) zconf <- abs(qnorm((1-conf)/2))
if (tails == 1) zconf <- abs(qnorm((1-conf)))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
nprev1 <- ceiling(2*(z^2)*var/(d^2))
df <- nprev1 - 1
while ((ncurr != nprev1 ) && (ncurr != nprev2) && (iteration < 1001)) {
   iteration <- iteration + 1
   nprev2 <- nprev1
   nprev1 <- ncurr
   if (tails != 1) tconf <- abs(qt((1-conf)/2,df))
   if (tails == 1) tconf <- abs(qt((1-conf),df))
   tpower <- abs(qt((1-power),df))
   t <- tconf + tpower
   ncurr <- ceiling(2*(t^2)*var/(d^2))
   df <- ncurr -1
   }
cat("\nRESULTS\n\n")
if (ncurr == nprev2 && ncurr < nprev1) cat("No convergence, fluctuating between:",ncurr,"and",nprev1,"\n")
if (ncurr == nprev2 && ncurr < nprev1) ncurr <- nprev1
cat("Recommended sample size per group:",ncurr,"\n")
cat("Recommended total sample size:",2*ncurr,"\n")
cat("Number of iterations:",iteration,"\n")
if (iteration > 999) cat("WARNING: Over 1000 iterations -- problem converging \n")
cat("\n")
}

# Sample size estimation for two-sample t-test given confidence, power, variance, critical difference, and 1 vs 2 tailed
# conf is the desired level of confidence expressed as a number between 0 and 1 or between 1 and 100
# e.g., 95% confidence would be .95 or 95
# power is also a number between 0 and 1 or 1 and 100, always one-sided, 
# e.g., 50% confidence would be .50 or 50 (with z = 0)
# Uses iteration to estimate required sample size, max of 1000 iterations
#
n.t.twosample.givenequalvar <- function(conf,power,var,d,tails) {
if (conf > 1) conf <- conf/100
if (power > 1) power <- power/100
ncurr <- -1
nprev1 <- -2
nprev2 <- -3
iteration <- 0
if (tails != 1) zconf <- abs(qnorm((1-conf)/2))
if (tails == 1) zconf <- abs(qnorm((1-conf)))
zpower <- abs(qnorm((1-power)))
z <- zconf + zpower
nprev1 <- ceiling(2*(z^2)*var/(d^2))
df <- nprev1 - 1
while ((ncurr != nprev1 ) && (ncurr != nprev2) && (iteration < 1001)) {
   iteration <- iteration + 1
   nprev2 <- nprev1
   nprev1 <- ncurr
   if (tails != 1) tconf <- abs(qt((1-conf)/2,df))
   if (tails == 1) tconf <- abs(qt((1-conf),df))
   tpower <- abs(qt((1-power),df))
   t <- tconf + tpower
   ncurr <- ceiling(2*(t^2)*var/(d^2))
   df <- ncurr -1
   }
cat("\nRESULTS\n\n")
if (ncurr == nprev2 && ncurr < nprev1) cat("No convergence, fluctuating between:",ncurr,"and",nprev1,"\n")
if (ncurr == nprev2 && ncurr < nprev1) ncurr <- nprev1
cat("Recommended sample size per group:",ncurr,"\n")
cat("Recommended total sample size:",2*ncurr,"\n")
cat("Number of iterations:",iteration,"\n")
if (iteration > 999) cat("WARNING: Over 1000 iterations -- problem converging \n")
cat("\n")
}

# Probability of an event happening at least once
#
p.atleastonce <- function(p,n) {
prob <- 1 - (1 - p)^n
cat("\nRESULT\n\n")
cat("Probability of an event of p =",p,"happening at least once in",n,"trials:",prob,"\n")
cat("\n")
}

# Probability of x or more events
# The application is to assess the number of significant outcomes for multiple tests
# The inputs are the number of significant results given alpha, the number of tests run, alpha, 
# and a criterion for determining the critical value of x -- the value of x at which the likelihood of x or more events is less than the criterion
# For example, in Table 9.5 of Practical Statistics for User Research, the criterion was .10
#
p.xormore <- function(numsig,numtests,alpha,criterion) {
# Build array of binomial probabilities
binomprobs <- array(0,c(numtests+1,2))
x <- 0
prob <- 0
while (x <= numtests) {
   binomprobs[x+1,1] <- x
   binomprobs[x+1,2] <- dbinom(x,numtests,alpha)
   if (x >= numsig) prob <- prob + dbinom(x,numtests,alpha)
   x <- x+1
   }
x <- 0
stopsearching <- 0
while (x <= numtests) {
   if (sum(binomprobs[x:numtests+1,2]) <= criterion && stopsearching == 0) {
      critx <- x
      stopsearching <- 1
      }
   x <- x+1
   }
cat("\nRESULTS\n\n")
cat("Probability of",numsig,"or more significant outcomes given",numtests,"tests with alpha of",alpha,"is:",prob,"\n")
cat("For the criterion of",criterion,"the critical value of x is:",critx,"\n")
cat("\n")
}

# Standard error of the mean given an array of scores (x)
#
se.fromarray <- function(x) {
n <- length(x)
stder <- sd(x)/(n^.5)
cat ("\nRESULTS \n\n")
cat ("Std deviation:\t",sd(x),"\n")
cat ("Sample size:\t",n,"\n")
cat ("Standard error:\t",stder,"\n")
cat ("","\n")
}

# N-1 Two-Proportion Test with confidence interval given p and n
# Inputs are p1, n1, p2, n2, and conf
# conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
#
test.nminusonetwoproportion.givenpandn <- function(p1,n1,p2,n2,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
x1 <- p1 * n1
x2 <- p2 * n2
N <- n1 + n2
a <- x1
b <- n1 - x1
c <- x2
d <- n2 - x2
m <- a + b
n <- c + d
r <- a + c
s <- b + d
P <- (x1 + x2)/(n1 + n2)
Q <- 1 - P
testznum <- (p1 - p2)*((N-1)/N)^.5
testzden <- (P * Q * (1/n1 + 1/n2))^.5
testz <- testznum/testzden
ptestz <- 2*pnorm(-abs(testz))
z <- abs(qnorm((1-conf)/2))
p1adj <- (a + z^2/4)/(m + z^2/2)
p2adj <- (c + z^2/4)/(n + z^2/2)
n1adj <- m + z^2/2
n2adj <- n + z^2/2
diff <- p1 - p2
diffadj <- p1adj - p2adj
se <- ((p1adj*(1 - p1adj)/n1adj) + (p2adj*(1 - p2adj)/n2adj))^.5
critdiff <- z*se
upper <- diffadj + critdiff
lower <- diffadj - critdiff
cat("\nRESULTS\n\n")
cat ("N-1 Two-Proportion z:",testz,"\n")
cat ("p:",ptestz,"\n\n")
cat (confper,"confidence interval of difference between proportions","\n")
cat ("p1:",p1,"\n")
cat ("p2:",p2,"\n")
cat ("P:",P,"\n")
cat ("Adjusted value of p1:",p1adj,"\n")
cat ("Adjusted value of p2:",p2adj,"\n")
cat ("Adjusted difference:",diffadj,"\n")
cat ("Margin of error:",critdiff,"\n")
cat ("Upper limit:", upper, "\n")
cat ("Maximum likelihood estimate (observed difference):", diff, "\n")
cat ("Lower limit:", lower, "\n")
cat ("\n")
}

# N-1 Two-Proportion Test with confidence interval given x and n
# Inputs are x1, n1, x2, n2, and conf
# conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
#
test.nminusonetwoproportion.givenxandn <- function(x1,n1,x2,n2,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
a <- x1
b <- n1 - x1
c <- x2
d <- n2 - x2
m <- a + b
n <- c + d
r <- a + c
s <- b + d
N <- a + b + c + d
p1 <- a/m
p2 <- c/n
P <- (x1 + x2)/(n1 + n2)
Q <- 1 - P
testznum <- (p1 - p2)*((N-1)/N)^.5
testzden <- (P * Q * (1/n1 + 1/n2))^.5
testz <- testznum/testzden
ptestz <- 2*pnorm(-abs(testz))
z <- abs(qnorm((1-conf)/2))
p1adj <- (a + z^2/4)/(m + z^2/2)
p2adj <- (c + z^2/4)/(n + z^2/2)
n1adj <- m + z^2/2
n2adj <- n + z^2/2
diff <- p1 - p2
diffadj <- p1adj - p2adj
se <- ((p1adj*(1 - p1adj)/n1adj) + (p2adj*(1 - p2adj)/n2adj))^.5
critdiff <- z*se
upper <- diffadj + critdiff
lower <- diffadj - critdiff
cat("\nRESULTS\n\n")
cat ("N-1 Two-Proportion z:",testz,"\n")
cat ("p:",ptestz,"\n\n")
cat (confper,"confidence interval of difference between proportions","\n")
cat ("p1:",p1,"\n")
cat ("p2:",p2,"\n")
cat ("P:",P,"\n")
cat ("Adjusted value of p1:",p1adj,"\n")
cat ("Adjusted value of p2:",p2adj,"\n")
cat ("Adjusted difference:",diffadj,"\n")
cat ("Margin of error:",critdiff,"\n")
cat ("Upper limit:", upper, "\n")
cat ("Maximum likelihood estimate (observed difference):", diff, "\n")
cat ("Lower limit:", lower, "\n")
cat ("\n")
}

# Two-tailed between-subjects (independent) t-test with confidence interval given two arrays
# x and y are the two arrays; conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
#
test.t.independent.fromarrays <- function(x1,x2,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
u1 <- mean(x1)
u2 <- mean(x2)
d <- u1 - u2
sd1 <- sd(x1)
sd2 <- sd(x2)
n1 <- length(x1)
n2 <- length(x2)
var1 <- sd1^2
var2 <- sd2^2
se <- ((var1/n1) + (var2/n2))^.5
t <- d/se
swdfnum <- ((var1/n1) + (var2/n2))^2
swdfden <- ((var1/n1)^2/(n1-1)) + ((var2/n2)^2/(n2-1))
df <- floor(swdfnum/swdfden)
p <- 2*pt(-abs(t),df)
cat("\nRESULTS\n\n")
cat("t:",t,"\n")
cat("df:",df,"\n")
cat("p:",p,"\n\n")
critt <- abs(qt((1-conf)/2,df))
critd <- critt*se
lower <- d-critd
upper <- d+critd
cat (confper,"confidence interval \n")
cat ("Upper limit:", upper, "\n")
cat ("Mean difference:", d, "\n")
cat ("Lower limit:", lower, "\n")
cat ("Margin of error:",critd, "\n")
cat ("\n")
}

# Two-tailed between-subjects (independent) t-test with confidence interval given summary data 
# u1, sd1, and n1 are the mean, standard deviation, and sample size of the first group,
# u2, sd2, and n2 are the mean, standard deviation, and sample size of the second group,
# conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95),
# df computation uses the Welch-Satterthwaite procedure 
# (appropriate for unequal group variances -- slightly conservative if variances equal)
#
test.t.independent.fromsummary <- function(u1,sd1,n1,u2,sd2,n2,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
d <- u1 - u2
var1 <- sd1^2
var2 <- sd2^2
se <- ((var1/n1) + (var2/n2))^.5
t <- d/se
swdfnum <- ((var1/n1) + (var2/n2))^2
swdfden <- ((var1/n1)^2/(n1-1)) + ((var2/n2)^2/(n2-1))
df <- floor(swdfnum/swdfden)
p <- 2*pt(-abs(t),df)
cat("\nRESULTS\n\n")
cat("t:",t,"\n")
cat("df:",df,"\n")
cat("p:",p,"\n\n")
critt <- abs(qt((1-conf)/2,df))
critd <- critt*se
lower <- d-critd
upper <- d+critd
cat (confper,"confidence interval \n")
cat ("Upper limit:", upper, "\n")
cat ("Mean difference:", d, "\n")
cat ("Lower limit:", lower, "\n")
cat ("Margin of error:",critd, "\n")
cat ("\n")
}

# Two-tailed within-subjects (paired) t-test with confidence interval on difference scores
# d is a single array of difference scores; conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
#
test.t.paired.fromarray.ofdifferences <- function(d,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
n <- length(d)
mean <- mean(d)
sd <- sd(d)
se <- sd/(n^.5)
df <- n - 1
t <- mean/se
p <- 2*pt(-abs(t),df)
cat("\nRESULTS\n\n")
cat("t:",t,"\n")
cat("df:",df,"\n")
cat("p:",p,"\n\n")
critt <- abs(qt((1-conf)/2,df))
critd <- critt*se
lower <- mean-critd
upper <- mean+critd
cat (confper,"confidence interval \n")
cat ("Upper limit:", upper, "\n")
cat ("Mean difference:", mean, "\n")
cat ("Lower limit:", lower, "\n")
cat ("Margin of error:",critd, "\n")
cat ("\n")
}

# Two-tailed within-subjects (paired) t-test with confidence interval on two sets of dependent raw scores
# x and y are paired sets of scores; conf is the level for the confidence interval 
# expressed as a number between 0 and 1 (e.g., 95% confidence would be .95)
#
test.t.paired.fromarrays <- function(x,y,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
d <- x - y
n <- length(d)
mean <- mean(d)
sd <- sd(d)
se <- sd/(n^.5)
df <- n - 1
t <- mean/se
p <- 2*pt(-abs(t),df)
cat("\nRESULTS\n\n")
cat("t:",t,"\n")
cat("df:",df,"\n")
cat("p:",p,"\n\n")
critt <- abs(qt((1-conf)/2,df))
critd <- critt*se
lower <- mean-critd
upper <- mean+critd
cat (confper,"confidence interval \n")
cat ("Upper limit:", upper, "\n")
cat ("Mean difference:", mean, "\n")
cat ("Lower limit:", lower, "\n")
cat ("Margin of error:",critd, "\n")
cat ("\n")
}

# Two-tailed within-subjects (paired) t-test with confidence interval on difference score summary data
# difmean is the mean of the difference scores, difsd is their standard deviation,
# n is the sample size; conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
#
test.t.paired.fromsummary <- function(difmean,difsd,n,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
se <- difsd/(n^.5)
df <- n - 1
t <- difmean/se
p <- 2*pt(-abs(t),df)
cat("\nRESULTS\n\n")
cat("t:",t,"\n")
cat("df:",df,"\n")
cat("p:",p,"\n\n")
critt <- abs(qt((1-conf)/2,df))
critd <- critt*se
lower <- difmean-critd
upper <- difmean+critd
cat (confper,"confidence interval \n")
cat ("Upper limit:", upper, "\n")
cat ("Mean difference:", difmean, "\n")
cat ("Lower limit:", lower, "\n")
cat ("Margin of error:",critd, "\n")
cat ("\n")
}

# Test of two-by-two contingency table (dependent data)
# Focuses on McNemar Test (two-tailed) with confidence interval on difference in proportions
# a, b, c, and d refer to counts in the table's cells
# (a: upper left, b: upper right, c: lower left, d: lower right)
# b and c are the two different discordance counts
# conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95)
#
test.twobytwo.dependent <- function(a,b,c,d,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
m <- a + b
n <- c + d
r <- a + c
s <- b + d
N <- a + b + c + d
p1 <- m/N
p2 <- r/N
z <- abs(qnorm((1-conf)/2))
adj = z^2/8
aadj <- a + adj
badj <- b + adj
cadj <- c + adj
dadj <- d + adj
madj <- aadj + badj
nadj <- cadj + dadj
radj <- aadj + cadj
sadj <- badj + dadj
Nadj <- aadj + badj + cadj + dadj
p1adj <- madj/Nadj
p2adj <- radj/Nadj
p12adj <- badj/Nadj
p21adj <- cadj/Nadj
diffadj <- p2adj - p1adj
se <- (((p12adj + p21adj) - (p21adj - p12adj)^2)/Nadj)^.5
critdiff <- se*z
upper <- diffadj + critdiff
lower <- diffadj - critdiff
ndiscord <- b + c
smaller <- min(b,c)
bench <- .5
mle <- smaller/ndiscord
exactexceedsp <- pbinom(smaller-1,ndiscord,bench)
midpexceedsp <- pbinom(smaller-1,ndiscord,bench) + .5*dbinom(smaller,ndiscord,bench)
exactislessthanp <- 1 - pbinom(smaller,ndiscord,bench)
midpislessthanp <- 1 - midpexceedsp
mcnemarmidp <- 2*(1-midpislessthanp)
mcnemarexactp <- 2*(1- exactislessthanp)
mcnemarz <- (c - b)/(c + b)^.5
mcnemarzp <- 2*pnorm(-abs(mcnemarz))
cat("\nRESULTS\n\n")
if (mcnemarmidp <= 1) cat("McNemar Mid Probability (recommended):", mcnemarmidp, "\n\n") else cat("McNemar Mid Probability (recommended): 1 \n\n")
cat("Alternate analyses (not recommended) \n")
if (mcnemarexactp <= 1) cat("McNemar Exact Probability:", mcnemarexactp, "\n") else cat("McNemar Exact Probability: 1 \n")
mcnemarchisquared <- ((c - b)^2)/(c + b)
df <- 1
pmcnemarchisquared <- 1-pchisq(mcnemarchisquared,df)
cat("McNemar Chi-Squared:",mcnemarchisquared,"   df:",df,"   p:",pmcnemarchisquared,"\n")
cat("McNemar z:",mcnemarz,"   p:",mcnemarzp,"\n")
mcnemaryates <- ((abs(c - b)-1)^2)/(c + b)
pmcnemaryates <- 1-pchisq(mcnemaryates,df)
cat("McNemar Chi-Squared with Yates Correction:",mcnemaryates,"   df:",df,"   p:",pmcnemaryates,"\n\n")
cat(confper,"CONFIDENCE INTERVAL \n\n")
cat("p1:",p1,"\n")
cat("p2:",p2,"\n")
cat ("Adjusted value of p1:",p1adj,"\n")
cat ("Adjusted value of p2:",p2adj,"\n")
cat("Adjusted difference in p:",diffadj,"\n")
cat("Margin of error:",critdiff,"\n")
cat("Upper limit:",upper,"\n")
cat("Observed difference:",p2-p1,"\n")
cat("Lower limit:",lower,"\n")
cat("\n")
}

# Test of independence of two-by-two contingency table (independent data)
# a, b, c, and d refer to counts in the table's cells
# (a: upper left, b: upper right, c: lower left, d: lower right)
# conf is the level for the confidence interval 
# expressed as a number between 0 and 1 or between 1 and 100 (e.g., 95% confidence would be .95 or 95),
# Function returns results for N-1 chi-squared (recommended in most cases),
# standard chi-squared, standard chi-squared with Yates continuity correction,
# Fisher's exact probability test, and a confidence interval around the 
# difference in the proportions
#
test.twobytwo.independent <- function(a,b,c,d,conf) {
if (conf > 1) conf <- conf/100
confper <- paste(conf*100,"%",sep="")
m <- a + b
n <- c + d
r <- a + c
s <- b + d
N <- a + b + c + d
chisquared <- (((a*d - b*c)^2)*N)/(m*n*r*s)
df <- 1
pchisquared <- 1-pchisq(chisquared,df)
mat <- matrix(c(a,b,c,d), nr=2, dimnames=list(c("A1A1", "A1A2"), c("A1", "A2")))
chisquaredyates <- (((abs(a*d - b*c) - .5*N)^2)*N)/(m*n*r*s)
pchisquaredyates <- 1-pchisq(chisquaredyates,df)
nminusonechisquared <- (((a*d - b*c)^2)*(N-1))/(m*n*r*s)
pnminusonechisquared <- 1-pchisq(nminusonechisquared,df)
if (r*m/N > 1) ae <- 0 else ae <- 1
if (s*m/N > 1) be <- 0 else be <- 1
if (r*n/N > 1) ce <- 0 else ce <- 1
if (s*n/N > 1) de <- 0 else de <- 1
check <- ae + be + ce + de
if (check > 0) rec <- "Fisher Exact Test" else rec <- "N-1 Chi-Squared Test"
p1 <- a/m
p2 <- c/n
P <- (a + c)/(m + n)
Q <- 1 - P
testznum <- (p1 - p2)*((N-1)/N)^.5
testzden <- (P * Q * (1/m + 1/n))^.5
testz <- testznum/testzden
ptestz <- 2*pnorm(-abs(testz))
z <- abs(qnorm((1-conf)/2))
p1adj <- (a + z^2/4)/(m + z^2/2)
p2adj <- (c + z^2/4)/(n + z^2/2)
n1adj <- m + z^2/2
n2adj <- n + z^2/2
diff <- p1 - p2
diffadj <- p1adj - p2adj
se <- ((p1adj*(1 - p1adj)/n1adj) + (p2adj*(1 - p2adj)/n2adj))^.5
critdiff <- z*se
upper <- diffadj + critdiff
lower <- diffadj - critdiff
fish <- fisher.test(mat)
cat("\nRESULTS\n\n")
cat ("Recommended test:",rec,"\n\n")
cat ("N-1 chi-squared:",nminusonechisquared,"\n")
cat ("df:",df,"\n")
cat ("p:",pnminusonechisquared,"\n\n")
cat ("N-1 two-proportion z:",testz,"\n")
cat ("p:",ptestz,"\n\n")
cat (confper,"confidence interval of difference between proportions","\n")
cat ("p1:",p1,"\n")
cat ("p2:",p2,"\n")
cat ("Adjusted value of p1:",p1adj,"\n")
cat ("Adjusted value of p2:",p2adj,"\n")
cat ("Adjusted difference:",diffadj,"\n")
cat ("Margin of error:",critdiff,"\n")
cat ("Upper limit:", upper, "\n")
cat ("Maximum likelihood estimate (observed difference):", diff, "\n")
cat ("Lower limit:", lower, "\n\n")
cat ("Fisher Exact Probability:",fish$p.value,"\n\n")
cat ("Standard chi-squared:",chisquared,"\n")
cat ("df:",df,"\n")
cat ("p:",pchisquared,"\n\n")
cat ("Standard chi-squared with Yates correction:",chisquaredyates,"\n")
cat ("df:",df,"\n")
cat ("p:",pchisquaredyates,"\n")
cat ("\n")
}
