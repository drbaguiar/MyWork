# Clear the environment
rm(list=ls())

# Turn off scientific notations for numbers
options(scipen = 999)  

# Set locale
Sys.setlocale("LC_ALL", "English") 

# Set seed for reproducibility
set.seed(2345)

# Load the libraries
library(rgl)
library(Hmisc)
library(nFactors)
library(FactoMineR)
library(Rcmdr)
library(vcd)
library(MASS)

# Load the data
df <-read.csv("d:/data/1.csv")

# clean the data names and data
names(df)<-tolower(names(df))
names(df) <- gsub("\\(","",names(df))
names(df) <- gsub("\\)","",names(df))
names(df) <- gsub("-","",names(df))
names(df) <- gsub(",","",names(df))
names(df) <- gsub("\\.","",names(df))

# Make and recode dummy variables
df$gasfueldummy <-as.numeric(df$fueltype)
df$gasfueldummy[df$gasfueldummy == 1] <- 0
df$gasfueldummy[df$gasfueldummy == 2] <- 1

df$maledummy<-as.numeric(df$gender)
df$maledummy[df$maledummy == 1] <- 0
df$maledummy[df$maledummy == 2] <- 1

df$marrieddummy<-as.numeric(df$married)
df$marrieddummy[df$marrieddummy == 2] <- 0

# do the random split (25% held out for test), put the label back into the data frame
df$istest <- runif(nrow(df))<0.25
df$datalabel <- ifelse(df$istest,"test data","train data")
dftrain = df[!df$istest,]
dftest = df[df$istest,]

# remove original columns for the dummies, istest, datatlabels, and policy number
drops <- c("gender","married","fueltype","policynumber","datalabel","istest")
dftrain<-dftrain[,!(names(dftrain) %in% drops)]
dftest<-dftest[,!(names(dftest) %in% drops)]

# attach for working
attach(dftrain)

# Explore the data
str(dftrain)
summary(dftrain)

#fIVE NUMBER SUMMARIES
fivenum(losses, na.rm = TRUE)
fivenum(age, na.rm = TRUE)
fivenum(vehicleage, na.rm = TRUE)
fivenum(yearsofdrivingexperience, na.rm = TRUE)
fivenum(numberofvehicles, na.rm = TRUE)

# Apply to all columns
sapply(dftrain, mean, na.rm=TRUE)
sapply(dftrain, sd, na.rm=TRUE)
sapply(dftrain, fivenum, na.rm=TRUE)

# aggregate group by
aggregate(dftrain, by=list(marrieddummy,gasfueldummy,maledummy),FUN=mean, na.rm=TRUE)
aggregate(dftrain, by=list(marrieddummy,gasfueldummy,maledummy),FUN=sd, na.rm=TRUE)
aggregate(dftrain, by=list(marrieddummy,gasfueldummy,maledummy),FUN=fivenum, na.rm=TRUE)

# Tables
table(age)
table(vehicleage)
table(yearsofdrivingexperience)
table(numberofvehicles)
table(gasfueldummy)
table(maledummy)
table(marrieddummy)

# REQUIRES Hmisc package
describe(dftrain)

# Pricipal Components Analysis
# princomp( ) function produces an unrotated principal component analysis.
fit <- princomp(dftrain, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# REQUIRES the FactoMiner package 
result <- PCA(dftrain) # graphs generated automatically

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit <- factanal(dftrain, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(dftrain),cex=.7) # add variable names

# Determine Number of Factors to Extract
# REQUIRES nfactors package
ev <- eigen(cor(dftrain)) # get eigenvalues
ap <- parallel(subject=nrow(dftrain),var=ncol(dftrain),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

## Correlation matrix (and grap)
cor(dftrain)

# Correlations with significance levels (p-values)
# REQUIRES Hmisc package
rcorr(as.matrix(dftrain))

# Visual
pairs(dftrain)

# Stripchart
stripchart(losses, method="stack", xlab="Amount", pch=1, col=2, main="Losses", offset=0.5)
stripchart(age~marrieddummy, method="stack", pch=c(1,2), col=marrieddummy+1, xlab="age", ylab="Married", main="Age by Married", offset=0.5)

# REQUIRES corrgram package
corrgram(df)
corrgram(df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)
corrgram(df, order=TRUE, lower.panel=panel.ellipse,upper.panel=panel.pts, text.panel=panel.txt,diag.panel=panel.minmax)

# Boxplots
boxplot(losses~marrieddummy)
boxplot(losses~maledummy)
boxplot(losses~gasfueldummy)
boxplot(losses~vehicleage,notch=TRUE, col=(c("gold","darkgreen")),)

# Kernel Density Plot
d <- density(age) # returns the density data 
plot(d) # plots the results
d<- density(losses) # returns the density data 
plot(d) # plots the results
d<- density(vehicleage) # returns the density data 
plot(d) # plots the results
d<- density(yearsofdrivingexperience) # returns the density data 
plot(d) # plots the results
d<- density(numberofvehicles) # returns the density data 
plot(d) # plots the results

# Make a 3d Plot
plot3d(losses,age,yearsofdrivingexperience,col=gasfueldummy+1)

# Another Spinning 3d Scatterplot
# REQUIRES Rcmdr package
scatter3d(losses, age, yearsofdrivingexperience)


# Build an train a regression model
# Stepwise Regression
# REQUIRES the MASS package
fit <- lm(losses~.,data=dftrain)
step <- stepAIC(fit, direction="both")
step$anova # display results

# Plots
outlierTest(fit)
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")

# distribution of studentized residuals
sresid <- studres(fit) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


# Be nice
detach(dftrain)