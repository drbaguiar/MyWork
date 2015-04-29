#install.packages("rgl")
rm(list=ls())

# Load the libraries
library(rgl)
library(Hmisc)
library(nFactors)
library(FactoMineR)

# Set the seed for reproducibility
set.seed(1234)

# Load the data
df <-read.csv("1.csv")

# clean the data names and data
names(df)<-tolower(names(df))
names(df) <- gsub("\\(","",names(df))
names(df) <- gsub("\\)","",names(df))
names(df) <- gsub("-","",names(df))
names(df) <- gsub(",","",names(df))
names(df) <- gsub("\\.","",names(df))

# Make and recode dummy variables
df$gasfueldummy <-as.numeric(fueltype)
df$gasfueldummy[df$gasfueldummy == 1] <- 0
df$gasfueldummy[df$gasfueldummy == 2] <- 1

df$maledummy<-as.numeric(gender)
df$maledummy[df$maledummy == 1] <- 0
df$maledummy[df$maledummy == 2] <- 1

df$marrieddummy<-as.numeric(married)
df$marrieddummy[df$marrieddummy == 2] <- 0

# remove original columns for the dummies and policy number
drops <- c("gender","married","fueltype","policynumber")
df<-df[,!(names(df) %in% drops)]

# attach for working
attach(df)

# Explore the data
str(df)
summary(df)
sapply(df, mean, na.rm=TRUE)

# REQUIRES Hmisc package
describe(df)

# aggregate group by
aggregate(df, by=list(marrieddummy,gasfueldummy),FUN=mean, na.rm=TRUE)

# Pricipal Components Analysis
# princomp( ) function produces an unrotated principal component analysis.
fit <- princomp(df, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

# Determine Number of Factors to Extract
# REQUIRES nfactors package
ev <- eigen(cor(df)) # get eigenvalues
ap <- parallel(subject=nrow(df),var=ncol(df),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# REQUIRES the FactoMiner package 
result <- PCA(df) # graphs generated automatically

## Correlation matrix (and grap)
cor(df)

# Correlations with significance levels (p-values)
# REQUIRES Hmisc package
rcorr(as.matrix(df))

# Visual
pairs(df)

# REQUIRES corrgram package
corrgram(df)
corrgram(df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)
corrgram(df, order=TRUE, lower.panel=panel.ellipse,upper.panel=panel.pts, text.panel=panel.txt,diag.panel=panel.minmax)


boxplot(losses~marrieddummy)
boxplot(losses~maledummy)
boxplot(losses~gasfueldummy)

# Make a 3d Plot
plot3d(losses,age,years.of.driving.experience,col=gasfueldummy+1)