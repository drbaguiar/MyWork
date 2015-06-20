# Load functions
source('functions.R')

# Load the data
df<-read.csv("D:/Data/climate_change.csv")

# Clean
df<-cleanit(df)

# Split Train on data up to and including 2006
dftrain<-subset(df,year<2007)
dftest<-subset(df,year>2006)

#Dep and Independent Vars
# define columns we will be working with
depvar <- 'temp'
indepvars <-c('mei', 'co2', 'ch4', 'n2o', 'cfc11', 'cfc12', 'tsi', 'aerosols')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

#Fit the model
fit<-lm(f1,data=dftrain)
fit
summary(fit)

# Check for multicolinearity
cor(dftrain[,3:10])

# Use only MEI, TSI, Aerosols and N2O as independent variables

#Dep and Independent Vars
# define columns we will be working with
depvar <- 'temp'
indepvars <-c('mei', 'n2o', 'tsi', 'aerosols')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

#Fit the model
fit<-lm(f1,data=dftrain)
fit
summary(fit)

# Step
depvar <- 'temp'
indepvars <-c('mei', 'co2', 'ch4', 'n2o', 'cfc11', 'cfc12', 'tsi', 'aerosols')
f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')

# Fit the model
stepfit <-step(lm(f1,data=dftrain))
summary(stepfit)

# Predict
temppredict<-predict(stepfit, dftest)

# Compute out-of-sample R^2
SSE = sum((temppredict - dftest$temp)^2)
SST = sum((mean(dftrain$temp) - dftest$temp)^2)
R2 = 1 - SSE/SST
R2

# Compute the RMSE
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE
