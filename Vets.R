# Load functions
source('functions.R')

# Load the libraries
library(e1071)
library(epitools)

# Build the data
# Place the referent group first
# Place the dependent variable 0 group first
df0<- data.frame(cbind (Ind=rep("NotVet",35),Dep=rep("NotSick",35)))
df1<- data.frame(cbind (Ind=rep("NotVet",15),Dep=rep("Sick",15)))
total <- rbind(df0, df1)
df0<- data.frame(cbind (Ind=rep("Vet",10),Dep=rep("NotSick",10)))
df1<- data.frame(cbind (Ind=rep("Vet",40),Dep=rep("Sick",40)))
total <- rbind(total, df0)
total <- rbind(total, df1)

#Build a table
x<-table(total$Dep,total$Ind,deparse.level = 2)
x
addmargins(x)
prop.table(x,1) #row
prop.table(x,2) #column
prop.table(addmargins(x),1) #row
prop.table(addmargins(x),2) #column

#Graphics
mosaicplot(x)
barplot(x)

#Odds ratio and confidence intervals (IF CONTAI 1 NOT SIGN)
oddsratio.wald(x) ## Or You can use epitab(x)

#Chi squared test
chisq.test(x)
chisq.test(x)$expected  # expected counts under the null
chisq.test(x)$observed  # observed counts (same as M)
chisq.test(x)$residuals # Pearson residuals
chisq.test(x)$stdres    # standardized residuals

#Fit the model
fit<-glm(Dep~Ind,data=total,family=binomial(link="logit"))
fit
summary(fit)
cbind(Coeff = coef(fit),confint(fit)) # 95% CI for the coefficients using profiled log-likelihood
cbind(Coeff = coef(fit),confint.default(fit)) # 95% CI for the coefficients using standard errors

exp(cbind(OR = coef(fit), confint(fit))) ## odds ratios and 95% CI together
exp(cbind(OR = coef(fit), confint.default(fit))) ## odds ratios and 95% CI together using standard errors

# Get loglikelihood
logLik(fit) 
