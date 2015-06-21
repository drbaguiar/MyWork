#ID  	Identification Code                                    
#LOW		Low Birth Weight (0 = Birth Weight >= 2500g,1 = Birth Weight < 2500g)
#AGE		Age of the Mother in Years                              
#LWT		Weight in Pounds at the Last Menstrual Period           
#RACE	Race (1 = White, 2 = Black, 3 = Other)                  
#SMOKE   Smoking Status During Pregnancy (1 = Yes, 0 = No)       
#PTL		History of Premature Labor (0 = None  1 = One, etc.)    
#HT		History of Hypertension (1 = Yes, 0 = No)               
#UI		Presence of Uterine Irritability (1 = Yes, 0 = No)      
#FTV		Number of Physician Visits During the First Trimester (0 = None, 1 = One, 2 = Two, etc.)
#BWT		Birth Weight in Grams 

# Load functions
source('functions.R')

# Load the libraries
library(psych)
library(e1071)
library(caret)
library(fBasics)

# Load the data
#Load the birthweight data (lowbw.csv)
#df<-read.csv(file.choose())
df<-read.csv("D:/Data/lowbwt.csv")

# count blanks remove blanks
colSums(!is.na(df))
#df <- na.omit(df)
#colSums(!is.na(df))

# Clean
df<- cleanit(df)

# remove a column
df$id <-NULL
df$bwt <-NULL
  
# Create dummies
#df$white <- as.numeric(df$race == 1)
#df$black <- as.numeric(df$race == 2)
#df$other <- as.numeric(df$race == 3)

# Create factor
df$race <- factor(df$race, levels=c(1,2,3),labels=c("white","black","other")) # White referent group
df$smoke<- factor(df$smoke, levels=c(0,1),labels=c("nonsmoker","smoker")) #Non smoker referent group
df$ht<- factor(df$ht, levels=c(0,1),labels=c("noHT","yesHT")) # No hypertension referent group
df$ui<- factor(df$ui, levels=c(0,1),labels=c("noUI","yesUI")) # No Urinary Tract Infection referent group
df$ptl<- factor(df$ptl, levels=c(0,1),labels=c("noPM","yesPML")) 

#Dep and Independent Vars
# define columns we will be working with
depvar <- 'low'
#indepvar <- 'race'
indepvars <-c('lwt','race')

# two-way contingency table of categorical outcome and predictors we want
#  to make sure there are not 0 cells
xtabs(~get(depvar) + get(indepvars), data = df)

f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
 
#Fit the model
fit<-glm(f1,data=df,family=binomial)
#fit<-step(glm(low~lwt+race+age+smoke+ht+ftv+ptl,data=df,family=binomial),direction="both")

summary(fit) # display results
confint(fit) # 95% CI for the coefficients using profiled log-likelihood
confint.default(fit) # 95% CI for the coefficients using standard errors
#exp(coef(fit)) # exponentiated coefficients a.k.a odds ratios
#exp(confint(fit)) # 95% CI for exponentiated coefficients
exp(cbind(OR = coef(fit), confint(fit))) ## odds ratios and 95% CI together

d<-anova(fit,test='Chisq') # or d<-anova(fit,test='LRT')
d

# get LR
d$Deviance

# Get loglikelihood
logLik(fit) 

# Make a prediction for a black woman with lwt=100
test<-data.frame(lwt=100,race="black")
fitpred<-predict(fit,test,se.fit=TRUE)
pi <- cbind(Prob=fitpred$fit,LCL=fitpred$fit - fitpred$se.fit*1.96,UCL=fitpred$fit + fitpred$se.fit*1.96)
pi2 <- cbind(Prob=exp(pi[,1])/(1+exp(pi[,1])),LCL=exp(pi[,2])/(1+exp(pi[,2])),UCL=exp(pi[,3])/(1+exp(pi[,3])))

#Note 59% chance of having a low birthweight baby for a black woman with lwt=100
pi
pi2

#Week5
# Load the data
# Load the birthweight data (lowbw.csv)
#df<-read.csv(file.choose())
#df<-read.csv("D:/Data/lowbwt.csv")

# Clean
#df<- cleanit(df)

# remove a column
#df$id <-NULL
#df$bwt <-NULL

# Create binary for weight 0 in >= 110
df<-within(df, {lwd <- ifelse( lwt == 110 | lwt > 110, 0, 1)})

#Dep and Independent Vars
# define columns we will be working with
depvar <- 'low'
indepvar <- 'lwd'
indepvars <-c('lwd','age')
indepvarsinter <-c('lwd','age','lwd*age')

#Paste together dep and independents
f1 <- paste(depvar,paste(indepvar,collapse=' + '),sep=' ~ ')
f2 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
f3 <-paste(depvar,paste(indepvarsinter,collapse=' + '),sep=' ~ ')

#Fit the models
#fit<-step(glm(f1,data=df,family=binomial),direction="both")
fit1<-glm(f1,data=df,family=binomial)
fit2<-glm(f2,data=df,family=binomial)
fit3<-glm(f3,data=df,family=binomial)

reviewit <- function(fit) {
  print(summary(fit)) # display results
  print(confint(fit)) # 95% CI for the coefficients using profiled log-likelihood
  print(confint.default(fit)) # 95% CI for the coefficients using standard errors
  print(exp(cbind(OR = coef(fit), confint(fit)))) ## odds ratios and 95% CI together
  print(anova(fit,test='Chisq')) # or d<-anova(fit,test='LRT')
  # get LR
  #d$Deviance
  # Get loglikelihood
  print(logLik(fit))
}

reviewit(fit1)
reviewit(fit2)
reviewit(fit3)

# Make a prediction for a 30 year old using model 3
test2<-data.frame(age=30,lwd=1)
fitpred<-predict(fit3,test2,se.fit=TRUE)
pi <- cbind(Prob=fitpred$fit,LCL=fitpred$fit - fitpred$se.fit*1.96,UCL=fitpred$fit + fitpred$se.fit*1.96)
pi2 <- cbind(Prob=exp(pi[,1])/(1+exp(pi[,1])),LCL=exp(pi[,2])/(1+exp(pi[,2])),UCL=exp(pi[,3])/(1+exp(pi[,3])))

#Note 59% chance of having a low birthweight baby for a black woman with lwt=100
pi
pi2