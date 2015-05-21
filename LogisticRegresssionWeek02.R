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

# Clear the environment
rm(list=ls())

# Turn off scientific notations for numbers
options(scipen = 999)  

# Set locale
Sys.setlocale("LC_ALL", "English") 

# Set seed for reproducibility
set.seed(2345)

# Load the libraries
library(psych)
library(e1071)
library(caret)
library(fBasics)

# Load the data
#Load the birthweight data (lowbw.csv)
df<-read.csv(file.choose())

# count blanks remove blanks
colSums(!is.na(df))
#df <- na.omit(df)
#colSums(!is.na(df))

# clean the data names and data
names(df) <-tolower(names(df))
names(df) <- gsub("\\(","",names(df))
names(df) <- gsub("\\)","",names(df))
names(df) <- gsub("\\.","",names(df))
names(df) <- gsub("_","",names(df))
names(df) <- gsub("-","",names(df))
names(df) <- gsub(",","",names(df))

# remove a column
df$id <-NULL
df$bwt <-NULL
  
# Create dummies
df$white <- as.numeric(df$race == 1)
df$black <- as.numeric(df$race == 2)
df$other <- as.numeric(df$race == 3)

# Create factor
df$race <- factor(df$race, levels=c(1,2,3),labels=c("white","black","other"))
df$smoke<- factor(df$smoke, levels=c(0,1),labels=c("nonsmoker","smoker"))
df$ht<- factor(df$ht, levels=c(0,1),labels=c("noHT","yesHT"))
df$ui<- factor(df$ui, levels=c(0,1),labels=c("noUI","yesUI"))
df$ptl<- factor(df$ptl, levels=c(0,1),labels=c("noPM","yesPML"))

#Dep and Independent Vars
# define columns we will be working with
depvar <- 'low'
indepvar <- 'race'
indepvars <-c('lwt','race','age','smoke','ht','ftv')

# two-way contingency table of categorical outcome and predictors we want
#  to make sure there are not 0 cells
xtabs(~get(depvar) + get(indepvar), data = df)



f1 <- paste(depvar,paste(indepvars,collapse=' + '),sep=' ~ ')
 
#Fit the model
fit<-step(glm(f1,data=df,family=binomial),direction="both")

fit<-step(glm(low~lwt+race+age+smoke+ht+ftv+ptl,data=df,family=binomial),direction="both")

summary(fit) # display results
confint(fit) # 95% CI for the coefficients using profiled log-likelihood
confint.default(fit) # 95% CI for the coefficients using standard errors
#exp(coef(fit)) # exponentiated coefficients a.k.a odds ratios
#exp(confint(fit)) # 95% CI for exponentiated coefficients
exp(cbind(OR = coef(fit), confint(fit))) ## odds ratios and 95% CI together

d<-anova(fit,test='Chisq') # or d<-anova(fit,test='LRT')

# get LR
d$Deviance

# Get loglikelihood
logLik(fit) 

