## Load the data
mydata <- read.csv("KidCreative.csv")

##Delete the first column which is observation number
mydata <- mydata[,-1]

##Here are the variables:
        
##Household Income (Income; rounded to the nearest $1,000.00)
##Gender (IsFemale = 1 if the person is female, 0 otherwise)
##Marital Status (IsMarried = 1 if married, 0 otherwise)
##College Educated (HasCollege = 1 if has one or more years of college education, 0 otherwise)
##Employed in a Profession (IsProfessional = 1 if employed in a profession, 0 otherwise)
##Retired (IsRetired = 1 if retired, 0 otherwise)
##Not employed (Unemployed = 1 if not employed,  0 otherwise)
##Length of Residency in Current City (ResLength; in years)
##Dual Income if Married (Dual = 1 if dual income, 0 otherwise)
##Children (Minors = 1 if children under 18 are in the household, 0 otherwise)
##Home ownership (Own = 1 if own residence, 0 otherwise)
##Resident type (House = 1 if residence is a single family house, 0 otherwise)
##Race (White = 1 if race is white, 0 otherwise)
##Language (English = 1 is the primary language in the household is English, 0 otherwise)

## suppressing scientific notation
options(scipen=999) 

##Build the model with all variables
mylogit <-glm(Buy~., data=mydata, family=binomial)

#View the results
summary(mylogit)


##Calculate Odds Ratio and 95% confidence intervals
exp(cbind(OR = coef(mylogit), confint(mylogit)))
##If you subtract 1 from the odds ratio and multiply by 100 
##(that is, (\text{odds ratio} - 1)\times 100), this shows the percentage change in the 
##odds for a 1-unit change in the X.
##cbind((exp(coef(mylogit))-1)*100)