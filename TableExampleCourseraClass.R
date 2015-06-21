# Load functions
source('functions.R')

#Load libraries
library(vcd)
library(exact2x2)

#data
#NE
#150 yes
#95 no
#NC
#181 yes
#204 no
#South
#448 yes
#605 no
#West
#343 yes
#214 no

# Build the data in a dataframe
df<- data.frame(cbind (area=rep("NE",150),response=rep(1,150)))
df1<- data.frame(cbind (area=rep("NE",95),response=rep(0,95)))
total <- rbind(df1, df)
df<- data.frame(cbind (area=rep("NC",181),response=rep(1,181)))
df1<- data.frame(cbind (area=rep("NC",204),response=rep(0,204)))
total <- rbind(total, df)
total <- rbind(total, df1)
df<- data.frame(cbind (area=rep("SOUTH",448),response=rep(1,448)))
df1<- data.frame(cbind (area=rep("SOUTH",605),response=rep(0,605)))
total <- rbind(total, df)
total <- rbind(total, df1)
df<- data.frame(cbind (area=rep("WEST",343),response=rep(1,343)))
df1<- data.frame(cbind (area=rep("WEST",214),response=rep(0,214)))
total <- rbind(total, df)
total <- rbind(total, df1)

# For more efficient analysis, transform the following 5 variables into factors:
total$response<- factor(total$response,levels=c(0,1),labels=c("No","Yes")) 

rm(df,df1)
summary(total)
x<- table(total$response,total$area)
barplot(x)


# Create a table with sums in margins
addmargins(x)

prop.table(x,1) #row
prop.table(x,2) #column

prop.table(addmargins(x),1) #row
prop.table(addmargins(x),2) #column

# Calculate Chi Square and Cramer's V
assocstats(x)

# Risk ratio
#riskratio(x)

# Odds ratio
oddsratio(x)

# Chi squared test
chisq.test(x)
chisq.test(x)$expected  # expected counts under the null
chisq.test(x)$observed  # observed counts (same as M)
chisq.test(x)$residuals # Pearson residuals
chisq.test(x)$stdres    # standardized residuals

# Data
# men
#62 smoke
#166 nonsmoker
# female
# 80 smoke
#203 nonsmoke

df<- data.frame(cbind (area=rep("male",62),response=rep(1,62)))
df1<- data.frame(cbind (area=rep("male",166),response=rep(0,166)))
total <- rbind(df1, df)
df<- data.frame(cbind (area=rep("female",80),response=rep(1,80)))
df1<- data.frame(cbind (area=rep("female",203),response=rep(0,203)))
total <- rbind(total, df)
total <- rbind(total, df1)

# For more efficient analysis, transform the following 5 variables into factors:
total$response<- factor(total$response,levels=c(0,1),labels=c("Nonsmoker","Smoker")) 

rm(df,df1)
summary(total)
x<- table(total$response,total$area)
barplot(x)

# Create a table with sums in margins
addmargins(x)
prop.table(x,1) #row
prop.table(x,2) #column

prop.table(addmargins(x),1) #row
prop.table(addmargins(x),2) #column

# Calculate Chi Square and Cramer's V
assocstats(x)

# Risk ratio
#riskratio(x)

# Odds ratio
oddsratio(x)

# Chi squared test
chisq.test(x)
chisq.test(x)$expected  # expected counts under the null
chisq.test(x)$observed  # observed counts (same as M)
chisq.test(x)$residuals # Pearson residuals
chisq.test(x)$stdres    # standardized residuals

# Exact McNemar test (with central confidence intervals) Only for 2x2 table
mcnemar.exact(x)