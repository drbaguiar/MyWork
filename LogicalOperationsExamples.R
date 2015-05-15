# Clear the environment
rm(list=ls())

# Turn off scientific notations for numbers
options(scipen = 999)  

# Set locale
Sys.setlocale("LC_ALL", "English") 

# Set seed for reproducibility
set.seed(2345)

# Create the Vectors
Name<- c('Aryan', 'Gopal','Zubin','Ravi','Umesh','Anita')
Gender<-c('M','M','F','M','M','F')
Age<-c(20,21,24,26,26,23)
Income<-c(20,30,35,40,41,50)

# Create the Data Frame
Records<-data.frame(Name,Gender,Age,Income)

# Attach it
attach(Records)

# Remove the vectors
rm(Name)
rm(Gender)
rm(Age)
rm(Income)

#Look at Records
str(Records)
summary(Records)

# Make some Plots
plot(Age)
abline(h=mean(Age),col='red' )
plot(Income)
abline(h=mean(Income),col='red' )
plot(Gender)
plot(Income~Age)
abline(v=mean(Age),col='red',h=mean(Income) )
plot(Income~Gender)

# Find those records for people under 23
#DATAFRAME[ROW,COLUMN]
data1<- Records[Records$Age<23,]
data1
nrow(data1)

# Find those records for males older than 21
data2<- Records[Records$Gender=="M"&Records$Age>21,]
data2
nrow(data2)

# Create a Dummy Variable for Gender (using if statement)
Records$Gender_dummy<- ifelse(Records$Gender=="M",1,0) ##Assigns 1 to males 0 to females