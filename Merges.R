# Clear the environment
rm(list=ls())

# Turn off scientific notations for numbers
options(scipen = 999)  

# Set locale
Sys.setlocale("LC_ALL", "English") 

# Set seed for reproducibility
set.seed(2345)

# Create the Dataframes for merger
x<- data.frame(k1=c(1,2,3),k2=c('a','b','c'),data =1:3)
y<- data.frame(k1=c(1,7,8,9,10),k3=c(5,6,7,8,9))

# Full Merge (all records merge)
merge (x,y,by.x="k1",by.y="k1",all=TRUE)

#Inner Merge (only common records)
merge (x,y,by.x="k1",by.y="k1")

#Outer Merge - left (only left records)
merge (x,y,by.x="k1",by.y="k1",all.x=TRUE)

#Outer Merge - right (only right records)
merge (x,y,by.x="k1",by.y="k1",all.y=TRUE)
