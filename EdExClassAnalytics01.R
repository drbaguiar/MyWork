# Load functions
source('functions.R')

# Load the data frame & clean names
df<- read.csv("D:/Data/who.csv")
df<-cleanit(df)

#subset data - pull out eurpoe
df2 <- subset(df, region=="Europe")

#write Europe
write.csv(df2,"D:/Data/who_europe.csv")

#Look at athe dataframe
str(df)
summary(df)

# statistics for under15 column
mean(df$under15)
sd(df$under15)
summary(df$under15)

#According to the summary, the minimum value was 13.120.  whhich country?
minctry<-which.min(df$under15) #Locate the row 
df$country[minctry] #find that row in the country column

#According to the summary, the minimum value was 49.99.  whhich country?
maxctry<-which.max(df$under15) #Locate the row 
df$country[maxctry] #find that row in the country column

#PLot income (gni) by fertility (X,Y)
plot(df$gni,df$fertilityrate)

#Based on the graph, we will subset the data by GNI > 10000 and Fertility rate > 2.5
outliers <-subset(df, gni>10000 & fertilityrate>2.5)

#Count the number of rows
nrow(outliers)

# Output just country name, GNI and fertility rate
outliers[c("country","gni","fertilityrate")]

#plot
plot(outliers$gni,outliers$fertilityrate)

hist(df$cellularsubscribers)
boxplot(df$lifeexpectancy~df$region)

#Table (counts)
table(df$region)

#Group by the second term, apply the third
tapply(df$lifeexpectancy,df$region,mean)
tapply(df$childmortality,df$region,min,na.rm=TRUE)


# Load the data frame & clean names
df<- read.csv("D:/Data/usda.csv")
df<-cleanit(df)
str(df)
summary(df)
