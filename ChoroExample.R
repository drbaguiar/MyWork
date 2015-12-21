# Load functions
#source('functions.R')

library(rUnemploymentData)

# Load the data
data(df_county_unemployment)
#df_county_unemployment<-cleanit(df_county_unemployment)
summary(df_county_unemployment)

# count blanks remove blanks
barplot(colSums(!is.na(df_county_unemployment)))
#df <- na.omit(df)
colSums(!is.na(df_county_unemployment))


# View the data
boxplot(df_county_unemployment[, c(-1, -2, -3)],main="USA County Unemployment Data",xlab="Year",ylab="Percent Unemployment")
county_unemployment_choropleth(year=2008)
#waanimated_county_unemployment_choropleth()
