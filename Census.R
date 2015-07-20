# Load functions
source('functions.R')

# Load the data
df1 = read.csv("D:/Data/censusdata/Unemployment.csv")
df2 = read.csv("D:/Data/censusdata/PovertyEstimates.csv")
df3 = read.csv("D:/Data/censusdata/PopulationEstimates.csv")
df4 = read.csv("D:/Data/censusdata/Education.csv")

df1<-cleanit(df1)
df2<-cleanit(df2)
df3<-cleanit(df3)
df4<-cleanit(df4)

df6<-merge(df2, df3, by= "fipstxt", all=TRUE)
df7<-merge(df1, df4, by= "fipscode", all=TRUE)

