# Load functions
source('C:/Users/bryan_000/Documents/GitHub/MyWork/functions.R')

# Load Data
matches<-read.csv("D:/Data/ElectionMatchUp.csv")

# CLean Columns
matches <- cleanit(matches)

head(matches)
plot (matches$poll, matches$who)
plot(matches$who~matches$match)

table(matches$who,matches$poll)
