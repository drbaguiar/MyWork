## String to serach in Twitter
search.string <- "@delta"

##How many Tweets to get
no.of.tweets <- 1000

#Get the tweets
tweets <- searchTwitter(search.string, n=no.of.tweets, cainfo="cacert.pem", lang="en")
##Show the tweets
tweets