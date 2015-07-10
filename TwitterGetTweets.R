if (!require("twitteR")) {
  install.packages("twitteR")
}
library("twitteR")

# Twitter Stuff
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "jYEGXw87lkgjitQqUIBVcnUzE"
consumerSecret <- "oOsXCQQvdzujOamHALWLWB4Hv22Whjilou7gcspTAk1jydTBcm"
setup_twitter_oauth(consumerKey, consumerSecret, access_token=NULL, access_secret=NULL)

## String to serach in Twitter
search.string <- "@JAMIE"

##How many Tweets to get
no.of.tweets <- 1000

#Get the tweets
tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")

##Show the tweets
tweets
