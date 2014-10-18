if (!require("twitteR")) {
        install.packages("twitteR")
}

if (!require("ROAuth")) {
        install.packages("ROAuth")
}

if (!require("RCurl")) {
        install.packages("RCurl")
}

library("twitteR")
library("ROAuth")
library("RCurl")

## Download"cacert.pem" file if it does not exist
if(!file.exists("cacert.pem")){
       download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
 }

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

# Twitter Stuff
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "jYEGXw87lkgjitQqUIBVcnUzE"
consumerSecret <- "oOsXCQQvdzujOamHALWLWB4Hv22Whjilou7gcspTAk1jydTBcm"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
registerTwitterOAuth(twitCred)