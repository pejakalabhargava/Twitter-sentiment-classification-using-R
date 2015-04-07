#Install required packages
install.packages("streamR")
install.packages("devtools")
install.packages("ROAuth")
install.packages("tm")

library(streamR)
library(devtools)
library(ROAuth)
library(tm)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

#GIVE YOUR TWITTER CREDENTIALS HERE
consumerKey <- ""
consumerSecret <- ""
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")

load("my_oauth.Rdata")
# Have just downloaded 300 tweets. change 'tweets' parameter below to desired number of tweets you want
filterStream("tweets.json", track = c("love", "hate"), timeout = 120, oauth = my_oauth, language="en", tweets=300)

# SET YOUR WORKING DIRECTORY HERE
setwd('/home/nakul/NCSU_CS_Courses/CSC591-AdvancedAlgos/project3/Twitter-sentiment-classification-using-R/corpus')
# CREATE A DIRECTORY AND PUT YOUR TWEETS.JSON FILE. MENTION IT HERE
tweets <- Corpus(DirSource("/home/nakul/NCSU_CS_Courses/CSC591-AdvancedAlgos/project3/Twitter-sentiment-classification-using-R/corpus"), readerControl=list(language="en"))
summary(tweets)
tweets <- tm_map(tweets, stripWhitespace)
tweets <- tm_map(tweets, tolower)
tweets <- tm_map(tweets, removePunctuation)
tweets <- tm_map(tweets, removeNumbers)
tweets <- tm_map(tweets, removeWords, stopwords("english"))
tweets <- tm_map(tweets, stemDocument, language="english")
tweets <- tm_map(tweets, PlainTextDocument)

# tweetsDTM <- DocumentTermMatrix(tweets)
