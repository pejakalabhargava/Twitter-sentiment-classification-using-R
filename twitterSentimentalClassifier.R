#---------------------------------------------------------------------------------------
#Start of function definitions

install.requiredPackages<- function(){
  #Install required packages
  install.packages("streamR")
  install.packages("devtools")
  install.packages("ROAuth")
  install.packages("tm")
  install.packages("SnowballC")
  install.packages("RMOA")
}

load.requiredLibraries <-function(){
  library(streamR)
  library(devtools)
  library(ROAuth)
  library(tm)
  library(RMOA)
}

twitter.authenticate <- function(consumerKey,consumerSecret) {
  library(streamR)
  library(ROAuth)
  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                               requestURL = requestURL, accessURL = accessURL, authURL = authURL)
  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  save(my_oauth, file = "my_oauth.Rdata")
  return("my_oauth.Rdata")
}

twitter.getTweets <- function(timeOut=10,noOfTweets=300) {
  # Have just downloaded 300 tweets. change 'tweets' parameter below to desired number of tweets you want
  filterStream("tweets.json", track = c("love","hate"), timeout = timeOut, oauth = my_oauth, language="en", tweets=noOfTweets)
  tweets <- parseTweets("tweets.json", simplify = TRUE)
  tweets = tweets$text
  tweets = iconv(tweets, "ASCII", "UTF-8", sub="")
  return(tweets)
}

twitter.preprocessTweets <- function(tweets) {
  tweetCorpus <- Corpus(VectorSource(tweets),readerControl=list(language="en"))
  tweetCorpus <- tm_map(tweetCorpus, tolower)
  tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
  tweetCorpus <- tm_map(tweetCorpus, removeNumbers)
  tweetCorpus <- tm_map(tweetCorpus, removeWords, c(stopwords("english"),"rt","http","retweet"))
  tweetCorpus <- tm_map(tweetCorpus, stemDocument, language="english")
  tweetCorpus <- tm_map(tweetCorpus, PlainTextDocument)
  return(tweetCorpus)
}
#End of function definitions
#---------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#step 0: Load required Libraries
load.requiredLibraries()
#---------------------------------------------------------------------------------------
#Step1 : Authenticate
consumerKey <- "AZ5TeacQBwKvjljHdzMytp1GL"
consumerSecret <- "noJOtlBCE0tRf1dNUJBm7ilp5reS627TjuawPpkn5cfHATQzNS"
authentication_file = twitter.authenticate(consumerKey=consumerKey,consumerSecret = consumerSecret)
load(authentication_file)
#---------------------------------------------------------------------------------------
#Step2: Get all tweets filtered by 'love' or 'hate'
tweets = twitter.getTweets(timeOut=10,noOfTweets=300)
#---------------------------------------------------------------------------------------
#step3: Preprocess Tweets
tweetCorpus = twitter.preprocessTweets(tweets)
summary(tweetCorpus)
#---------------------------------------------------------------------------------------
#step4: Create DocumentMatrix
twitterDocMatrix <- TermDocumentMatrix(tweetCorpus, control = list(minWordLength = 1))
twitterDocMatrix
#findFreqTerms(twitterDocMatrix, lowfreq=15)
#findAssocs(twitterDocMatrix, "love", 0.4)
#---------------------------------------------------------------------------------------
#TODO
#step5: Feature Selection
#---------------------------------------------------------------------------------------
#TODO
#step6: Build Model
#---------------------------------------------------------------------------------------










# tweetsDTM <- DocumentTermMatrix(tweets)
