#This method is used to authenticate with the twitter so that
#there is an access to the twitter stream.
twitter.authenticate <- function(consumerKey,consumerSecret) {
  library(streamR)
  library(ROAuth)
  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                               requestURL = requestURL, accessURL = accessURL, authURL = authURL)
  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  #Saves the authentication data to the disk
  save(my_oauth, file = "my_oauth.Rdata")
  return("my_oauth.Rdata")
}

#This method returns the stream of tweets filtered by the love/hate keywords.
twitter.getTweets <- function(auth_object,timeOut=10,noOfTweets=300) {
  #Get all the tweets
  tweets = filterStream("", track = c("love","hate"), timeout = timeOut, oauth = auth_object, language="en", tweets=noOfTweets)
  #Parese the tweets
  tweets = parseTweets(tweets, simplify = TRUE)
  #return the tweet text to the caller
  return(tweets$text)
}
