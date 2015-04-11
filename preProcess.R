#This mehod removes all the URLs from a tweet
removeURL <- function(tweetText) {
  gsub("http[[:alnum:][:punct:]]+", "", tweetText)
}

#This method preprocess the tweets
twitter.preprocessTweets <- function(tweets) {
  #get rid of latin words by cnverting to utf-8
  tweets = iconv(tweets, "LATIN1", "UTF-8",sub="")
  tweets = iconv(tweets, "LATIN2", "UTF-8",sub="")
  #Create corpus out of the tweets
  tweetCorpus <- Corpus(VectorSource(tweets),readerControl=list(language="en"))
  #Convert to lowercase
  tweetCorpus <- tm_map(tweetCorpus, content_transformer(tolower), lazy = TRUE)
  #Remove the URLs from the tweet
  tweetCorpus <- tm_map(tweetCorpus, removeURL)
  #Removes all the punctuation
  tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
  #Removes all the numbers
  tweetCorpus <- tm_map(tweetCorpus, removeNumbers)
  #removes the stop words 
  tweetCorpus <- tm_map(tweetCorpus, removeWords, c(stopwords("english"),"rt","http","retweet"))
  #Stem the document using SnowballC
  tweetCorpus <- tm_map(tweetCorpus, stemDocument)
  #Convert to plain text document
  tweetCorpus <- tm_map(tweetCorpus, PlainTextDocument,language="english")
  return(tweetCorpus)
}

#THis method is used to select features from the document term matrix of the tweets.
#The logic used is to eliminate all the attributes that have the total sum of the 
#column less than a given threshold as specified by minFreq paramater
twitter.selectFeatures <- function(twitterDocMatrix,minfreq = 3) {
  #Get terms with frequency as minFreq
  frequentTerms =findFreqTerms(twitterDocMatrix, lowfreq=minfreq)
  dm.matrix = as.matrix(twitterDocMatrix)
  #selct the subset of features
  dm.matrix =  dm.matrix[,frequentTerms]
  return(dm.matrix)
}
