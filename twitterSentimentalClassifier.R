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
  if (file.exists("tweets.json")) file.remove("tweets.json")
  filterStream("tweets.json", track = c("love","hate"), timeout = timeOut, oauth = my_oauth, language="en", tweets=noOfTweets)
  tweets <- parseTweets("tweets.json", simplify = TRUE)
  tweets = tweets$text
  tweets = iconv(tweets, "ASCII", "UTF-8", sub="")
  tweets = iconv(tweets, "ISO_8859-2", "UTF-8",sub="")
  tweets = iconv(tweets, "LATIN2", "UTF-8",sub="")
  return(tweets)
}

twitter.preprocessTweets <- function(tweets) {
  tweetCorpus <- Corpus(VectorSource(tweets),readerControl=list(language="en"))
  tweetCorpus <- tm_map(tweetCorpus, tolower)
  tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
  tweetCorpus <- tm_map(tweetCorpus, removeNumbers)
  tweetCorpus <- tm_map(tweetCorpus, removeNumbers)
  tweetCorpus <- tm_map(tweetCorpus, removeWords, c(stopwords("english"),"rt","http","retweet"))
  tweetCorpus <- tm_map(tweetCorpus, stemDocument, language="english")
  tweetCorpus <- tm_map(tweetCorpus, PlainTextDocument)
  return(tweetCorpus)
}

twitter.selectFeatures <- function(twitterDocMatrix,minfreq = 5) {
  frequentTerms =findFreqTerms(twitterDocMatrix, lowfreq=minfreq)
  dm.matrix = as.matrix(twitterDocMatrix)
  dm.matrix =  dm.matrix[,frequentTerms]
  return(dm.matrix)
}

twitter.appendClass <- function(doc.matrixToModify) {
  doc.dataFrame = as.data.frame(doc.matrixToModify)
  doc.dataFrame$Class = ''
  doc.dataFrame[doc.dataFrame$love>0,'Class'] = 'love'
  doc.dataFrame[doc.dataFrame$hate>0,'Class'] = 'hate'
  doc.dataFrame[doc.dataFrame$Class=='','Class'] = 'love'
  return(doc.dataFrame)
}

twitter.getTestTweetMatrix <- function() {
  testTweets = twitter.getTweets(timeOut=10,noOfTweets=300)
  testTweetCorpus = twitter.preprocessTweets(testTweets)
  testTwitterDocMatrix <- DocumentTermMatrix(testTweetCorpus, control = list(minWordLength = 1))
  testdoc.matrix= as.matrix(testTwitterDocMatrix)
  return(testdoc.matrix)
}


#End of function definitions

#---------------------------------------------------------------------------------------

#step 0: Load required Libraries
install.requiredPackages
load.requiredLibraries()
#---------------------------------------------------------------------------------------

#Step1 : Authenticate
consumerKey <- "AZ5TeacQBwKvjljHdzMytp1GL"
consumerSecret <- "noJOtlBCE0tRf1dNUJBm7ilp5reS627TjuawPpkn5cfHATQzNS"
authentication_file = twitter.authenticate(consumerKey=consumerKey,consumerSecret = consumerSecret)
load(authentication_file)
#---------------------------------------------------------------------------------------

#Step2: Get all tweets filtered by 'love' or 'hate'
tweets = twitter.getTweets(timeOut=30,noOfTweets=3000)
#---------------------------------------------------------------------------------------

#step3: Preprocess Tweets
tweetCorpus = twitter.preprocessTweets(tweets)
summary(tweetCorpus)
#---------------------------------------------------------------------------------------

#step4: Create DocumentMatrix
twitterDocMatrix <- DocumentTermMatrix(tweetCorpus, control = list(minWordLength = 1))
#---------------------------------------------------------------------------------------

#step5: Feature Selection
#findAssocs(twitterDocMatrix, "love", 0.4)
doc.matrix = twitter.selectFeatures(twitterDocMatrix,minfreq = 5)
#---------------------------------------------------------------------------------------

#step6: Build Model

allColumns = paste("Class ~ ", paste(colnames(doc.matrix), collapse= "+"))
gl.fmla <<- as.formula(allColumns)

#Appened Class to the model
doc.dataFrame = twitter.appendClass(doc.matrix)
length(which(doc.dataFrame$Class=='hate'))
length(which(doc.dataFrame$Class=='love'))

#remove love hate colum
doc.dataFrame = subset(doc.dataFrame, select=-love)
doc.dataFrame = subset(doc.dataFrame, select=-hate)



#Train the model using HoeffdingTree
doc.dataFrame <- factorise(doc.dataFrame)
#hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver", splitConfidence = "1")
hdt <- HoeffdingTree(splitConfidence="0.9")
datastream <- datastream_dataframe(data=doc.dataFrame)
model <- trainMOA(model=hdt, formula=Class ~ ., data=datastream)
model$model

#GetTestData
testdoc.matrix = twitter.getTestTweetMatrix()
testdoc.dataFrame = twitter.appendClass(testdoc.matrix)

length(which(testdoc.dataFrame$Class=='hate'))
length(which(testdoc.dataFrame$Class=='love'))


names = colnames(doc.dataFrame)
colsToAdd = names[which(!names%in%colnames(testdoc.dataFrame))]
testdoc.dataFrame[,colsToAdd]=0

#remove class
testdocWithoutClass.dataframe = subset(testdoc.dataFrame, select=-love)
testdocWithoutClass.dataframe = subset(testdoc.dataFrame, select=-hate)


testdocWithoutClass.dataframe <- factorise(testdocWithoutClass.dataframe)

#colnames(testdoc.dataframe)[which(!colnames(testdoc.dataframe)%in%colnames(testdocWithoutClass.dataframe))]
modelPredict <- predict(model, newdata = testdocWithoutClass.dataframe)

#Build contigency table
table(modelPredict,testdoc.dataFrame$Class)
#---------------------------------------------------------------------------------------