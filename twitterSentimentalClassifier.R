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
  return(tweets)
}

removeURL <- function(tweetText) {
<<<<<<< HEAD
  gsub("http[[:alnum:][:punct:]]+", "", tweetText)
=======
     gsub("http[[:alnum:][:punct:]]+", "", tweetText)
>>>>>>> 37f438ff4c0301d132fb209b8bfbeb468fd2a113
}

twitter.preprocessTweets <- function(tweets) {
  tweets = iconv(tweets, "ASCII", "UTF-8", sub="")
  tweets = iconv(tweets, "ISO_8859-2", "UTF-8",sub="")
  tweets = iconv(tweets, "LATIN2", "UTF-8",sub="")
  tweetCorpus <- Corpus(VectorSource(tweets),readerControl=list(language="en"))
  tweetCorpus <- tm_map(tweetCorpus, tolower)
  tweetCorpus <- tm_map(tweetCorpus, removeURL)
  tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
  tweetCorpus <- tm_map(tweetCorpus, removeNumbers)
  tweetCorpus <- tm_map(tweetCorpus, removeWords, c(stopwords("english"),"rt","http","retweet"))
  tweetCorpus <- tm_map(tweetCorpus, stemDocument, language="english")
  tweetCorpus <- tm_map(tweetCorpus, PlainTextDocument)
  return(tweetCorpus)
}

twitter.selectFeatures <- function(twitterDocMatrix,minfreq = 5) {
  #TODO : CHECK FOR PCA/ANy other method
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

twitter.getTestData <- function(trainDataFrame) {
  # Get tweets for the test data
  testTweets = twitter.getTweets(timeOut=10,noOfTweets=300)
  # Preprocess the test tweets
  testTweetCorpus = twitter.preprocessTweets(testTweets)
  # Construct the Doc Term Matrix
  testTwitterDocMatrix <- DocumentTermMatrix(testTweetCorpus, control = list(minWordLength = 1))
  testdoc.matrix= as.matrix(testTwitterDocMatrix)
  testdoc.dataFrame = twitter.appendClass(testdoc.matrix)
    
  # Columns to add to the test dataframe, ie, the ones in training data but not in test data
  trainDataFrameNames = colnames(trainDataFrame)
  colsToAdd = trainDataFrameNames[which(!trainDataFrameNames%in%colnames(testdoc.dataFrame))]
  
  # Add the required columns
  testdoc.dataFrame[, colsToAdd] = 0
  
  # Columns to remove from the test dataframe, ie, the ones in test data but not in train data
  testDataFrameNames = colnames(testdoc.dataFrame)
  colsToRemove = testDataFrameNames[which(!testDataFrameNames%in%trainDataFrameNames)]
  
  # Remove the unnecessary columns
  testdoc.dataFrame = testdoc.dataFrame[, -which(names(testdoc.dataFrame)%in%colsToRemove)]
  
  # Copy and remove the 'Class' column, since we need that to be the last 
  testdoc.dataFrame.class = testdoc.dataFrame$Class
  testdoc.dataFrame = testdoc.dataFrame[, -which(names(testdoc.dataFrame)%in%c("Class"))]
  
  # Sort the columns of the test data frame
  testdoc.dataFrame = testdoc.dataFrame[, order(names(testdoc.dataFrame))]
  
  # Append the Class column so that it is at the last position
  testdoc.dataFrame$Class = testdoc.dataFrame.class
  
  # remove class
  # Commented the below two since that will be taken care of while removing columns to match the train data
  # testdocWithoutClass.dataframe = subset(testdoc.dataFrame, select=-love)
  # testdocWithoutClass.dataframe = subset(testdoc.dataFrame, select=-hate)
  return(testdoc.dataFrame)
}

#End of function definitions

#---------------------------------------------------------------------------------------

#step 0: Load required Libraries
install.requiredPackages()
load.requiredLibraries()
#---------------------------------------------------------------------------------------

#Step1 : Authenticate
consumerKey <- "UPNJX5tOEJekgW3OWGzf10ewQ"
consumerSecret <- "uC2eORsbRkUb4QEORhvcZwfodh5XIR34mTGEoDzUe7R5JFdiVl"
options(httr_oauth_cache=T)
authentication_file = twitter.authenticate(consumerKey=consumerKey,consumerSecret = consumerSecret)
load(authentication_file)
#---------------------------------------------------------------------------------------

#Step2: Get all tweets filtered by 'love' or 'hate'
tweets = twitter.getTweets(timeOut=30,noOfTweets=3000)
#---------------------------------------------------------------------------------------

#step3: Preprocess Tweets
tweetCorpus = twitter.preprocessTweets(tweets)
#---------------------------------------------------------------------------------------

#step4: Create DocumentMatrix
twitterDocMatrix <- DocumentTermMatrix(tweetCorpus, control = list(minWordLength = 1))
#---------------------------------------------------------------------------------------

#step5: Feature Selection
#findAssocs(twitterDocMatrix, "love", 0.4)
doc.matrix = twitter.selectFeatures(twitterDocMatrix,minfreq = 3)
#---------------------------------------------------------------------------------------

#step6: Build Model

#Appened Class to the model
doc.dataFrame = twitter.appendClass(doc.matrix)

#remove love hate colum
doc.dataFrame = subset(doc.dataFrame, select=-love)
doc.dataFrame = subset(doc.dataFrame, select=-hate)

#Train the model using HoeffdingTree
doc.dataFrame <- factorise(doc.dataFrame)
#hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver", splitConfidence = "1")
hdt <- HoeffdingTree(splitConfidence="0.5")
datastream <- datastream_dataframe(data=doc.dataFrame)
model <- trainMOA(model=hdt, formula=Class ~ ., data=datastream,chunksize = 10)
model$model

#GetTestData
if (file.exists("tweets.json")) file.remove("tweets.json")
testdocWithoutClass.dataframe = twitter.getTestData(trainDataFrame = doc.dataFrame)
testdocWithoutClass.dataframe <- factorise(testdocWithoutClass.dataframe)
testdocWithoutClass.dataframe = testdocWithoutClass.dataframe[1:100, ]

#colnames(doc.dataFrame)[which(!colnames(doc.dataFrame)%in%colnames(testdocWithoutClass.dataframe))]
modelPredict <- predict(model, newdata = testdocWithoutClass.dataframe)

#Build contigency table
table(modelPredict,testdocWithoutClass.dataframe$Class)
 
#REBUILD MODEL
# Build the formula using the column names
cols=setdiff(colnames(testdocWithoutClass.dataframe),c('Class'))
allColumns = paste("Class ~ ", paste(cols, collapse= " + "))
formula.rebuildModel=as.formula(allColumns)

noOfIterations = 5
for(i in 1:noOfIterations) {
  
  # Rebuild the model by sending the existing set of testTweets as a stream
  datastream <- datastream_dataframe(data=testdocWithoutClass.dataframe)
  model <- trainMOA(model = model$model, formula=formula.rebuildModel, data = datastream, reset = F, chunksize = 10)
  
  #Test model against a new batch of tweets
  if (file.exists("tweets.json")) file.remove("tweets.json")
  testdocWithoutClass.dataframe = twitter.getTestData(doc.dataFrame)
  testdocWithoutClass.dataframe <- factorise(testdocWithoutClass.dataframe)
  testdocWithoutClass.dataframe = testdocWithoutClass.dataframe[1:100, ]
  
  # Predict the category for the new set of test tweets using the new model 
  modelPredict <- predict(model, newdata = testdocWithoutClass.dataframe)
  
  #Build contigency table
  table(modelPredict,testdocWithoutClass.dataframe$Class)
}
