setwd("/Users/bkakran/Twitter-sentiment-classification-using-R")
source("setUp.R")
source("twitterUtility.R")
source("preProcess.R")
source("model.R")
source("twitterSentimentalClassifier.R")

#---------------------------------------------------------------------------------------
#step 0: Load required Libraries
install.requiredPackages()
load.requiredLibraries()
#---------------------------------------------------------------------------------------

consumerKey <- "UPNJX5tOEJekgW3OWGzf10ewQ"
consumerSecret <- "uC2eORsbRkUb4QEORhvcZwfodh5XIR34mTGEoDzUe7R5JFdiVl"
authentication_file = "my_oauth.Rdata"
load(authentication_file)
classifyTweets(numberOfTweetsForTest=300,NumberOfTweetsForTrain=3000,numberOfIterations = 4,splitConfidence="0.7",
               authentication_object=my_oauth)