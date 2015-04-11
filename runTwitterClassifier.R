# Set the working directory to the directory that contains the submission files
setwd("/Users/bkakran/Twitter-sentiment-classification-using-R")

# Source the below files
source("setUp.R")
source("twitterUtility.R")
source("preProcess.R")
source("model.R")
source("twitterSentimentalClassifier.R")

#---------------------------------------------------------------------------------------
#Install and load required Libraries
install.requiredPackages()
load.requiredLibraries()
#---------------------------------------------------------------------------------------

# Enter your consumer key and secret
consumer_api_key = "AZ5TeacQBwKvjljHdzMytp1GL"
consumer_api_secret = "noJOtlBCE0tRf1dNUJBm7ilp5reS627TjuawPpkn5cfHATQzNS"
access_token = "61251563-qyleRuLgWPWpWkmsuq99oSpOhk0s3tuE1KI4o0Zwq"
access_token_secret = "Rl1pSppW8EpaUoKhibEUEHxgLzY68i8M05ClPFYH1W9GI"

# Call the below function with the above parameters, as shown
classifyTweets(numberOfTweetsForTest=200,NumberOfTweetsForTrain=600,numberOfIterations = 2,splitConfidence="0.7",
               consumer_api_key,consumer_api_secret,access_token,access_token_secret)