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
consumerKey <- "YOUR CONSUMER KEY"
consumerSecret <- "YOUR CONSUMER SECRET"

# Enter the name of the twitter authentication file and load it
authentication_file = "my_oauth.Rdata"
load(authentication_file)

# Call the below function with the above parameters, as shown
classifyTweets(numberOfTweetsForTest=300,NumberOfTweetsForTrain=3000,numberOfIterations = 4,splitConfidence="0.7",
               authentication_object=my_oauth)