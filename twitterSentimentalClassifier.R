
#This method is used to classify the tweets as love or hate
classifyTweets = function(numberOfTweetsForTest=300,NumberOfTweetsForTrain=3000,
                          numberOfIterations = 2,splitConfidence="0.5",authentication_object=NULL,consumerKey=NULL,consumerSecret=NULL) {
  #-----------------------------------------------------------------------------------------------
  #Step1 : Authenticate
 
  print("---------------------------------------------------------------------------------------")
  print("Step1: Authenticating with twitter to get the tweets")
  options(httr_oauth_cache=T)
  if(is.null(authentication_object)) {
    print("Connecting to twitter to get the PIN code based on consumerKey and consumerSecret")
    authentication_file = twitter.authenticate(consumerKey=consumerKey,consumerSecret = consumerSecret)
  }
  print("Loading the Authentication File for twitter stream access.")
  load(authentication_file)
  print("---------------------------------------------------------------------------------------")
  
  #------------------------------------------------------------------------------------------------
  #Step2: Get all tweets filtered by 'love' or 'hate'
  print(paste("Step 2:Capturing tweets for training with NumberOfTweetsForTrain =",NumberOfTweetsForTrain))
  tweets = twitter.getTweets(auth_object = authentication_object,timeOut=60,noOfTweets=NumberOfTweetsForTrain)
  print("---------------------------------------------------------------------------------------")
  
  #------------------------------------------------------------------------------------------------
  
  #step3: Preprocess Tweets
  print("Step3 :Preprocessing Tweets")
  tweetCorpus = twitter.preprocessTweets(tweets)
  print("---------------------------------------------------------------------------------------")
  
  #---------------------------------------------------------------------------------------
  
  #step4: Create DocumentMatrix
  print("Step4 :Creating Document Matrix for tweet corpus.")
  twitterDocMatrix <- DocumentTermMatrix(tweetCorpus, control = list(minWordLength = 1))
  print("---------------------------------------------------------------------------------------")
  
  #---------------------------------------------------------------------------------------
  
  #step5: Feature Selection
  print("Step5 :Generate feature selection matrix with minfreq as 3")
  doc.matrix = twitter.selectFeatures(twitterDocMatrix,minfreq = 3)
  print("---------------------------------------------------------------------------------------")
  
  #---------------------------------------------------------------------------------------
  
  #step6: Build Model
  print("Step6 :Start Model Building phase.")
  
  #Appened Class to the model
  print("Append Class to the data.")
  doc.dataFrame = twitter.appendClass(doc.matrix)
  
  #remove love hate colum
  print("Removal of Love/Hate column from the data.")
  doc.dataFrame = subset(doc.dataFrame, select=-love)
  doc.dataFrame = subset(doc.dataFrame, select=-hate)
  
  #Train the model using HoeffdingTree
  print(paste("Generate HoeffdingTree tree with splitConfidence as :",splitConfidence))
  doc.dataFrame <- factorise(doc.dataFrame)
  #hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver", splitConfidence = "1")
  hdt <- HoeffdingTree(splitConfidence=splitConfidence)
  datastream <- datastream_dataframe(data=doc.dataFrame)
  print("Generate the model using the training data")
  model <- trainMOA(model=hdt, formula=Class ~ ., data=datastream,chunksize = 10)
  
  #Start:Debug
  #model$model
  #colnames(doc.dataFrame)[which(!colnames(doc.dataFrame)%in%colnames(testdocWithoutClass.dataframe))]
  #End:Debug
  
  print("Step6 :End of Model Building phase.")
  print("---------------------------------------------------------------------------------------")
  
  #---------------------------------------------------------------------------------------
  
  #step7: TestModel
  print("Step7 :Test the model with new set of tweet stream.")
  print(paste("parameters are NumberOfIterations =",numberOfIterations," and number of tweets for testing is:",numberOfTweetsForTest))
  
  for(i in 1:numberOfIterations) {
    print("---------------------------------------------------------------------------------------")
    print(paste("Start of iteration:",i))
    
    print("Get test tweets for classification.");
    #Test model against a new batch of tweets
    testData.dataframe = twitter.getTestData(auth_object = authentication_object,doc.dataFrame,numberOfTweets=numberOfTweetsForTest,timeOut=40)
    testData.dataframe <- factorise(testData.dataframe)

    print("Predict the class for the test data using the model.");
    #Predict the category for the new set of test tweets using the new model 
    modelPredict <- predict(model, newdata = testData.dataframe)
    
    confusionMat = table(modelPredict,testData.dataframe$Class)
    #Build contigency table
    print("Confusion matrix is as follows:")
    print(table(modelPredict,testData.dataframe$Class))
    if(ncol(confusionMat) == 2 && nrow (confusionMat) == 2) {
      perfMeasure = getPerformanceMeasure(TP=confusionMat[1,1],FP=confusionMat[1,2],FN=confusionMat[2,1],TN=confusionMat[2,2]);
      print(paste("Accuracy for the ",i," iteration is:",perfMeasure$Accuracy*100))
    } else {
      print("The confusion matrix is not 2*2")
    }
    # Rebuild the model by sending the existing set of testTweets as a stream
    print("Update the model with the test data.")
    datastream <- datastream_dataframe(data=testData.dataframe)
    
    #Build the formula using the column names
    cols=setdiff(colnames(doc.dataFrame),c('Class'))
    allColumns = paste("Class ~ ", paste(cols, collapse= " + "))
    formula.rebuildModel=as.formula(allColumns)
    
    model <- trainMOA(model = model$model, formula=Class ~ ., data = datastream, reset = F, chunksize = 10)
    
    print(paste("End of iteration:",i))
  }
  print("Step7 :End of test phase.")
  print("---------------------------------------------------------------------------------------")
}
