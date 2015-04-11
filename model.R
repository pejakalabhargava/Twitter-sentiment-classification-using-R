
#This method appends the Class to the data set.The class is either 'love' or 'hate' based oh the
#presence of the correpsonding word in the tweet text.
twitter.appendClass <- function(doc.matrixToModify) {
  doc.dataFrame = as.data.frame(doc.matrixToModify)
  doc.dataFrame$Class = ''
  #Append love class
  doc.dataFrame[doc.dataFrame$love>0,'Class'] = 'love'
  #Append hate class
  doc.dataFrame[doc.dataFrame$hate>0,'Class'] = 'hate'
  doc.dataFrame[doc.dataFrame$Class=='','Class'] = 'love'
  return(doc.dataFrame)
}
# This method returns the test data for the model validation.Makes sure that the attribute set
# in the test data are same as the training data.
twitter.getTestData <- function(auth_object,trainDataFrame,numberOfTweets=10,timeOut=10) {
  # Get tweets for the test data
  testTweets = twitter.getTweets(auth_object,timeOut=timeOut,noOfTweets=numberOfTweets)
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
  
  return(testdoc.dataFrame)
}

#This method is used to get the performance measure based on the confusion matrix
getPerformanceMeasure = function(TP=0,FP=0,FN=0,TN=0){
  Accuracy = (TP+TN)/(TP+FP+TN+FN)
  Precision = TP/(TP+FP)
  Recall = Sensitivity = TP/(TP+FN)
  F1_measure = 2*Precision*Recall/ (Precision + Recall)
  Specificity = TN/(TN+FP)
  return(list(Accuracy=Accuracy,Precision=Precision,Recall=Recall,Sensitivity=Sensitivity,F1_measure=F1_measure,Specificity=Specificity))
}