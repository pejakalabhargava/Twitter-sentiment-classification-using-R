#This installs all the required packages for the project
install.requiredPackages<- function(){
  #Install required packages
  install.packages("twitteR")
  install.packages("devtools")
  install.packages("ROAuth")
  install.packages("tm")
  install.packages("SnowballC")
  install.packages("RMOA")
}

#This installs  all the required packages for the project
load.requiredLibraries <-function(){
  library(twitteR)
  library(devtools)
  library(ROAuth)
  library(tm)
  library(RMOA)
}
