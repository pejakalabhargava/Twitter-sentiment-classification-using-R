#This method returns the stream of tweets filtered by the love/hate keywords.
twitter.getTweets <- function(consumer_api_key,consumer_api_secret,access_token,access_token_secret,noOfTweets=300) {
  # make sure twitter authentication is taken care
  setup_twitter_oauth(consumer_api_key, consumer_api_secret, access_token, access_token_secret)
  # get the love or hate tweets
  lovehatetweets = searchTwitter('hate OR love', noOfTweets, lang='en')
  # convert to data frame using twListToDF:function to convert twitteR lists to data.frames
  lovehatetweets = twListToDF(lovehatetweets)
  return (lovehatetweets$text)
}
