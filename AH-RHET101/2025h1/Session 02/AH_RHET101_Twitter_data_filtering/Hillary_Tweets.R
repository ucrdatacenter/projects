library(tidyverse)
#Upload the trump and hillary  tweets csv file downloaded from Kaggle:
#https://www.kaggle.com/datasets/benhamner/clinton-trump-tweets/data
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)

#Only keep the id, handle, and text colons
tweets_cleaned <- tweets |>
  select(id, handle, text)

#Create a data set of only realDonaldTrump handle tweets 
trump_tweets <- tweets_cleaned |>
  filter(handle == "realDonaldTrump")

#Create a data set of only HillaryClinton handle tweets
hillary_tweets <- tweets_cleaned |>
  filter(handle == "HillaryClinton")

#Only keep the text columns for trump tweets
trump_tweets_cleaned <- trump_tweets |>
  select(text)

#Only keep the text columns for hillary tweets
hillary_tweets_cleaned <- hillary_tweets |>
  select(text)

#Convert the trump tweets cleaned into a downloadable csv file
write.csv(trump_tweets_cleaned, "trump_tweets.csv")

#Convert the hillary tweets cleaned into a downloadable csv file
write.csv(hillary_tweets_cleaned, "hillary_tweets.csv")
