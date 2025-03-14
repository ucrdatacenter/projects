 library(tidyverse)
#Upload the trump insult tweets csv file downloaded from Kaggle:
#https://www.kaggle.com/datasets/ayushggarg/all-trumps-twitter-insults-20152021?resource=downloadsxs

trump_insult_tweets <- read.csv("trump_insult_tweets_2014_to_2021.csv")

#From the dataset only keep the column that hold the actual tweets and filter out any duplicates or retweets
trump_insult_tweets_only <- trump_insult_tweets |> 
  select("tweet") |> 
  unique()

#Save the new da. aset
write_csv(trump_insult_tweets_only, "trump_insult_tweets_unique_2014_to_2021.csv")
view(trump_insult_tweets_unique_2014_to_2021.csv)
