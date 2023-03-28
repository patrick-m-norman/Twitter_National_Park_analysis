#The following scripts are to discover commonly discussed park names using an N-gram analysis

setwd("WORK_DIRECTORY")

library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(tidytext)
library(openair)

# Get the files names of the tweet spreadsheets
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#removing duplicates
myfiles_done <- myfiles[!duplicated(myfiles[c('status_url')]),]

#subsetting the data to the study period
myfiles_done$date <- as.Date(myfiles_done$time, format='%d/%m/%Y')

myfiles_done_6months <- selectByDate(myfiles_done, start = "2018-07-10", end = "2019-01-09")

#removing retweets
NP_rt <- str_detect(myfiles_done_6months$text, "RT @")
#rejoining this variable 
myfiles_done_6months$rts <- NP_rt

#selecting only the FALSE results
NP_no_retweets <- subset.data.frame(myfiles_done_6months, myfiles_done_6months$rts==FALSE)

#exporting the csv of 6 months,no retweets
write.csv(NP_no_retweets, 'np_6months_no_retweets.csv')

#number of users
num_of_users <- table(NP_no_retweets$from_user)
num_of_users_df <- as.data.frame(num_of_users)

#grabbing only the text column
np_tweets <- data_frame(text = NP_no_retweets$text)
#getting the text into a readable format for the bigram
np_tweets$Each_tweet <- 1: nrow(np_tweets)

#CONDUCTING A BIGRAM
NP_bigrams <- np_tweets %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

NP_bigrams_separated <- NP_bigrams %>%
  separate(bigram,c("word1", "word2", "word"), sep = " ")

NP_bigrams_filtered <- NP_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

NP_bigram_counts <- NP_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

write.csv(NP_bigram_counts, file = "twitter_NP_bigram.csv")

#TRI GRAM
NP_trigrams <- np_tweets %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

NP_trigrams_separated <- NP_trigrams %>%
  separate(trigram,c("word1", "word2", "word3"), sep = " ")

NP_trigrams_filtered <- NP_trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

NP_trigram_counts <- NP_trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

NP_trigram_counts
#cleaning for 'word3' being park
NP_trigrams_w_park <- str_detect(NP_trigram_counts$word3, "park|Park")
#rejoining this variable 
NP_trigram_counts$clean <- NP_trigrams_w_park

#selecting only the TRUE results
NP_trigram_clean <- subset.data.frame(NP_trigram_counts, NP_trigram_counts$clean==TRUE)

#Exporting the trigram results
write.csv(NP_trigram_clean, file = "twitter_NP_trigram.csv")



#Quad GRAM
NP_quadgrams <- np_tweets %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)

NP_quadgrams_separated <- NP_quadgrams %>%
  separate(quadgram,c("word1", "word2", "word3","word4"), sep = " ")

NP_quadgrams_filtered <- NP_quadgrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word)

NP_quadgram_counts <- NP_quadgrams_filtered %>% 
  count(word1, word2,word3, word4, sort = TRUE)

NP_quadgram_counts
#cleaning for 'word4' being park
library(stringr)
NP_quadgrams_w_park <- str_detect(NP_quadgram_counts$word4, "park|Park")
#rejoining this variable 
NP_quadgram_counts$clean <- NP_quadgrams_w_park

#selecting only the TRUE results
NP_quadgram_clean <- subset.data.frame(NP_quadgram_counts, NP_quadgram_counts$clean==TRUE)

write.csv(NP_quadgram_clean, file = "twitter_quadgram_np_noretweets_6months.csv")

#five GRAM
NP_fivegrams <- np_tweets %>%
  unnest_tokens(fivegram, text, token = "ngrams", n = 5)

NP_fivegrams_separated <- NP_fivegrams %>%
  separate(fivegram,c("word1", "word2", "word3","word4", "word5"), sep = " ")

NP_fivegrams_filtered <- NP_fivegrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word5 %in% stop_words$word)

NP_fivegram_counts <- NP_fivegrams_filtered %>% 
  count(word1, word2,word3, word4, word5, sort = TRUE)

NP_fivegram_counts
#cleaning for 'word5' being park
NP_fivegrams_w_park <- str_detect(NP_fivegram_counts$word5, "park|Park")
#rejoining this variable 
NP_fivegram_counts$clean <- NP_fivegrams_w_park

#selecting only the TRUE results
NP_fivegram_clean <- subset.data.frame(NP_fivegram_counts, NP_fivegram_counts$clean==TRUE)

write.csv(NP_fivegram_clean, file = "twitter_fivegram_np_noretweets_6months.csv")