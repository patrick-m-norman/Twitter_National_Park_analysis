#The following script reads in the raw tweets and compares them with the names of the commonly discussed National Parks
library(stringr)

tweets <- read.csv('RAW_tweets_spreadsheet.csv')
park_names<-read.csv('Common_park_names.csv')

#putting all the text column into caps so it won't through out the pull
tweets$text_lower <- sapply(tweets$text, tolower)

#identifying the park names in the text, then putting them into a separate column
tweets$the_common_parks = str_extract(tweets$text_lower, paste0(
  "\\b", park_names$park_name, "\\b", collapse = "|"
))

#Only tweets with the park names in them
tweets_cleaned <- tweets[!is.na(tweets$the_common_parks),]
write.csv(tweets_cleaned, "tweets_about_common_parks.csv")
