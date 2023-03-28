#The following script was used to determine the users specific location to a single latitude and longitude

setwd("WORK_DIRECTORY")

library(dplyr)
library(ggmap)
library(tmaptools)
library(stringr)
library(shiny)

#reading in the data
tweets <- read.csv("RAW_tweet_data.csv")

#removing duplicates
tweets_done <- tweets[!duplicated(tweets[c('from_user_id_str')]),]
tweets_done_df <- as.data.frame(tweets_done)
#removing is the location hasn't been stated
tweets_fin <- tweets_done_df[!apply(is.na(tweets_done_df) | tweets_done_df$user_location == "", 1, all),]
tweets_fin_df <- as.data.frame(tweets_fin)
locations <- as.character(tweets_fin_df$user_location)
#getting rid of any weird characters that might throw off the program
tweets_fin_df$user_location2 <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", locations)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon

for(i in 1:nrow(tweets_fin_df)){
  # Print("Working...")
  result <- tryCatch(geocode_OSM(tweets_fin_df$user_location2[i], projection = "+proj=longlat +datum=WGS84",
                                 return.first.only = TRUE, details = FALSE, as.data.frame = FALSE,
                                 as.sf = FALSE, server = "http://nominatim.openstreetmap.org"),
                     warning = function(w) data.frame(coordinates = NA,unkown = NA))
  tweets_fin_df$coordinates[i] <- lapply(result[2],as.numeric)
}

#separating the coords column 
clean_coords <- data.frame(do.call('rbind', strsplit(as.character(tweets_fin_df$coordinates),',',fixed=TRUE)))
#removing the c() trash
clean_coords$lon <- gsub("[a-zA-Z() ]", "", clean_coords$X1)
clean_coords$lat <- gsub("[a-zA-Z() ]", "", clean_coords$X2)

tweets_fin_df$user_lon <- as.character(clean_coords$lon) 
tweets_fin_df$user_lat <- as.character(clean_coords$lat)

#cleaned out for just the user name, and user lat/lon
User_location_df <- tweets_fin_df[,c('from_user', 'user_lon', 'user_lat')]

#rejoing the new coordinates back onto the original dataframe
all_merged <- merge(x = tweets, y = User_location_df, by='from_user', all.x = TRUE)

# Write the tweet data with the geolocation into a spreadsheet.
library(data.table)
fwrite(all_merged, "output_file_name.csv")

