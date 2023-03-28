setwd("WORK_DIRECTORY")


#running some linear models for the top 40 parks
forty_parks <- read.csv('Tweets_per_country_cleaned.csv')

#running some linear models for the top 40 parks
library(ggpubr)

#spearman rank correlation for visitor numbers... all tweets
res2 <-cor.test(forty_parks$Twitter_use_rank.out.of.the.top.20.countries.for.twitter.use. , forty_parks$Park_tweets_rank,  method = "spearman")
res2 #p = 3.195e-05
plot(res2$method)
#ranks for a ggplot scatter
library(ggplot2)
windowsFonts(Times=windowsFont("Times New Roman"))

#SCATTER PLOT FOR ALL TWEETS COUNTS PER PARK VS PARK VISITATION
graph <- ggplot(forty_parks, aes(x=forty_parks$Twitter_use_rank.out.of.the.top.20.countries.for.twitter.use. , y=forty_parks$Park_tweets_rank, colour = forty_parks$Is_US)) +
  geom_point(size = 3, aes(col=forty_parks$Is_US))+ 
  geom_abline(intercept =0 , slope = 1, size= 1)
graph

graph + theme_classic() + 
  scale_color_manual(values = c("tomato","midnightblue"),name = "Park locations",
                     labels= c('Other', 'United States'),guide = guide_legend(reverse = TRUE))+
  theme(axis.text = element_text(colour = "black"))+
  ylab("Rank order of national park tweets per country") + xlab("Rank order of Twitter popularity per country")+
  theme(text=element_text(family="Times", size=25), axis.text = element_text(size = 25))+
  scale_shape_discrete(name= "Country")

