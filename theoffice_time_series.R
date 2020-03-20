library(tidyverse)
library(grid)
library(gridExtra)

setwd("/mnt/Storage_2TB/Documents/Rscripts/tidyTuesday")

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

office_ratings$season_yr <- format(office_ratings$air_date, "%Y")
office_ratings$season_mo <- format(office_ratings$air_date, "%m")





p1 <- ggplot(office_ratings, aes(x=episode, y=total_votes,  fill=imdb_rating )) +
  geom_bar(stat = "identity") +
  facet_wrap(~paste("season",season)) +
  ggtitle("The Office Total Votes")

episodefinale <- tapply(office_ratings$episode, list(season=office_ratings$season), max)
office_ratings$finale <- ifelse(office_ratings$episode==rep(episodefinale, episodefinale),1,0)
datafinale <- office_ratings[which(office_ratings$finale==1),]

p2 <- ggplot(office_ratings, aes(x=log(total_votes), y=imdb_rating)) +
  geom_point(aes(color=factor(season))) +
  geom_smooth(method="lm") +
  geom_point(data=datafinale, aes(x=log(total_votes), y=imdb_rating, color=factor(season)), size=4 )

plotlist <- list(p1,p2)

ggsave("theoffice_timeseries.png", marrangeGrob(grobs=plotlist, nrow = 2, ncol = 1))

