library(tidyverse)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")
hm <- horror_movies

head(hm[,1:6])
head(hm[,7:12])

### modify variables
hm$movie_run_time_num <- as.numeric(gsub(" min","", hm$movie_run_time))
hm$budget_num <- gsub("[[:punct:]]","", hm$budget)
hm$budget_num <- gsub("INR","", hm$budget_num)
hm$budget_num <- as.numeric(hm$budget_num)
hm$log_budget_num <- log(hm$budget_num)
hm$budget_unit <- substr(hm$budget,1,1)


#### budget vs rating #####
hmtemp <- hm[which(hm$budget_unit=="$" & hm$review_rating>0),]
head(hmtemp[,c("budget_num","review_rating")])

ggplot(hmtemp, aes(x=review_rating, y=log_budget_num)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  ggtitle("Tidy Tuesday Horror Movie Ratings vs Budget") +
  ylab("log(Budget)") +
  xlab("Movie Rating")


#### budget vs run time #####
hmtemp2 <- hm[which(hm$budget_unit=="$" & hm$movie_run_time_num>0),]

ggplot(hmtemp2, aes(x=log_budget_num, y=movie_run_time_num)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  xlab("Log(Budget $)") + ylab("Movie Run Time (min.)") +
  ggtitle("Tidy Tuesday: Horror Movie Budget vs Run Time")
 