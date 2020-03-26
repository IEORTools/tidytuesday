library(tidyverse)

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

### modify data
tbi_age$age_group <- as.factor(tbi_age$age_group)
levels(tbi_age$age_group) <- c("0-17","0-4","5-14","15-24","25-34","35-44","45-54","55-64","65-74","75+","Total")

tbi_year$date <- as.Date(strptime(tbi_year$year,"%Y"))

#### age group plots ####
ggplot(subset(tbi_age,!age_group=="Total"), aes(x=reorder(injury_mechanism, number_est), y=rate_est, fill=number_est)) +
  geom_bar(stat="identity") +
  facet_wrap(~age_group) +
  coord_flip()


#### year plots ####
ggplot(subset(tbi_year,!injury_mechanism=="Total"), aes(x=date, y=number_est, fill=rate_est)) +
  geom_bar(stat="identity") +
  facet_wrap(~injury_mechanism)



#### intentional harm chart ####

intharm <- tbi_age[which(tbi_age$injury_mechanism=="Intentional self-harm"),]

ggplot(subset(intharm,!age_group %in% c("0-17","0-4","Total")), aes(x=age_group, y=rate_est, fill=number_est)) +
  geom_bar(stat="identity") +
  facet_grid(type ~ .) +
  coord_flip() +
  xlab("Age Group") +
  ylab("Rate per 100,000 in 2014") +
  scale_fill_continuous(name="Est. Observed") +
  ggtitle("Traumatic Brain Injury by Intentional Self-Harm (2014)")
