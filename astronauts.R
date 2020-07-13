library(tidyverse)
library(janitor)
library(knitr)
library(ggplot2)

astronauts.raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')


astronauts <- astronauts.raw %>% 
  clean_names() %>% 
  filter(!is.na(number)) %>%  # remove last row (all values are NA)
  mutate(
    sex = if_else(sex == "M", "male", "female"),
    military_civilian = if_else(military_civilian == "Mil", "military", "civilian")
  )

missions <- filter(astronauts, mission_number==1)

ggplot(filter(missions, total_number_of_missions>=5), aes(x=reorder(name, total_number_of_missions), y=total_number_of_missions, fill=nationality)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Salute to the 5 Timers Space Club") +
  theme(axis.title.y=element_blank())


temp <- filter(astronauts, name=="Young, John W.")
