library(tidyverse)
library(rpart)
library(rattle)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# summary(transit_cost)


### modify data
transit_cost$start_year <- as.numeric(transit_cost$start_year)
transit_cost$end_year <- as.numeric(transit_cost$end_year)
transit_cost$year <- as.numeric(transit_cost$year)

transit_cost$tunner_per_n <- as.numeric(gsub("[%]","",transit_cost$tunnel_per))
transit_cost$yrs_to_finish <- transit_cost$end_year-transit_cost$start_year
transit_cost$yrs_to_midpoint <- transit_cost$year-transit_cost$start_year


### decision tree

dtvars <- c("rr","length","tunner_per_n","yrs_to_finish"
            ,"yrs_to_midpoint","stations","ppp_rate","cost_km_millions")

fitDT <- rpart(cost_km_millions~., data=transit_cost[,dtvars])
fitDT

fancyRpartPlot(fitDT
               ,main="Cost per KM - Decision Tree"
               ,sub="Tidy Tuesday 1/5/2021 Transit Cost Project")
