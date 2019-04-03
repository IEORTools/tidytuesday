
suppressWarnings(library(tidyverse))
suppressWarnings(library(ggplot2))
suppressWarnings(library(plyr))
suppressWarnings(library(scales))
suppressWarnings(library(zoo))


bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

#### modify variables
bike_traffic$date_f <- strptime(bike_traffic$date, "%m/%d/%Y %H:%M:%S %p")
bike_traffic$date_f <- as.POSIXct(bike_traffic$date_f)

bike_traffic$week <- as.numeric(format(bike_traffic$date_f, "%U"))
bike_traffic$year <- as.numeric(format(bike_traffic$date_f, "%Y"))
bike_traffic$monthf <- format(bike_traffic$date_f, "%b")
bike_traffic$monthf <- as.factor(bike_traffic$monthf)
levels(bike_traffic$monthf) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
bike_traffic$weekdayf <- format(bike_traffic$date_f, "%a")
bike_traffic$weekdayf <- as.factor(bike_traffic$weekdayf)
levels(bike_traffic$weekdayf) <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
  
### tidy up the data
df <- bike_traffic

df$yearmonth <- as.yearmon(df$date_f)
df$yearmonthf <- factor(df$yearmonth)
df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # compute week number of month
df <- df[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "bike_count")]

### sample down, too big
set.seed(12345)
df.samp <- df[sample(1:10, nrow(df), replace=TRUE)==1,]
#df.samp <- df.samp[which(df.samp$bike_count<100),]

### Calendar heatmap 
# plot credit goes to https://margintale.blogspot.com/2012/04/ggplot2-time-series-heatmaps.html)
ggplot(df.samp, aes(monthweek, weekdayf, fill = bike_count)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="green", trans="log") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Number of Bikes", 
       fill="Count")
