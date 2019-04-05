---
title: Seattle Bike Path Traffic
subtitle: TidyTuesday data exploration
authors: ["admin"]
date: '2019-04-05'
categories: ["R"]
tags: ["#tidytuesday"]
---


Jumping into spring quarter of my MPA program and looking forward to digging into some data analysis as part of a self-directed study program. Just in time to get back to R, the weekly #TidyTuesday exercise dealt with two of my favorite things: Seattle and bikes. The data set was from several bike and pedestrian traffic counters in the Seattle-area. I'm sure I contributed to these counts when I was in Seattle until 2017. The data wasn't perfect, and if I had more time I'd dig into ways to control for incomplete sensor data, especially from the Burke-Gilman crossing. Also, I bet there is a big gap on weekend "fun" trips compared to commuting trips, especially on high traffic trails like the Burke-Gilman. The original data set was from 2014-2018, but I filtered to 2018 to compare to daily precipitation in the Seattle area. It made for a very predictable, yet still enjoyable visualization exercise.



```r
library(tidyverse)
library(lubridate)
library(grid)
library(extrafont)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")
weather <- readr::read_csv("../../static/data/seattle_weather.csv")


trip_data <- bike_traffic %>%
  separate(date, c("date", "time", "timestamp"), sep = " ") %>%
  mutate(date = mdy(date), ped_count = as.numeric(ped_count), bike_count = as.numeric(bike_count)) %>%
  group_by(date, crossing) %>%
  summarise(bike_trips = sum(bike_count), ped_trips = sum(ped_count)) %>%
  ungroup() %>%
  gather(key = "trip_type", value = "trips", bike_trips, ped_trips) %>%
  filter(date >= "2018-01-01", date <= "2018-12-31", trip_type == "bike_trips", trips <= 20000) %>%
  left_join(weather, by = c("date" = "DATE"))



#to combine
gg2 <- ggplot(trip_data, aes(date, trips))+
  geom_point(color = "gray", alpha = 0.2) +
  geom_smooth() +
  labs(title = "Fair Weather Bike Commuters Unite",
       y = "Number of bike trips") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size=16, family="Roboto Condensed"),
        plot.title = element_text(hjust = 0.5))



gg3 <- ggplot(trip_data, aes(date, PRCP))+
  geom_point(color = "gray", alpha = 0.2) +
  geom_smooth() +
  labs(caption = "Data from the Seattle Times & #tidytuesday", y = "Inches of precipitation", x = "Date") +
  theme_minimal() +
  theme(text = element_text(size = 16, family = "Roboto Condensed"))


grid.newpage()
grid.draw(rbind(ggplotGrob(gg2), ggplotGrob(gg3), size = "first"))
```

<img src="/post/2019-04-05-seattle-bike-trafic_files/figure-html/unnamed-chunk-1-1.png" width="672" />

