---
title: Tidy Tennis
authors: ["admin"]
date: '2019-04-13'
slug: tidy-tennis
categories: ["R"]
tags: ["tidytuesday"]
image:
  caption: ''
  focal_point: ''
---

I took tennis lessons for two weeks in middle school. That was enough. Thankfully, other folks stuck it out so I could plot their data for this week's #TidyTuesday exercise. I spent more time on this dot plot than I anticipated, but I think it turned out alright. I'm looking forward to digging in to some more modeling later this quarter for my MPA program, so some time on the basics now will definitely help later on. 


```r
library(tidyverse)
library(lubridate)
library(extrafont)
player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")
```




```r
cumulative_wins_age <- player_dob %>%
  mutate(age = round(age/365.25), grand_slam = ifelse(grepl("Australian Open", grand_slam), 
                                                      "Australian Open", grand_slam)) %>%
  left_join(grand_slams, by = "name") %>%
  select(name, grand_slam.x, age, gender) %>%
  distinct(name, grand_slam.x, age, gender)


rolling_win_pct <- cumulative_wins_age %>%
  arrange(desc(age)) %>%
  mutate(counter = 1, numtotal = 102, agerank = cumsum(counter), cum_pct = agerank/numtotal)

#Is it too late to win your first Grand Slam title? It's not too late... Until it is
gg <- ggplot(cumulative_wins_age, aes(x = age, fill = grand_slam.x, color = grand_slam.x)) +
  geom_dotplot(method = "histodot", binwidth = 1, stackdir = "centerwhole",
               stackgroups = TRUE, binpositions = "all", dotsize = .75) +
  labs(title = "Is it too late to win your first Grand Slam title?",
       subtitle = "Age of first-time champs",
       x = "Age at first Grand Slam win",
       caption = "Source: #TidyTuesday") +
  scale_fill_viridis_d()+
  scale_color_viridis_d(guide = FALSE) +
  guides(fill = guide_legend(title = "Venue")) +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text=element_text(size = 16, family="Roboto Condensed"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom")
gg
```

<img src="/post/2019-04-13-tidy-tennis_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r
gg2 <- ggplot(rolling_win_pct, aes(x = age, y = cum_pct)) +
  geom_smooth() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Quite Possibly",
       y = "Percent of Winners Older Than You",
       x = "Your Age",
       caption = "Source: TidyTuesday") +
  theme(text = element_text(size = 16, family = "Roboto Condensed"),
        plot.title = element_text(hjust = 0.5))

gg2
```

<img src="/post/2019-04-13-tidy-tennis_files/figure-html/unnamed-chunk-2-2.png" width="672" />

