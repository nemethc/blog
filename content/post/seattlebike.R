library(tidyverse)
library(lubridate)
library(grid)
library(extrafont)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")
weather <- readr::read_csv("static/data/seattle_weather.csv")


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
