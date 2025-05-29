library(tidyverse)
library(gganimate)
library(janitor)
library(sf)

states <- st_read("../../common-resources/cb_2018_us_state_500k.shp")

data <- read_csv("../../week5-tornadoes-usa/data/1950-2021_all_tornadoes.csv") %>% clean_names()

#filter
data <- data %>%
  filter(yr >= 1950 & yr <= 2021) %>%
  filter(!is.na(slat) & !is.na(slon)) %>%
  filter(slat >= 22 & slat <= 50) %>%
  filter(slon >= -126 & slon <= -66) %>%
  filter(5 >= mag & mag >= 1) %>%
  mutate(mag = factor(mag))

p <- ggplot() +
  geom_sf(data = states, fill = NA, color = "black", size = 0.5) +
  geom_point(
    data = data,
    aes(x = slon, y = slat, color = mag),
    alpha = 0.5,
    size = 1.8
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +
  scale_color_manual(
    values = c("1" = "blue", "2" = "green", 
               "3" = "yellow", "4" = "orange", 
               "5" = "red")
  ) +
  transition_states(yr, transition_length = 2, state_length = 2) +
  labs(
    title = "Tornadoes in the US from 1950 to 2021",
    subtitle = "Year: {closest_state}",
    x = "Longitude",
    y = "Latitude",
    color = "Magnitude"
  ) +
  theme_minimal() +
  ease_aes('linear')

anim <- animate(p, renderer = gifski_renderer(), width = 800, 
                height = 600, fps = 5, duration = 30)
anim_save("../output/tornadoes-by-year.gif", animation = anim)