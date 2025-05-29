library(tidyverse)
library(sf)
library(ggplot2)

X1950_2021_all_tornadoes <- read_csv("../data/1950-2021_all_tornadoes.csv")

us_shapefile <- st_read("../../common-resources/cb_2018_us_state_500k.shp")

X2021_tornadoes <- X1950_2021_all_tornadoes %>%
  filter(
    yr == 2021,           # Filter 2021
    mag >= 0 & mag <= 5, # Filter magnitudes
    !is.na(slat) & !is.na(slon) & !is.na(elat) & !is.na(elon),  # Filter coordinates
    slat != 0 & slon != 0 & elat != 0 & elon != 0 
  )

ggplot() +
  geom_sf(data = us_shapefile, fill = "#343a3f", color = "white") +
  geom_segment(data = X2021_tornadoes, 
               aes(x = slon, y = slat, xend = elon, yend = elat, 
                   color = factor(mag)),  
               linewidth = .5, alpha = 0.7) + 
  scale_color_manual(values = c("0" = "#33b1ff", "1" = "#42be65", "2" = "#dfb00a", "3" = "#ea6c04", "4" = "#b50d0c", "5" = "#b13dab")) +
  coord_sf(xlim = c(-123, -69), ylim = c(25, 49)) + 
  labs(title = "Tornado Tracks in the US (2021)", 
       x = "Longitude", y = "Latitude", 
       color = "Magnitude") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#4d5358"),
    plot.background = element_rect(fill = "#697077"), 
    panel.grid = element_blank(),
    axis.title = element_blank(), 
    axis.text = element_blank(),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  )