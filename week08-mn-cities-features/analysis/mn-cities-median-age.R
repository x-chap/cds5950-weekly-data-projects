library(tidyverse)
library(janitor)
library(usmap)
library(scales)
library(sf)
library(plotly)

usmap <- us_map()

MNcities <- read_csv("../data/MNcities.csv") %>% clean_names()

#rm.na "Missing["NotAvailable"]" values
MNcities <- MNcities %>%
  filter(city_sales_tax_rate != "Missing[\"NotAvailable\"]") %>%
  mutate(city_sales_tax_rate = as.numeric(city_sales_tax_rate)) %>%
  mutate(median_age = as.numeric(median_age))

mn_map <- usmap %>% 
  filter(abbr == "MN") %>%
  st_transform(crs = 4326)  # NAD27 -> WGS84

p <- ggplot() +
  geom_sf(data = mn_map, fill = "gray90", color = "black") + 
  geom_point(data = MNcities, aes(
    x = longitude, 
    y = latitude, 
    color = median_age, 
    text = paste("City:", name, "<br>Median Age:", median_age)
  ), size = 3) +
  scale_color_gradient(low = "white", high = "#007d79", name = "Median Age") +
  theme_void() +
  theme(legend.position = "right") +
  labs(title = "Median Age in Minnesota Cities")

ggplotly(p, tooltip = "text")