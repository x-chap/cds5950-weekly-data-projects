library(tidyverse)
library(sf)

# load crime data and filter
ucr_1960_2020 <- read_csv("../data/ucr_1960_2020.csv") %>%
  filter(number_of_months_missing == 0) %>%
  mutate(
    decade = floor(year / 10) * 10,
    fips_code = paste0(fips_state_code, fips_county_code)
  ) %>%
  group_by(decade, fips_code) %>%
  summarize(
    violent_crime_rate = sum(actual_index_violent, na.rm = TRUE) / sum(population, na.rm = TRUE) * 100000,
    property_crime_rate = sum(actual_index_property, na.rm = TRUE) / sum(population, na.rm = TRUE) * 100000,
    .groups = "drop"
  )

#load shape file
county_shapes <- st_read("../../common-resources/cb_2018_us_county_500k.shp") %>%
  mutate(fips_code = as.character(GEOID))  

# merge crime with shape
crime_map_data <- county_shapes %>%
  left_join(ucr_1960_2020, by = "fips_code") %>%
  mutate(
    violent_crime_bin = cut(violent_crime_rate, breaks = seq(0, 1000, by = 100), include.lowest = TRUE, labels = FALSE),
    property_crime_bin = cut(property_crime_rate, breaks = seq(0, 5000, by = 500), include.lowest = TRUE, labels = FALSE)
  )

# theme (ur welcome)
map_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#4d5358"),
    plot.background = element_rect(fill = "#697077"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.position = "bottom"
  )

# function for maps (ur welcome again)
plot_crime_map <- function(data, aes_fill, colors, labels, title) {
  ggplot(data) +
    geom_sf(aes(fill = factor({{ aes_fill }})), color = "white", size = 0.1) +
    scale_fill_manual(values = colors, name = title, labels = labels) +
    labs(title = title) +
    coord_sf(xlim = c(-125, -66.5), ylim = c(24, 49)) +
    map_theme
}

# color palettes and labels
violent_colors <- c("#fff1f1", "#ffd7d9", "#ffb3b8", "#ff8389", "#fa4d56", 
                    "#da1e28", "#a2191f", "#750e13", "#520408", "#2d0709")
violent_labels <- c("0-100", "100-200", "200-300", "300-400", "400-500", 
                    "500-600", "600-700", "700-800", "800-900", "900+")

property_colors <- c("#edf5ff", "#d0e2ff", "#a6c8ff", "#78a9ff", "#4589ff", 
                     "#0f62fe", "#0043ce", "#002d9c", "#001d6c", "#001141")
property_labels <- c("0-500", "500-1000", "1000-1500", "1500-2000", "2000-2500", 
                     "2500-3000", "3000-3500", "3500-4000", "4000-4500", "4500+")

# plots
plot_crime_map(crime_map_data, violent_crime_bin, violent_colors, violent_labels, "Violent Crime Rate by County (1960-2020)")
plot_crime_map(crime_map_data, property_crime_bin, property_colors, property_labels, "Property Crime Rate by County (1960-2020)")
