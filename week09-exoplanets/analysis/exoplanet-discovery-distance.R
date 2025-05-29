library(tidyverse)
library(janitor)
library(plotly)

cleaned_5250 <- read_csv("../data/cleaned_5250.csv") %>%
  clean_names() %>% filter(planet_type != "Unknown") %>%
  mutate(hover_text = paste0(
    "Planet: ", name, "<br>",
    "Distance: ", distance, " ly<br>",
    "Discovery Year: ", discovery_year))

p <- ggplot(cleaned_5250, aes(x = discovery_year, y = distance, color = planet_type, text = hover_text)) +
  geom_point() +
  geom_jitter(width = 0.5, height = 0) +
  labs(title = "Distance vs Discovery Year by Planet Type",
       x = "Discovery Year",
       y = "Distance (light years)",
       color = "Planet Type") +
  theme_minimal() +
  facet_wrap(~ planet_type)

ggplotly(p, tooltip = "text")