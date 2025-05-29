library(tidyverse)
library(janitor)
library(plotly)
library(scales)  # Load scales package for formatting

agile_sub <- read_csv("../data/agile-sub.csv") %>%
  clean_names()

# Convert from wide to long format
agile_sub_long <- agile_sub %>%
  pivot_longer(cols = -yr_cohort_reaches_hs_graduation, 
               names_to = "City", 
               values_to = "Graduates")

# Rename columns
colnames(agile_sub_long) <- c("Year", "City", "Graduates")

# Standardize the Graduates column within each city group
agile_sub_long <- agile_sub_long %>%
  group_by(City) %>%
  mutate(Normalized_Graduates = scale(Graduates)[,1]) %>%
  ungroup()

# Convert Year to numeric for plotting
agile_sub_long$Year <- as.numeric(agile_sub_long$Year)

p <- ggplot(agile_sub_long, aes(x = Year, y = Normalized_Graduates, color = City, group = City)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, alpha = .5) +  # Add trendline with alpha = 1
  labs(title = "Standardized Predicted Trends in High School Graduates by City", 
       x = "Year", 
       y = "Standardized Number of Graduates") +
  theme_minimal() +  # Theme with larger text
  theme(legend.position = "bottom")  # Remove the legend

# Convert to interactive Plotly chart
ggplotly(p)
