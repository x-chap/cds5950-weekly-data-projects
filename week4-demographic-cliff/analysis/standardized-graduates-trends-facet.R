library(tidyverse)
library(janitor)
library(plotly)
library(scales)

agile_sub <- read_csv("../data/agile-sub.csv") %>%
  clean_names()

agile_sub_long <- agile_sub %>%
  pivot_longer(cols = -yr_cohort_reaches_hs_graduation, 
               names_to = "City", 
               values_to = "Graduates")

colnames(agile_sub_long) <- c("Year", "City", "Graduates")

agile_sub_long <- agile_sub_long %>%
  group_by(City) %>%
  mutate(Normalized_Graduates = scale(Graduates)[,1]) %>%
  ungroup()

agile_sub_long$Year <- as.numeric(agile_sub_long$Year)

city_totals <- agile_sub_long %>%
  group_by(City) %>%
  summarize(Total_Graduates = sum(Graduates, na.rm = TRUE)) %>%
  arrange(desc(Total_Graduates))

agile_sub_long$City <- factor(agile_sub_long$City, levels = city_totals$City)

p <- ggplot(agile_sub_long, aes(x = Year, y = Normalized_Graduates, color = City, group = City)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = "gam", se = FALSE, alpha = .5) + 
  labs(title = "Standardized Predicted Trends in High School Graduates by City", 
       x = "", 
       y = "") +
  theme_minimal() + 
  theme(
    legend.position = "none",  
    strip.text = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text()
  ) + 
  facet_wrap(~ City, scales = "fixed", ncol = 5)

ggplotly(p)