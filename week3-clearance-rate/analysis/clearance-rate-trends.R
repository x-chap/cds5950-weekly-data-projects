library(tidyverse)
library(plotly)
library(janitor)

clearances <- read_csv("../data/clearances.csv") %>%
  clean_names()

# Ensure year is numeric
clearances$year <- as.numeric(clearances$year)

# Compute national average clearance rates per year
national_trends <- clearances %>%
  group_by(year) %>%
  summarize(
    total_crime = mean(total_crime, na.rm = TRUE),
    index_violent = mean(index_violent, na.rm = TRUE),
    index_property = mean(index_property, na.rm = TRUE),
    index_total = mean(index_total, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(total_crime, index_violent, index_property, index_total),
               names_to = "crime_type", values_to = "clearance_rate")

# Reshape individual agency data for plotting (keeping same crime_type)
agency_trends <- clearances %>%
  pivot_longer(cols = c(total_crime, index_violent, index_property, index_total), 
               names_to = "crime_type", 
               values_to = "clearance_rate")

# Create the plot
p <- ggplot() +
  geom_line(data = agency_trends, 
            aes(x = year, y = clearance_rate, group = interaction(agency_name, crime_type), color = crime_type),
            alpha = 0.5, size = 0.5) +  
  # Bold Minneapolis trend lines
  geom_line(data = agency_trends %>% filter(agency_name == "minneapolis"), 
            aes(x = year, y = clearance_rate, group = interaction (agency_name, crime_type), color = crime_type), 
            alpha = 1, size = 1) +
  # Bolder national average trend lines with points
  geom_line(data = national_trends, 
            aes(x = year, y = clearance_rate, color = crime_type),
            size = 1.5) +
  geom_point(data = national_trends, 
             aes(x = year, y = clearance_rate, color = crime_type),
             size = 2.5) +
  labs(
    title = "Crime Clearance Rate Trends (National vs. Individual Agencies)",
    x = "Year",
    y = "Clearance Rate",
    color = "Crime Type"
  ) +
  theme_minimal()

# Convert to interactive plotly plot
ggplotly(p)



clearancescount <- sum(
  sum(clearances$total_crime, na.rm = TRUE),
  sum(clearances$index_violent, na.rm = TRUE),
  sum(clearances$index_property, na.rm = TRUE),
  sum(clearances$index_total, na.rm = TRUE),
  sum(clearances$murder, na.rm = TRUE))

print(sum(clearancescount, na.rm = TRUE)) # 66862.2