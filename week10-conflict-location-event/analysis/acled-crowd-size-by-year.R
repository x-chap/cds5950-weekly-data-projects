# ACLED DATA USAGE NOTICE
# Data Source: Armed Conflict Location & Event Data Project (ACLED); www.acleddata.com
# Date Accessed: Retrieved from professor April 10 2025
# Filters Applied: Data filtered to include USA and Canada, from 2020 to 2023. 2023 data is up to March 24.
# Data Manipulations: Column names cleaned using janitor::clean_names()
# Please refer to ACLED's usage policy for more information: https://acleddata.com
# Raleigh, C., Kishi, R. & Linke, A. (2023). Political instability patterns are obscured by conflict dataset scope conditions, sources, and coding choices. Humanit Soc Sci Commun, 10, 74. https://doi.org/10.1057/s41599-023-01559-4

library(tidyverse)
library(readxl)
library(janitor)
library(sf)
library(usmap)

USA_Canada_2020_2023_Mar24 <- read_excel(
  "../data/USA_Canada_2020_2023_Mar24.xlsx", 
    col_types = c("text", "date", "numeric", 
    "numeric", "text", "text", "text", 
    "text", "text", "numeric", "text", 
    "text", "numeric", "numeric", "text", 
    "numeric", "text", "text", "text", 
    "text", "text", "text", "numeric", 
    "numeric", "numeric", "text", "text", 
    "text", "numeric", "text", "numeric")) %>% 
  clean_names()


USA_Canada_2020_2023_Mar24 <- USA_Canada_2020_2023_Mar24 %>%
  mutate(
    aprox_crowd_size = str_extract(tags, "\\d+") %>%
      as.numeric()
  )

summary(USA_Canada_2020_2023_Mar24$aprox_crowd_size)
length(USA_Canada_2020_2023_Mar24$aprox_crowd_size)
# 71.51% is missing crowd size

crowd_events <- USA_Canada_2020_2023_Mar24 %>%
  filter(!is.na(aprox_crowd_size),
         !is.na(year))

crowd_events %>%
  group_by(year) %>%
  summarise(total_crowd_size = sum(aprox_crowd_size, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_crowd_size)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total Crowd Size by Year (2020–2023)",
    x = "Year",
    y = "Total Crowd Size",
    caption = "Data Source: Armed Conflict Location & Event Data Project (ACLED); www.acleddata.com\nDate Accessed: Retrieved from professor April 10, 2025\nFilters: USA and Canada, 2020–2023 | Data cleaned using janitor::clean_names()"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 8, face = "italic")
  )
