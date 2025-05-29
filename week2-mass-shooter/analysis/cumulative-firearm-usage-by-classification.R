library(tidyverse)
firearmdata <- read_csv("../data/firearmdata.csv")
shooterdata <- read_csv("../data/shooterdata.csv")

library(janitor) #remove spaces from column names
firearmdata <- clean_names(firearmdata)
shooterdata <- clean_names(shooterdata)

library(lubridate) #fix date format
firearmdata <- firearmdata %>%
  mutate(full_date = ymd(full_date))

classification_labels <- c("Handgun", "Shotgun", "Rifle", "Assault weapon")

firearmdata_summary <- firearmdata %>%
  filter(used_in_shooting == 1) %>%  # only count firearms used in shootings
  mutate(year = year(full_date)) %>%  # get year
  group_by(year, classification) %>% # group by year and classification
  summarise(usage_count = n(), .groups = "drop") %>% # count number of times each classification was used
  arrange(year, classification) %>%  # sort by year and classification
  mutate(cumulative_usage = cumsum(usage_count)) %>%  # calculate cumulative sum of usage
  mutate(classification = factor(classification, levels = 0:3, labels = classification_labels)) %>% 
  drop_na(classification)  # drop rows where there is no classification

max_cumulative_usage <- max(firearmdata_summary$cumulative_usage/4, na.rm = TRUE)

ggplot(firearmdata_summary, aes(x = year, y = cumulative_usage, color = classification)) +
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = seq(min(firearmdata_summary$year, na.rm = TRUE), 
                                  max(firearmdata_summary$year, na.rm = TRUE), 1)) +
  scale_y_continuous(limits = c(0, ceiling(max_cumulative_usage * 1.1)),  
                     breaks = pretty(c(0, max_cumulative_usage), n = 5)) +  
  labs(title = "Cumulative Firearm Usage Over Time by Classification",
       x = "Year",
       y = "",
       color = "Firearm Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
