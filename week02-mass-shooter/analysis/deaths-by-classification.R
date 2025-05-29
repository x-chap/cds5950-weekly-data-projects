library(tidyverse)
firearmdata <- read_csv("../data/firearmdata.csv")
shooterdata <- read_csv("../data/shooterdata.csv")

library(janitor) #remove spaces from column names
firearmdata <- clean_names(firearmdata)
shooterdata <- clean_names(shooterdata)

classification_labels <- c("Handgun", "Shotgun", "Rifle", "Assault weapon")

merged_data <- firearmdata %>%
  left_join(shooterdata, by = "case_number")

merged_data <- merged_data %>%
  mutate(classification = factor(classification, levels = 0:3, labels = classification_labels))

summary_data <- merged_data %>%
  filter(!is.na(classification) & !is.na(number_killed)) %>%  
  group_by(classification) %>%
  summarise(average_number_killed = mean(number_killed, na.rm = TRUE),
            total_number_killed = sum(number_killed, na.rm = TRUE),
            .groups = "drop")

ggplot(summary_data, aes(x = classification, y = total_number_killed, fill = classification)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of People Killed by Firearm Classification",
       x = "Firearm Classification",
       y = "Total Number of People Killed") +
  theme_minimal() +
  scale_x_discrete(labels = classification_labels)