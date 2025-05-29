library(tidyverse)

data <- read.csv("../data/canvas data fall 2024.csv")

data <- data %>%
  mutate(
    course_number = map_chr(strsplit(name, " - "), ~ .x[2]),  
    course_number = str_extract(course_number, "\\d+"),       
    is_1000 = str_starts(course_number, "1")
  ) %>%
  filter(!is.na(is_1000)) 

summary_stats <- data %>%
  group_by(is_1000) %>%
  summarize(Median = median(enrollment), Mean = mean(enrollment))

course_counts <- data %>%
  count(is_1000)

# Create the plot
ggplot(data, aes(x = is_1000, y = enrollment, fill = is_1000)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.3) +  # Show individual points
  scale_x_discrete(labels = c("Upper-Level", "1000-Level")) +  # Clearer labels
  labs(
    x = "Course Level", 
    y = "Enrollment"
  ) +
  geom_text(
    data = course_counts,
    aes(x = is_1000, y = max(data$enrollment) * 1.1, label = paste("n =", n)),
    vjust = -0.5, size = 4, color = "black"
  )
  theme_minimal()



# Add a statistical test (e.g., Wilcoxon rank test)
wilcox.test(enrollment ~ is_1000, data = data) %>%
  broom::tidy() %>%
  knitr::kable(caption = "P-value < 0.05 confirms significant difference.")

# Count the number of courses by is_1000
course_counts <- data %>%
  count(is_1000)


total_enrollment <- data %>%
  group_by(is_1000) %>%
  summarize(Total_Enrollment = sum(enrollment))

# Print the result
print(total_enrollment)