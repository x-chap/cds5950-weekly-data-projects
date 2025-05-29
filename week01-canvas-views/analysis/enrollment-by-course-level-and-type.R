library(tidyverse)

data <- read.csv("../data/canvas data fall 2024")

data <- data %>%
  mutate(
    #2024 Fall - EXSC 3510-1 - Exercise Physiology
    course_number = map_chr(strsplit(name, " - "), ~ .x[2]),
    #3510
    course_number = str_extract(course_number, "\\d+"),       
    course_level = case_when(
      str_starts(course_number, "1") ~ "1000-Level",
      # 3 =/= 1
      str_starts(course_number, "[2-4]") ~ "Upper-Level (2-4)",
      # 3 = [2-4]
      str_starts(course_number, "[5-8]") ~ "Graduate-Level (5-8)"
    ),
    underVgrad = case_when(
      str_starts(course_number, "[1-4]") ~ "Undergraduate",
      # 3 = [1-4]
      str_starts(course_number, "[5-8]") ~ "Graduate"
    ),
    # redundant as a factor so ggplot2 works nicely
    course_level = factor(course_level, levels = c("1000-Level", "Upper-Level (2-4)", "Graduate-Level (5-8)")),
    underVgrad = factor(underVgrad, levels = c("Undergraduate", "Graduate"))
  ) %>%
  filter(!is.na(course_level)) 
  #removes "how canvas works" course for new students

summary_stats <- data %>%
  group_by(course_level) %>%
  summarize(Median = median(enrollment), Mean = mean(enrollment))

gradSummary_stats <- data %>%
  group_by(underVgrad) %>%
  summarize(Median = median(enrollment), Mean = mean(enrollment))

course_counts <- data %>%
  count(course_level)

underVgrad_counts <- data %>%
  count(underVgrad)

#course_level plot
ggplot(data, aes(x = course_level, y = enrollment, fill = course_level)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.3) +  # Show individual points
  scale_x_discrete(labels = c("1000-Level", "Upper-Level", "Graduate-Level")) +  # Clearer labels
  labs(
    x = "Course Level", 
    y = "Enrollment"
  ) +
  geom_text(
    data = course_counts,
    aes(x = course_level, y = max(data$enrollment) * 1.1, label = paste("n =", n)),
    vjust = -0.5, size = 4, color = "black"
  )
  theme_minimal()

#underVgrad plot
ggplot(data, aes(x = underVgrad, y = enrollment, fill = underVgrad)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.3) +  # Show individual points
  scale_x_discrete(labels = c("Undergraduate", "Graduate")) +  # Clearer labels
  labs(
    x = "Course Level", 
    y = "Enrollment"
  ) +
  geom_text(
    data = underVgrad_counts,
    aes(x = underVgrad, y = max(data$enrollment) * 1.1, label = paste("n =", n)),
    vjust = -0.5, size = 4, color = "black"
  )
  theme_minimal()

  

course_counts <- data %>%
  count(course_level)


total_enrollment <- data %>%
  group_by(course_level) %>%
  summarize(Total_Enrollment = sum(enrollment))


print(total_enrollment)
print(summary_stats)
print(gradSummary_stats)
