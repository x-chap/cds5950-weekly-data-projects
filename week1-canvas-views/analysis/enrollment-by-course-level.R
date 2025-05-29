library(tidyverse)

data <- read.csv("../data/canvas data fall 2024")

data <- data %>%
  mutate(
    course_number = sapply(strsplit(name, " - "), function(x) {
      #"2024 Fall", "HONS 1000-1", "Introduction to Honors"
      parts_after_year <- x[-1] 
      #"HONS 1000-1", "Introduction to Honors"
      subject_section <- parts_after_year[1]
      #"HONS 1000-1"
      subject_number <- unlist(strsplit(subject_section, " "))
      #"HONS", "1000-1"
      number_section <- subject_number[2]
      #"1000-1"
      course_num <- unlist(strsplit(number_section, "-"))[1]
      #"1000"
      substr(course_num, 1, 4)
      #"1"
    }),
    is_1000 = substr(course_number, 1, 1) == "1"
    #TRUE or FALSE if the course number starts with 1
  )

data <- data[!is.na(data$is_1000), ]
#removes 1 NA row

ggplot(data, aes(x = is_1000, y = enrollment)) +
  geom_boxplot() +
  labs(
    title = "Enrollment Distribution by Course Level",
    x = "Is 1000-Level Course",
    y = "Enrollment"
  ) +
  theme_minimal()

