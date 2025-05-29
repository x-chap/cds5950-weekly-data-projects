library(dplyr)
library(ggplot2)
library(plotly)

week11 <- read_csv("../data/grade dataset for CDS 5950.csv")

# Define the order of academic periods for proper time-based plotting
period_order <- c(
  "Fall 2015", "Winter 2016", "Spring 2016", "Summer 2016",
  "Fall 2016", "Winter 2017", "Spring 2017", "Summer 2017",
  "Fall 2017", "Winter 2018", "Spring 2018", "Summer 2018",
  "Fall 2018", "Winter 2019", "Spring 2019", "Summer 2019",
  "Fall 2019", "Winter 2020", "Spring 2020", "Summer 2020",
  "Fall 2020", "Winter 2021", "Spring 2021", "Summer 2021",
  "Fall 2021", "Winter 2022", "Spring 2022", "May Term 2022", "Summer 2022",
  "Fall 2022", "Winter 2023"
)
# Define a mapping of letter grades to GPA values
grade_scale <- c(
  "A" = 4.0, "A-" = 3.7, "B+" = 3.3, "B" = 3.0, "B-" = 2.7,
  "C+" = 2.3, "C" = 2.0, "C-" = 1.7, "D+" = 1.3, "D" = 1.0,
  "F" = 0.0, "P" = NA, "W" = NA, "I" = NA
)



# Clean the data: calculate GPA and remove rows with NA GPA or unknown race
cleaned_data <- week11 %>%
  mutate(
    GPA = grade_scale[`Transcript Grade`]
  ) %>%
  filter(
    !is.na(GPA),
    !tolower(`Race/Ethnicities`) %in% c("unknown")
  )
# Set the academic period as an ordered factor based on the defined sequence
cleaned_data <- cleaned_data %>%
  mutate(`Academic Period` = factor(`Academic Period`, levels = period_order))
# Calculate university-wide average GPA for each academic period
univ_avg <- cleaned_data %>%
  group_by(`Academic Period`) %>%
  summarise(
    TotalGPA = sum(GPA, na.rm = TRUE),
    N = n(),
    AvgGPA = TotalGPA / N,
    .groups = "drop"
  )

# Calculate GPA averages for only Fall and Spring semesters
fall_spring_avg <- cleaned_data %>%
  filter(grepl("^Fall|^Spring", `Academic Period`)) %>%
  group_by(`Academic Period`) %>%
  summarise(
    TotalGPA = sum(GPA, na.rm = TRUE),
    N = n(),
    AvgGPA = TotalGPA / N,
    .groups = "drop"
  ) %>%
  mutate(`Academic Period` = factor(`Academic Period`, levels = period_order))

# Ensure avg_by_period_race has the correct factor levels for plotting
avg_by_period_race <- avg_by_period_race %>%
  mutate(`Academic Period` = factor(`Academic Period`, levels = period_order))
# Reapply the factor levels to the university average dataset (if not already)
univ_avg <- univ_avg %>%
  mutate(`Academic Period` = factor(`Academic Period`, levels = period_order))

p <- ggplot() +
  # Race lines
  geom_line(
    data = avg_by_period_race,
    aes(x = `Academic Period`, y = AvgGPA, color = `Race/Ethnicities`, group = `Race/Ethnicities`),
    size = .9,
    alpha = .8
  ) +
  geom_point(
    data = avg_by_period_race,
    aes(x = `Academic Period`, y = AvgGPA, color = `Race/Ethnicities`),
    alpha = 0.8,
    size = .45
  ) +
  # University-wide weighted average line
  geom_line(
    data = univ_avg,
    aes(x = `Academic Period`, y = AvgGPA, group = 1),
    color = "black",
    size = 1,
    alpha = 0.0
  ) +
  # Fall/Spring only average line
  geom_line(
    data = fall_spring_avg,
    aes(x = `Academic Period`, y = AvgGPA, group = 1),
    color = "red",
    size = 1,
    alpha = 0.8
  ) +
  labs(
    title = "Average Grade by Race with University Weighted GPA Trend",
    x = "Academic Period",
    y = "Average Grade",
    color = "Race/Ethnicities"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # Customize y-axis to show grade labels
  scale_y_continuous(
    limits = c(0, 4),
    breaks = c(0, 1, 2, 2.3, 2.7, 3.0, 3.3, 3.7, 4),
    labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A")
  )

ggplotly(p)