library(tidyverse)
library(treemap)
library(dplyr)

df <- read_csv("../data/grade dataset for CDS 5950.csv")


# Remove duplicate students
unique_students <- df |>
  distinct(`Student ID`, .keep_all = TRUE)

# Filter out undeclared majors/pseo
filtered_students <- unique_students |>
  filter(!`Primary Program of Study` %in% c("Undeclared CLA", "Undeclared HSB", "Post Secondary Enrollment Options"))

# Count majors
major_counts <- filtered_students |>
  count(`Primary Program of Study`, name = "count")

treemap(
  major_counts,
  index = "Primary Program of Study",
  vSize = "count",
  title = "Most Popular Majors",
  palette = "Set3",
  fontsize.labels = 8,        # Smaller font size
  fontcolor.labels = "black",
  border.col = "white"
)