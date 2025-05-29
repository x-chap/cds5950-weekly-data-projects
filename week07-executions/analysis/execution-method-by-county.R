library(tidyverse)
library(janitor)
library(usmap)
library(sf)


execution_database_19 <- read_csv("../data/execution_database_19.csv") %>%
  clean_names() %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    year = year(date)
  )

# group method by county
most_common_method_by_county <- execution_database_19 %>%
  group_by(county, state) %>%
  count(method) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  select(county, state, method)

# fips from usmap
county_fips_data <- usmap::countypop %>%
  select(fips, state = abbr, county) %>% 
  mutate(county = str_remove(county, " County") %>% str_to_title()) 


execution_database_with_method <- most_common_method_by_county %>%
  left_join(county_fips_data, by = c("state", "county")) %>%
  drop_na(fips) 

# state lines for map
states_sf <- usmap::us_map("states") %>%
  st_as_sf() %>%
  rename(geometry = geom)



plot_usmap(regions = "counties", data = execution_database_with_method, values = "method", 
                include = execution_database_with_method$fips,  # only plot counties WITH data
                color = "black", size = 0.25) + 
  scale_fill_viridis_d(name = "Execution Method") + 
  theme_void() +
  labs(title = "Most Common Execution Method by County") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom") +
  geom_sf(data = states_sf, aes(geometry = geometry), color = "black", size = 1.5, fill = NA)