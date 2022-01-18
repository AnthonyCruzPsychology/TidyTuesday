#### Library and Data Imports ####

library(tidyverse)
library(stringr)
library(gganimate)
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

#### Cleaning Data ####
chocolate_small <- chocolate %>%
  mutate(
    num_ingredients = as.integer(str_extract(ingredients, "[:digit:]")),
    cocoa_percent_num = as.integer(str_extract(cocoa_percent, "[:digit:]{1,3}"))) %>% 
  select(review_date, cocoa_percent_num, rating)

cocoa_tertiles <- quantile(chocolate_small$cocoa_percent_num, c(0,.33,.67))
cocoa_quintiles <- quantile(chocolate_small$cocoa_percent_num, c(0,.2,.4,.6,.8))

chocolate_small <- chocolate_small %>% 
  mutate(cocoa_group = case_when(
    cocoa_percent_num >= cocoa_tertiles[3] ~ "High",
    cocoa_percent_num <= cocoa_tertiles[2] ~ "Low",
    TRUE ~ "Moderate"
  )) %>% 
  select(review_date, cocoa_group, rating) %>% 
  group_by(review_date, cocoa_group) %>% 
  summarize(mean_rating = mean(rating))

#### Plot ####
chocolate_small %>% ggplot(., aes(x = review_date, y = mean_rating))+
  geom_hline(aes(yintercept = mean(chocolate$rating)), color = "red", linetype = "dashed") +
  geom_line(color="#5C4033", size=1.5) + #geom_point(color = "black", pch = 12, size = 3) +
  # geom_text(x = 2017, y = 3.23, label = "Mean") +
  transition_filter(`Low Cocoa` = cocoa_group == "Low", `Moderate Cocoa` = cocoa_group == "Moderate", `High Cocoa` = cocoa_group == "High" ) +
  ease_aes('cubic-in-out') +
  labs(title = "Chocolate Bar Ratings by Year", subtitle="Bars with {closest_filter}", y = "Mean Rating", x = "Year") +
  theme_minimal() + theme(
    panel.grid = element_blank(),
    axis.line.x.bottom = element_line(),
    axis.line.y.left = element_line(),
    title = element_text(family = "Arial", size = 20),
    axis.text = element_text(family = "Arial", size = 15)
  )
