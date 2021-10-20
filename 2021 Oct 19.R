library(tidyverse)
library(gganimate)
library(stringr)
library(ggrepel)

tuesdata <- tidytuesdayR::tt_load('2021-10-19')$pumpkins

tuesdata$year <- as.integer(str_extract(tuesdata$id, "[:digit:]{4}"))
tuesdata$type <- as.factor(str_extract(tuesdata$id, "[:alpha:]{1}"))
tuesdata$weight_lbs <- as.double(str_remove(tuesdata$weight_lbs, ","))
tuesdata$est_weight <- as.numeric(str_remove(tuesdata$est_weight, ","))
tuesdata$place <- as.integer(tuesdata$place)

firstPlaceGiants <- tuesdata %>% 
  filter(type == "P", country %in% c("Italy", "Spain", "Belgium")) %>% 
  select(year, country, place, weight_lbs)

firstPlaceGiantsWeight <- firstPlaceGiants %>% 
  group_by(year, country) %>% 
  summarize(weight = mean(weight_lbs))
firstPlaceGiantsPlace <- firstPlaceGiants %>% 
  group_by(year, country) %>% 
  filter(!is.na(place)) %>% 
  summarize(place = min(place))
firstPlaceGiantsAgg <- left_join(firstPlaceGiantsPlace, firstPlaceGiantsWeight)
firstPlaceGiantsAgg$place <- as.character(firstPlaceGiantsAgg$place)

giantAnim <- firstPlaceGiantsAgg %>% 
  ggplot(aes(x = year, y = weight, color = country, label = place)) +
  geom_line(size = 1.2) + geom_text_repel() + transition_reveal(year) +
  scale_x_continuous(breaks = 2013:2021, labels = 2013:2021) + scale_color_manual(values = c("#FFCF00", "#FF8C00", "#FF4800")) +
  theme_minimal() + theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Average Giant Pumpkin Weights in 2021's Top-Placing Countries", x = "Year", y = "Weight")
animate(giantAnim, end_pause = 20)
