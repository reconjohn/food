library(tidyverse)
library(lubridate)
library(stringr)
library(ggmap)
library(ggrepel)

restaurants <- read_csv("https://clanfear.github.io/CSSS508/Lectures/Week8/restaurants.csv",  col_types = "ccccccccnnccicccciccciD")

str(restaurants)
View(restaurants)

restaurants_only <- restaurants %>% 
  filter(str_detect(Description, "Seating"))

scores_over_time <- restaurants_only %>% 
  group_by(Business_ID, Date) %>% 
  distinct(Name, Address, Zip_Code, Longitude, Latitude, Inspection_Score)

scores_over_time <- scores_over_time %>% 
  mutate(Label_40 = ifelse(Inspection_Score >= 40, Name, "")) %>% 
  filter(!is.na(Longitude))

View(scores_over_time)

recent_scores <- scores_over_time %>% 
  group_by(Business_ID) %>% 
  filter(Date == max(Date)) %>% 
  filter(Inspection_Score == max(Inspection_Score)) 

qmplot(data = recent_scores, 
       x = Longitude, 
       y = Latitude, 
       zoom = 10,
       maptype = "toner-lite",
       color = Inspection_Score, 
       size = Inspection_Score,
       alpha = I(0.2))+
  scale_color_gradient2(
    "Inspection_Score", 
    low = "white", 
    mid = "yellow", 
    high = "red")

qmplot(data = recent_scores, geom = "blank",
       x = Longitude, y = Latitude, 
       maptype = "toner-lite", 
       darken = 0.2) + 
  stat_density_2d(
    aes(fill = stat(level)),
    geom = "polygon", 
    alpha = .2, color = NA) + 
  scale_fill_gradient2(
    "Inspection_Score", 
    low = "white", 
    mid = "yellow", 
    high = "red") + 
  theme(legend.position = "bottom")

udist <- recent_scores %>%
  filter(Latitude > 47.655, Latitude < 47.67,
         Longitude > -122.32, Longitude < -122.30)

qmplot(data = udist,
       x = Longitude,
       y = Latitude,
       maptype = "toner-lite", 
       color = I("firebrick"), 
       alpha = I(0.5)) + 
  geom_label_repel(
    data = udist,
    aes(label = Label_40), 
    fill = "black", 
    color = "white", 
    segment.color = "black")

caphill <- recent_scores %>%
  filter(Latitude > 47.605, Latitude < 47.625,
         Longitude > -122.33, Longitude < -122.31)

qmplot(data = caphill,
       x = Longitude,
       y = Latitude,
       maptype = "toner-lite", 
       color = I("firebrick"), 
       alpha = I(0.5)) + 
  geom_label_repel(
    data = caphill,
    aes(label = Label_40), 
    fill = "black", 
    color = "white", 
    segment.color = "black")

tofu <- scores_over_time %>% 
  filter(str_detect(Name, "TOFU|Tofu|tofu")) %>% 
  filter(Latitude > 47.57, Latitude < 47.70,
         Longitude > -122.39, Longitude < -122.14)

tofu_point <- tofu %>% 
  group_by(Business_ID) %>% 
  filter(Date == max(Date)) %>% 
  filter(Inspection_Score == max(Inspection_Score)) 

View(tofu_point)

qmplot(data = tofu_point,
       x = Longitude,
       y = Latitude,
       maptype = "toner-lite", 
       color = I("firebrick"), 
       size = I(3),
       alpha = I(0.8)) + 
  geom_label_repel(
    data = tofu_point,
    aes(label = Name), 
    fill = "black", 
    color = "white", 
    segment.color = "black")

tofu <- tofu %>% 
  filter(Longitude < -122.24, Date > as.Date("2015-01-01")) 

tofu_arr <- tofu %>% 
  group_by(Name) %>% 
  summarise(high = max(Inspection_Score)) %>% 
  arrange(desc(high))

tofu %>% 
  mutate(Name = parse_factor(Name, levels = tofu_arr$Name)) %>% 
  ggplot(aes(x = Date, y = Inspection_Score, group = Name, color = Name)) +
  facet_wrap( ~ Name) +
  geom_point() +
  geom_line(size = 2) +
  xlab("Year") + ylab("Inspection score") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
