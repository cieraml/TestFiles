library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)

#Section 1: Bar Plot ----

load("~/Downloads/test_4/sec_stad.Rdata")
cap <- sec_stad %>% group_by(Name) %>% summarise(Capacity)

ggplot(cap) +
  geom_bar(aes(x = Capacity, y= Names),
           position = "dodge",
           stat = "identity") +
  xlab('90-degree angle')

#Section 2: Multiple Points ---- 

load("~/Downloads/test_4/team_statistics.Rdata")
sb <- ts[ts$Conference == "Sun Belt Conference",]
ggplot(data = sb, aes(x = Pass.Yard, y = Rush.Yard)) +
  geom_point()


#Section 3: Box-and-whisker plot ---- 

bt <- ts[ts$Conference == "Big Ten Conference",]
ggplot(bt, aes(x = Big Ten Conference, y = Rush.Yard)) +
  geom_boxplot() +
  theme(panel.background = "dark blue", fill = "bright yellow")

#Section 4: Heat Map ----

load("~/Downloads/test_4/football_stats.Rdata")
se <- football.stats[football.stats$Conference == "Southeastern Conference",]
ggplot(se, aes(x= Southeastern Conference, y= Stat))+
  heatmap()


#Section 5: Mapping ---- 

library(tidyverse)
install.packages('ggmap')
install.packages('osmdata')
library(ggmap)
library(osmdata)

map.toner = get_stamenmap(bbox = Central US, zoom = 8, maptype = 'toner-background')

map.toner2 = get_stamenmap(bbox = Southeastern US, zoom = 8, maptype = 'toner-background')


#Section 6: Analysis ---- 


