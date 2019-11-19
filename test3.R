library(tidyverse)
library(plyr)
library(dplyr)
load("/Users/cieralogan/Downloads/test3_data.Rdata")

# Section 1 - Subsetting & Ordering Data

fields <- names(d[,c("cruise", "transect.id", "haul","area", "tow", "region",  
                                  "dateTime", "depth", "temp", "salinity", "pressure", "sw.density",
                                  "fluoro", "irradiance", "lat", "lon")])
d <- d[,c(fields)]
d <- arrange(d, transect.id, dateTime)

# Section 2 - Generating figures using batch processing

dir.create("plots")
library(ggplot2)

u <- d[d$tow == "und",]
s <- d[d$tow == "s",]
m <- d[d$tow == "m",]

ddply(.data = u, .variables = c("transect.id"), function(x){
  
  u.plot <- ggplot(data = x, aes(x = dateTime, y = lon)) +
    geom_point(aes(fill = NA, color = "darkblue")) +
    scale_x_continuous(name = "Time", labels = "%H:%M",
                     limits = c(0,15)) +
    ggtitle(label = unique(x$transect.id)) +
    geom_smooth()
  
  ggsave(filename = paste0('plots/', unique(x$transect.id),'.png'),
         plot = u.plot, width = 4, height = 3, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")

ddply(.data = s, .variables = c("transect.id"), function(x){
  
  s.plot <- ggplot(data = x, aes(x = dateTime, y = lon)) +
    geom_point(aes(fill = NA, color = "darkblue")) +
    scale_x_continuous(name = "Time", labels = "%H:%M",
                       limits = c(0,15)) +
    ggtitle(label = unique(x$transect.id)) +
    geom_smooth()
  
  ggsave(filename = paste0('plots/', unique(x$transect.id),'.png'),
         plot = s.plot, width = 4, height = 3, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")

ddply(.data = m, .variables = c("transect.id"), function(x){
  
  m.plot <- ggplot(data = x, aes(x = dateTime, y = lon)) +
    geom_point(aes(fill = NA, color = "darkblue")) +
    scale_x_continuous(name = "Time", labels = "%H:%M",
                       limits = c(0,15)) +
    ggtitle(label = unique(x$transect.id)) +
    geom_smooth()
  
  ggsave(filename = paste0('plots/', unique(x$transect.id),'.png'),
         plot = m.plot, width = 4, height = 3, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")

# Section 3 - Batch Processing

study.f <- function(x){
  
  t <- str_split_fixed(string = x[['transect.id']], pattern = "-", n = 2)
  s <- t[2]
  
  if(str_detect(string = s, pattern = fixed("Eddy"))){
    study <- "eddy"
    
  } else if(str_detect(string = s, pattern = "W")) {
    study <- "spatial"
    
  } else if(str_detect(string = s, pattern = "E")) {
    study <- "spatial"
    
  } else if(str_detect(string = s, pattern = "C")) {
    study <- "spatial"
    
  } else {}
  
  return(study)
  
}

for(i in 1:nrow(d)){
  d[i,]$study <- study.f(x = d[i,])
}
d$study <- apply(X = d, MARGIN =  1, FUN = study.f)

# Section 4 - Plotting a histogram

d$spatial.pressure <- factor(x = d$study, levels = c("west", "central", "east"),
                     labels = c("West", "Central", "East"))
p <- ggplot(data = d, aes(x = depth)) + geom_histogram() + facet_wrap(.~spatial.pressure)

# Section 5 - Summarising

r <- group_by(.data = d, region)
w <- summarise(.data = r, avg.temp = mean(temp, na.rm = T))

w$tempF <- NA 
w$tempK <- NA

for(i in 1:nrow(w)){
  
  w[i,]$tempF <- w[i,]$avg.tempC * (9/5) + 32
  w[i,]$tempK <- w[i,]$avg.tempC + 273.15
  
}

wm <- melt(data = w[w$region != "sof",], id.vars = c("region", "tow"),measure.vars = c("tempF","tempK"))

# Section 6 - Bar plot 

library(reshape2)
p1 <- ggplot(data = wm, aes(x = variable, y = value)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~region)




