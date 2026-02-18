
#google 
# https://dkahle.r-universe.dev/ggmap

rm(list = ls())

library("ggplot2")
library("cowplot")
library("ggrepel")
library("reshape2")


library("ggmap")



locs <- read.csv("latlongs.csv")
head(locs)

ggplot(locs, aes( long, lat))+geom_point()

c(left=145.4542-0.02, right=145.4542+0.03, bottom=-14.67657-0.02, top=-14.67657+0.04)

### Set a range
lat <- c(-14.67657-0.035, -14.67657+0.042)                
lon <- c(145.4542-0.02, 145.4542+0.03)   

#map <- get_googlemap(center=c(lon=145.46, lat=-14.67), size=c(350, 350), zoom = 12, maptype = "satellite") 


### Get a map
map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 14,
               maptype = "satellite", source = "google")


map1 <- ggmap(map) +
  geom_point(aes(long, lat), data = locs[locs$region=="GBR",],   shape = 21, color = "gray25", fill = "yellow")+
  #facet_wrap(~ region, nrow = 1) +
 labs(x="Longitude", y="Latitude")+
 #coord_cartesian(xlim=lon, ylim=lat)+
  #scale_x_continuous(limits = lon, expand = c(0, 0)) +
 # scale_y_continuous(limits = lat, expand = c(0, 0))+
  theme(axis.title = element_text(size=7), axis.text = element_text(size=7))
map1 


### Set a range
lat <- c(21.436618-0.015, 21.436618+0.01)                
lon <- c(-157.78742-0.005, -157.78742+0.0025)   

#map <- get_googlemap(center=c(lon=145.46, lat=-14.67), size=c(350, 350), zoom = 12, maptype = "satellite") 


### Get a map
map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 16,
               maptype = "satellite", source = "google")


map2 <- ggmap(map) +
  geom_point(aes(long, lat), data = locs[locs$region=="Hawaii",],   shape = 21, color = "gray25", fill = "yellow")+
  #geom_text(aes(long, lat, label=site), data = locs[locs$region=="Hawaii",], col="red")+  
  #facet_wrap(~ region, nrow = 1) +
 labs(x="Longitude", y="Latitude")+
 #coord_cartesian(xlim=lon, ylim=lat)+
  #scale_x_continuous(limits = lon, expand = c(0, 0)) +
  #scale_y_continuous(limits = lat, expand = c(0, 0))+
  theme(axis.title = element_text(size=7), axis.text = element_text(size=7))
map2

plot_grid(map1, map2)

#################################
################################# 
#################################

 
library("ggmap")
 
locs <- read.csv("latlongs.csv")
head(locs)

ggplot(locs, aes( long, lat))+geom_point()


qmplot(long, lat, data = locs[locs$region=="GBR",], maptype =   "stamen_terrain", color = I("red")) #"stamen_toner_lite"

qmplot(long, lat, data = locs[locs$region=="Hawaii",], maptype =   "stamen_terrain", color = I("red")) 

#"stamen_watercolor"


bbox <- c(left=145.4542-0.02, right=145.4542+0.03, bottom=-14.67657-0.02, top=-14.67657+0.04)#make_bbox(long, lat, data = locs[locs$region=="GBR",])
map <- get_stadiamap( bbox = bbox, maptype = , zoom = 14 )
#ggmap(map)

ggmap(map, darken = .3) +
  geom_point( aes(long, lat), data = locs[locs$region=="GBR",],   shape = 21, color = "gray25", fill = "yellow" )+
  #facet_wrap(~ region, nrow = 1) +
  labs(x="Longitude", y="Latitude")+
  theme(axis.title = element_text(size=7), axis.text = element_text(size=7))

