library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

cc <- coords %>% 
  select(Long, Lat) %>% 
  rename(lat = Lat, lon = Long)

cc2 <- coords %>% 
  select(Long, Lat, Site) %>% 
  rename(lat = Lat, lon = Long, name = Site)
  
# Peru
sq_map <- get_map(location = cc,  maptype = "terrain", source = "google", zoom = 6)

Peru.map <- ggmap(sq_map) + 
  geom_point(data = cc, color = "red", size = 2)
ggsave(Peru.map, filename = "Peru_map.jpeg", dpi = 300)

# Wayqecha map
sq_map2 <- get_map(location = cc,  maptype = "terrain", source = "google", zoom = 12)
Wayqecha.map <- ggmap(sq_map2) + 
  geom_point(data = cc, color = "red", size = 2) +
  geom_text(data = cc2, aes(label = paste("  ", as.character(name), sep="")), hjust = 0, color = "red")
ggsave(Wayqecha.map, filename = "Wayqecha_map.jpeg", dpi = 300)
