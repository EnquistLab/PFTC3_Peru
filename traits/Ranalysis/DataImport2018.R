# LOAD LIBRARIES
library("readxl")
library("tidyverse")
library("lubridate")
library("tpl")

pn <- . %>% print(n = Inf)


#### COORDINATES ####
coordinates <- read_excel("Coordinates_Peru_2018.xlsx")
head(coordinates)

coordinates_2018 <- coordinates %>% 
  rename(Latitude = lat, Longitude = lon, Elevation = ele, Time = time) %>% 
  select(Latitude, Longitude, Elevation, name, Time) %>% 
  filter(!name %in% c("CAR PARK TRES CRUCES", "PUMA", "QUELLO CCASA", "TRECRUCES UPPER", "WAYQECHA")) %>% 
  mutate(Site = substr(name, 1, 3)) %>%
  mutate(Plot = substr(name, nchar(name), nchar(name))) %>%
  mutate(Treatment = substr(name, nchar(name)-2, nchar(name)-1)) %>%
  mutate(Treatment = gsub(" ", "", Treatment))

save(file = "Coordinates_2018.Rdata", coordinates_2018)

coords <- coordinates_2018 %>% 
  group_by(Site) %>% 
  summarise(Lat = mean(Latitude), Long = mean(Longitude), Elev = mean(Elevation))
  



#### SPECIES LIST ####

# Read in species list and check names
species <- read_excel("community/data/2018-03-08-Species_List_2018.xlsx")

species <- species %>% 
  mutate(Species = gsub(" ", "_", Species))
  
# Check names with species list, tpl
sp.check <- tpl.get(species$Species)

sp.check %>% 
  select(name, note, original.search)



#### SPECIES COVER ####
cover <- read_excel("community/data/2018-03-07_Peru.cover.data.xlsx", sheet = "Cover", col_types = c("text", "text", "text", "numeric", "text", "numeric", "text", "text", "text", "text"))

cover <- cover %>% 
  mutate(site = plyr::mapvalues(site, c("WAY", "ACJ", "PIL", "TRE", "QUE"), c("WAY_3100m", "ACJ_3400m", "PIL_3600m", "TRE_3700m", "QUE_3900m"))) %>% 
  mutate(site = factor(site, level = c("WAY_3100m", "ACJ_3400m", "PIL_3600m", "TRE_3700m", "QUE_3900m"))) %>% 
  mutate(treatment = plyr::mapvalues(treatment, c("C", "B", "BB"), c("Late", "Early", "Very Early"))) %>%
  mutate(treatment = factor(treatment, level = c("Late", "Early", "Very Early"))) %>% 
  left_join(species, by = c("species" = "Species"))
  



# Calculate highest cover
cover.max <- cover %>% 
  group_by(site, species, treatment) %>% 
  summarise(n = n(), mean = mean(cover, na.rm = TRUE), se = sd(cover, na.rm = TRUE)/sqrt(n)) %>% 
  arrange(site, treatment, -mean, se)

write_csv(cover.max, path = "community/data/scover.max.csv")


### Cover in each site and by treatment
cover %>% 
  group_by(site, species, treatment) %>% 
  summarise(n = n(), mean = mean(cover, na.rm = TRUE), se = sd(cover, na.rm = TRUE)/sqrt(n)) %>% 
  filter(!is.na(mean)) %>% 
  filter(site == "WAY_3100m", treatment == "Control") %>% 
  ggplot(aes(x = species, y = mean, ymin = mean - se, ymax = mean + se, color = species)) +
  geom_point() +
  geom_errorbar() +
  labs(x = "", y = "Mean cover in %") +
  #facet_grid(treatment ~ site) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  

# Cover by functional Group
coverPlot <- cover %>% 
  group_by(site, treatment, FunctionalGroup) %>% 
  summarise(n = n(), mean = mean(cover, na.rm = TRUE), se = sd(cover, na.rm = TRUE)/sqrt(n)) %>%   
  filter(!is.na(FunctionalGroup), treatment != "Double Burnt") %>% 
  ggplot(aes(x = FunctionalGroup, y = mean, ymin = mean - se, ymax = mean + se, color = FunctionalGroup)) +
  geom_point(size = 3) +
  geom_errorbar() +
  labs(x = "", y = "Mean cover in %") +
  facet_grid(treatment ~ site) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 15),
        axis.text=element_text(size = 15),
        axis.title=element_text(size = 15))

ggsave(coverPlot, filename = "cover.jpeg", dpi = 300)

# Species richness
total <- cover %>% 
  group_by(site, treatment) %>% 
  distinct(species) %>% 
  summarise(total = n())

richnessPlot <- cover %>% 
  filter(!is.na(FunctionalGroup)) %>% 
  left_join(total, by = c("site", "treatment")) %>%
  group_by(site, treatment, FunctionalGroup) %>% 
  summarise(count = n_distinct(species)) %>% 
  ggplot(aes(x = FunctionalGroup, y = count, fill = FunctionalGroup)) +
  geom_bar(stat="identity") +
  labs(x = "", y = "Species richness (count)") +
  geom_text(aes(x = 5, y = 12, label = "total = ")) +
  facet_grid(treatment ~ site) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 15),
        axis.text=element_text(size = 15),
        axis.title=element_text(size = 15))
ggsave(richnessPlot, filename = "richness.jpeg", dpi = 300)

unique(cover$species)


# Get distinct species list
sp.list.2018 <- cover %>% distinct(species) %>% 
  mutate(species = tolower(species)) %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  left_join(species, by = c("species" = "Taxon")) %>% 
  select(species, Family)
  

write_csv(sp.list.2018, path = "community/sp.list.2018.csv")

