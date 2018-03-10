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

# Check names with species list, tpl
sp.check <- tpl.get(species$Species)

sp.check %>% 
  select(name, note, original.search) %>% 
  filter(note %in% c("was misspelled"))

species <- species %>% 
  mutate(Species = gsub(" ", "_", Species)) %>% 
  # remove duplicates
  distinct(Species, Family, FunctionalGroup) %>% 
  rename(species = Species, family = Family,  functionalGroup = FunctionalGroup)



#### SPECIES COVER ####
cover <- read_excel("community/data/2018-03-07_Peru.cover.data.xlsx", sheet = "Cover", col_types = c("text", "text", "text", "numeric", "text", "numeric", "text", "text", "text", "text"))

cover <- cover %>% 
  rename(successionalStage = treatment) %>% 
  mutate(site = plyr::mapvalues(site, c("WAY", "ACJ", "PIL", "TRE", "QUE"), c("WAY_3100m", "ACJ_3400m", "PIL_3600m", "TRE_3700m", "QUE_3900m"))) %>% 
  mutate(site = factor(site, level = c("WAY_3100m", "ACJ_3400m", "PIL_3600m", "TRE_3700m", "QUE_3900m"))) %>% 
  mutate(successionalStage = plyr::mapvalues(successionalStage, c("C", "B", "BB"), c("Late", "Early", "Recent"))) %>%
  mutate(successionalStage = factor(successionalStage, level = c("Late", "Early", "Recent"))) %>% 
  left_join(species, by = "species")
  

### Correct misspellings ####
# according to TNRS: Pseudognaphalium ramosissimum <- Gnaphalium ramosissimum

cover <- cover %>% 
  mutate(species = gsub("Acaena_cylindrystachya", "Acaena_cylindristachya", species),
         species = gsub("Paspallum_bonplandianum", "Paspalum_bonplandianum", species),
         species = gsub("Rhynchosphora_macrochaeta", "Rhynchospora_macrochaeta", species),
         species = gsub("Viola_pygmea", "Viola_pygmaea", species),
         species = gsub("Melpomene_miniliformis", "Melpomene_moniliformis", species))


# Calculate highest cover
cover.max <- cover %>% 
  group_by(site, species, successionalStage) %>% 
  summarise(n = n(), mean = mean(cover, na.rm = TRUE), se = sd(cover, na.rm = TRUE)/sqrt(n)) %>% 
  arrange(site, successionalStage, -mean, se)

write_csv(cover.max, path = "community/data/scover.max.csv")


### Cover in each site and by successionalStage
cover %>% 
  group_by(site, species, successionalStage) %>% 
  summarise(n = n(), mean = mean(cover, na.rm = TRUE), se = sd(cover, na.rm = TRUE)/sqrt(n)) %>% 
  filter(!is.na(mean)) %>% 
  filter(site == "WAY_3100m", successionalStage == "Early") %>% 
  ggplot(aes(x = species, y = mean, ymin = mean - se, ymax = mean + se, color = species)) +
  geom_point() +
  geom_errorbar() +
  labs(x = "", y = "Mean cover in %") +
  #facet_grid(treatment ~ site) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  

# Cover by functional Group
coverPlot <- cover %>% 
  group_by(site, successionalStage, functionalGroup) %>% 
  summarise(n = n(), mean = mean(cover, na.rm = TRUE), se = sd(cover, na.rm = TRUE)/sqrt(n)) %>%   
  filter(!is.na(functionalGroup), successionalStage != "Recentt") %>% 
  ggplot(aes(x = functionalGroup, y = mean, ymin = mean - se, ymax = mean + se, color = functionalGroup)) +
  geom_point(size = 3) +
  geom_errorbar() +
  labs(x = "", y = "Mean cover in %") +
  facet_grid(successionalStage ~ site) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 15),
        axis.text=element_text(size = 15),
        axis.title=element_text(size = 15))

ggsave(coverPlot, filename = "cover.jpeg", dpi = 300, height = 8, width = 12)

# Species richness
total <- cover %>% 
  group_by(site, successionalStage) %>% 
  distinct(species) %>% 
  summarise(total = n())

richnessPlot <- cover %>% 
  filter(!is.na(functionalGroup)) %>% 
  group_by(site, successionalStage, functionalGroup) %>% 
  summarise(count = n_distinct(species)) %>% 
  left_join(total, by = c("site", "successionalStage")) %>%
  ggplot(aes(x = functionalGroup, y = count, fill = functionalGroup)) +
  geom_bar(stat="identity") +
  labs(x = "", y = "Species richness (count)") +
  geom_text(aes(x = 4, y = 15, label = paste("total = ", total))) +
  facet_grid(successionalStage ~ site) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 15),
        axis.text=element_text(size = 15),
        axis.title=element_text(size = 15))
ggsave(richnessPlot, filename = "richness.jpeg", dpi = 300, height = 8, width = 12)

unique(cover$species)
cover %>% group_by(functionalGroup) %>% summarise(count = n_distinct(species))


# Get distinct species list
sp.list.2018 <- cover %>% distinct(species) %>% 
  mutate(species = tolower(species)) %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  left_join(species, by = c("species" = "Taxon")) %>% 
  select(species, Family)
  

write_csv(sp.list.2018, path = "community/sp.list.2018.csv")



cover %>% 
  filter(functionalGroup != c("Lichen", "Bryophytes")) %>% 
  group_by(site, successionalStage, plot) %>% 
  summarise(n = n()) %>% 
  group_by(site) %>% 
  summarise(mean = mean(n))


cover %>% 
  filter(functionalGroup != c("Lichen", "Bryophytes")) %>% 
  group_by(site, successionalStage, plot) %>% 
  summarise(n = n()) %>% 
  group_by(site) %>% 
  summarise(mean = mean(n))