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
  
#### SPECIES COVER ####



# Read in species list and check names
species <- read_excel("community/data/Base_de_Datos_Proyecto Puna_drive.xlsx")
species <- species %>% 
  mutate(Taxon = paste(Genero, Specie, sep = "_")) %>% 
  mutate(Taxon = gsub("cf. ", "", Taxon)) %>% 
  mutate(Taxon = gsub("\\.", "", Taxon)) %>% 
  mutate(Taxon = gsub("NA_NA", "", Taxon)) %>% 
  mutate(Taxon = gsub("_", " ", Taxon)) %>% 
  mutate(Taxon = tolower(Taxon))

  
# Check names with species list, tpl
sp.check <- tpl.get(species$Taxon)

sp.check %>% 
  select(name, note, original.search)

cover <- read_excel("community/data/Peru.cover.data_Kopie.xlsx", sheet = "Cover", col_types = c("text", "text", "text", "numeric", "text", "numeric", "text", "text", "text", "text"))

# Make species list
cover %>% distinct(Site)

cover <- cover %>% 
  mutate(cover2 = ifelse(cover == "0.0001", "NA", cover)) %>% 
  mutate(site = plyr::mapvalues(site, c("WAY", "ACJ", "PIL", "TRE", "QUE"), c("WAY_3100m", "ACJ_3400m", "PIL_3600m", "TRE_3700m", "QUE_3900m"))) %>% 
  mutate(site = factor(site, level = c("WAY_3100m", "ACJ_3400m", "PIL_3600m", "TRE_3700m", "QUE_3900m"))) %>% 
  mutate(treatment = plyr::mapvalues(treatment, c("C", "B", "BB"), c("Control", "Burnt", "Double Burnt"))) %>%
  mutate(treatment = factor(treatment, level = c("Control", "Burnt", "Double Burnt")))
  

### Cover in each site and by treatment
cover %>% 
  group_by(site, species, treatment) %>% 
  summarise(mean.cover = mean(cover2, na.rm = TRUE)) %>% 
  filter(!is.na(mean.cover)) %>% 
  filter(species != "Stipa_ichu") %>% 
  ggplot(aes(x = species, y = mean.cover, color = species)) +
  geom_point() +
  facet_grid(treatment ~ site) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
# Number of species
cover %>% 
  group_by(site, treatment) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = site, y = count, color = treatment)) +
  geom_point() +
  scale_color_manual(name = "Treatment", values = c("blue", "red", "orange"))

sp.list.2018 <- cover %>% distinct(species) %>% 
  mutate(species = tolower(species)) %>% 
  mutate(species = gsub("_", " ", species)) %>% 
  left_join(species, by = c("species" = "Taxon")) %>% 
  select(species, Family)
  

write_csv(sp.list.2018, path = "community/sp.list.2018.csv")

