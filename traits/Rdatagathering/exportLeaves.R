load(file = "traits.cover2.Rdata")
Export_Species <- traits.cover %>% 
  select(Site, Elevation, family, Genus, Species, Experiment, Plot, Individual_nr) %>% 
  left_join(coords, by = "Site") %>% 
  select(-Elev) %>% 
  group_by(Site, Elevation, Lat, Long, family, Genus, Species) %>% 
  summarise(Number_Plants = n())

write.csv(Export_Species, file = "ExportSpeciesList.csv")
