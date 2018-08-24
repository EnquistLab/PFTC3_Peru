source("DataImport2018.R")

load(file = "traits.cover2.Rdata")
Export_Species <- traits.cover %>% 
  select(Site, Elevation, family, Genus, Species, Experiment, Plot, Individual_nr, ID) %>% 
  left_join(coords, by = "Site") %>% 
  select(-Elev) %>% 
  group_by(Site, Elevation, Lat, Long, family, Genus, Species) %>% 
  mutate(Number_Plants = n()) %>% 
  arrange(Site, Elevation, family, Genus, Experiment, Plot)

writexl::write_xlsx(x = Export_Species, path = "ExportSpeciesList.xlsx")


# Check Cites sp list
sp <- Export_Species %>% 
  mutate(SP = paste(Genus, Species)) %>% 
  select(family, Genus, Species, SP) %>%
  #filter(family %in% c("Bromeliaceae", "Euphorbiaceae", "Orchidaceae", "Rubiaceae")) %>% 
  filter(family == "Orchidaceae") %>% 
  distinct(SP)



cites <- read_excel(path = "traits/cites_listings_2018-08-24 09_03_semicolon_separated.xlsx", col_names = TRUE)

cites %>% 
  filter(Family == "Orchidaceae", Genus == "Pterichis") %>% 
  select(Genus, Species) %>% pn

unique(cites$Family)
unique(sp$family)
  


