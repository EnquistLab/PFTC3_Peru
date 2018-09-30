source("DataImport2018.R")

load(file = "traits.cover2.Rdata")
Export_Species <- traits.cover %>% 
  select(Site, Elevation, family, Genus, Species, Experiment, Plot, Individual_nr, ID, Nr_leaves) %>% 
  mutate(Nr_leaves = ifelse(is.na(Nr_leaves), 1, Nr_leaves)) %>% 
  left_join(coords, by = "Site") %>% 
  select(Site, Elevation, Lat, Long, family, Genus, Species, ID, Nr_leaves) %>% 
  #summarize(Number_Plants = n(), AllIDs = paste(ID, collapse = ", "), Nr_Leaf = paste(Nr_leaves, collapse = ", ")) %>% 
  mutate(Nr_leaves = ifelse(Nr_leaves == "8 cm", 1, Nr_leaves)) %>% 
  arrange(Site, Elevation, family, Genus)


load(file = "traits.fixed.genus.Rdata")
Export_Species <- traits.fixed.genus %>% 
  select(Site, Elevation, Genus, Species, Treatment, PlotID, Individual_nr, ID, NrLeaves) %>%
  left_join(coords, by = "Site") %>% 
  left_join(SPlist, by = c("Genus" = "genus")) %>% 
  select(Site, Elevation, Lat, Long, family, Genus, Species, ID, NrLeaves) %>% 
  arrange(Site, Elevation, family, Genus, Species, ID)

writexl::write_xlsx(x = Export_Species, path = "18-09-11_ExportSpeciesList.xlsx")


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
  


# For Tucson
load(file = "traits/data/traits_2018_Peru_cleaned.Rdata")
Export_SpeciesTUCSON <- traits_2018_Peru_cleaned %>% 
  select(Site, Elevation, Genus, Species, Treatment, PlotID, Individual_nr, ID, NrLeaves) %>% 
  #mutate(Nr_leaves = ifelse(is.na(Nr_leaves), 1, Nr_leaves)) %>% 
  group_by(Genus, Species) %>% 
  summarize(NumberPlants = n()) %>% 
  arrange(Genus, Species) %>% print(n = Inf)

writexl::write_xlsx(x = Export_SpeciesTUCSON, path = "18-09-19_ExportSpeciesList_TUCSON.xlsx")


