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
  mutate(species = gsub("Baccharis_triconeata", "Baccharis_tricuneata", species),
         species = gsub("Cerastium_arvensiforme", "Cerastium_arvense", species)) %>% 
  #anti_join(species, by = "species") %>% distinct(species) # check with species do not match between species list and cover
  left_join(species, by = "species")
  

setdiff(cover$species, species$species)
setdiff(species$species, cover$species)


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

cover %>% filter(is.na(functionalGroup))


Species.list.field <- cover %>% 
  filter(!functionalGroup %in% c("Lichen", "Bryophytes")) %>% 
  arrange(site, successionalStage, plot, -cover) %>% 
  select(site, plot, successionalStage, species, cover, notes2)
write_csv(Species.list.field, path = "community/Species.list.field.csv")


#### LEAF AREA ####
load("traits/data/LeafArea.raw.Rdata", verbose = TRUE)
LeafArea2018 <- LeafArea.raw %>% 
  mutate(ID = substr(ID, 1, 7)) %>% 
  filter(!(ID == "BZW1631" &  LeafArea == 104.434)) %>% # remove large area, is black area on scan
  
  filter(ID != "VJD1110") %>% # double scan (other leaf is BDJ1110, where ID is also wrong)
  mutate(ID = gsub("ECV!179", "ECV1792", ID),
         ID = gsub("BAT0442", "BAT9442", ID),
         ID = gsub("BEZ1256", "BEZ1356", ID),
         ID = gsub("CSJ9255", "CZJ9255", ID),
         ID = gsub("EBO1873", "EBO1973", ID),
         ID = gsub("avs5435", "AVS5435", ID),
         ID = gsub("BCI3405", "BIC3405", ID),
         ID = gsub("BDJ1110", "BJD1110", ID),
         ID = gsub("bov5280", "BOV5280", ID),
         ID = gsub("BTK6387", "BTK6307", ID),
         ID = gsub("egg2246", "EGG2246", ID)) %>% 
  group_by(ID) %>% 
  summarise(Area_cm2 = sum(LeafArea)) %>% 
  # remove duplicate/tripilcante scans
  mutate(Area_cm2 = ifelse(ID == "BCE2654", Area_cm2 / 3, Area_cm2)) %>% # triple scan
  mutate(Area_cm2 = ifelse(ID == "AGX5711", Area_cm2 / 2, Area_cm2)) # double scan

  
  
#### LEAF TRAITS ####
traits0 <- read_excel(path = "traits/data/LeafArea_Peru_18-03-2018_SMK.xlsx")
traits1 <- read_excel(path = "traits/data/LeafArea_Peru_19-03-2018_SMK.xlsx")
traits2 <- read_excel(path = "traits/data/LeafArea_Peru_2018_AI.xlsx")
traits3 <- read_excel(path = "traits/data/LeafArea_Peru_2018_AST.xlsx")
traits3 <- traits3 %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
traits4 <- read_excel(path = "traits/data/LeafArea_Peru_2018_EK.xlsx")
traits5 <- read_excel(path = "traits/data/LeafArea_Peru_2018_ErikAlyssa.xlsx")
traits6 <- read_excel(path = "traits/data/LeafArea_Peru_2018_IO.xlsx")
traits6 <- traits6 %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
traits7 <- read_excel(path = "traits/data/LeafArea_Peru_2018_KL.xlsx")
traits7 <- traits7 %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
traits8 <- read_excel(path = "traits/data/LeafArea_Peru_2018_SMK.xlsx")
traits9 <- read_excel(path = "traits/data/LeafArea_Peru_2018_TEM_19.03.2018.xlsx")
traits9 <- traits9 %>% mutate(Date = as.Date(Date, format = "%d.%m.%Y"))
traits10 <- read_excel(path = "traits/data/LeafArea_Peru_2018_VZ_2.xlsx")
traits11 <- read_excel(path = "traits/data/LeafArea_Peru_2018_VZ.xlsx")

# Merge tables
traits.raw <- traits1 %>% 
  rbind(traits2, traits3, traits4, traits5, traits6, traits7, traits8, traits9, traits10, traits11) %>% 
  filter(!is.na(ID)) # remove empty rows
save(traits.raw, file = "traits/data/traits.raw.Rdata")

# Merge traits and LeafArea  and clean data
traits <- traits.raw %>% 
  ### FIX WROND LEAF ID's
  mutate(ID = gsub("WSY0063", "ESY0063", ID)) %>% 
  mutate(ID = gsub("EEA7841", "EAA7841", ID)) %>% 
  mutate(ID = gsub("EOP2126", "EOP2116", ID)) %>% 
  mutate(ID = gsub("ELN57788", "ELN5788", ID)) %>% 
  mutate(ID = gsub("EIH0038", "EHI0038", ID)) %>% 
  mutate(ID = gsub("EFD9867", "EFP9867", ID)) %>% 
  mutate(ID = gsub("EKB8895", "EBK8895", ID)) %>% 
  mutate(ID = gsub("CJO6932", "COJ6932", ID)) %>% 
  mutate(ID = gsub("DJV0218", "DJV0318", ID)) %>% 
  mutate(ID = gsub("DMM5647", "DMM5645", ID)) %>% 
  mutate(ID = gsub("DGE6973", "DGE6927", ID)) %>% 
  mutate(ID = gsub("DCB", "DCB6902", ID)) %>% 
  mutate(ID = gsub("DHG7363", "DHG7383", ID)) %>% 
  mutate(ID = gsub("CMO0287", "CMO2587", ID)) %>% 
  mutate(ID = gsub("CZV0169", "CZV0159", ID)) %>% 
  mutate(ID = gsub("ELB6979", "ELB6970", ID)) %>% 
  mutate(ID = gsub("FAG3940", "FAG3950", ID)) %>% 
  mutate(ID = gsub("CQ06175", "CQO6175", ID)) %>% 
  #mutate(ID = gsub("EXP4335", "???", ID)) %>%
  mutate(ID = gsub("DQO712", "DQO7122", ID)) %>%
  
  ### JOIN TRAITS WITH LEAF AREA
  left_join(LeafArea2018, by = "ID") %>% 
  
  ### FIX WRONG DATA
  # wrong elevation
  mutate(Elevation = ifelse(Site == "PIL" & Elevation %in% c(3600, 3700), 3675, Elevation)) %>% #probably add 3475, but it's ACJ elevation
  mutate(Elevation = ifelse(Site == "ACJ" & Elevation %in% c(3400, 3440), 3475, Elevation)) %>% 
  # missing experiment
  mutate(Experiment = ifelse(ID == "ETC9124", "C", Experiment)) %>% 
  mutate(Experiment = plyr::mapvalues(Experiment, c("B", "BB", "C", "E", "EARLY", "L", "LATE", NA), c("B", "BB", "C", "B", "B", "C", "C", NA))) %>% 
  # wrong individual number
  ### CHECK IF I DO NOT OVERWRITE THE ACTUAL IND_NR 5!!!!
  mutate(Individual_nr = ifelse(Site == "WAY" & Genus == "Eriosorus" & Experiment == "C" & Plot == 2, 5, Individual_nr)) %>%  
  
  ### CALCULATE AREA, SLA, etc.
  # Sisyrinchium: leaves are folded: area needs to be doubled and leaf thickness halfed!!!!
  
  
  ### FIXING COMMENTS
  mutate(Comment = ifelse(ID == "ENF3830", paste(Comment, "_dirt"), Comment)) %>% 
  
  
  ### FLAG DATA
  ## AREAFLAG
  mutate(AreaFlag = ifelse(ID == "BOT1325", "DryLeaf_wrongArea")) %>%  # leaf was very dry, wrong area
  mutate(AreaFlag = ifelse(ID %in% c("ECN1410", "EPV0866", "FCC3736", "FDL7538"), "LeafOutside_wrongArea")) %>%   # Leaf on ruler or on edge - nothing one can do about it - area flag
  mutate(AreaFlag = ifelse(ID == "EMY0414", "TooLargeArea")) %>% # Lycopodiella, more than2 branches scanned
  
  ## DRYWEIGHTFLAG
  mutate(DryFlag = ifelse(ID == "EMY0414", "TooLargeWeight")) # Lycopodiella, more than2 branches scanned

## WETWEIGHTFLAG
mutate(WetFlag = ifelse(ID == "EMY0414", "TooLargeWeight")) # Lycopodiella, more than2 branches scanned

### Fix species names
traits %>% 
  mutate(Taxon = paste(Genus, Species, sep = " ")) %>% filter(Taxon == "Paspalum Sean")
  mutate(Taxon = gsub("Elaphoglossum sp. ", "Elaphoglossum sp.", Taxon)) %>% 
  mutate(Taxon = gsub("Carex P", "Carex pinchinensis", Taxon)) %>% 
  mutate(Taxon = gsub("Bromus sp. Blue", "Bromus blue", Taxon)) %>% 
  mutate(Taxon = gsub("Eriosorus chelianthoides", "Eriosorus cheilanthoides", Taxon)) %>% 
  mutate(Taxon = gsub("Eriosorus sp", "Eriosorus sp.", Taxon)) %>% 
  mutate(Taxon = gsub("Nertera  granadensis", "Nertera granadensis", Taxon)) %>% 
  mutate(Taxon = gsub("Orchio sp.", "Orchid sp.", Taxon)) %>% 
  mutate(Taxon = gsub("Pernettya sp", "Pernettya sp.", Taxon)) %>% 
  mutate(Taxon = gsub("Werneria nugibena", "Werneria nubigena", Taxon))

Cortaderia NA = Cortaderia sp?
Hieracium NA = Hieracium sp?
Melpomene NA = Melpomene sp?
Paspalum bonplandianum=Paspalum Sean=Paspalum sp. ???
Rhynchosphora 5 different spellings

  EOR9773 -> project sean
  

### some 17.3.2018 QEL are actually WAY
