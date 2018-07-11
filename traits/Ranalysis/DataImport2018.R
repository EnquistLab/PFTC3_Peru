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


load(file = "community/gramm.comm.cover.Rdata", verbose = TRUE)
graminoids <- gram.comm[,-5] %>% as.tibble() %>% mutate(cover = as.numeric(gsub("\\+", "0.5", cover)))

graminoids <- graminoids %>% 
  select(site, plot, treatment, genus, cover) %>% 
  group_by(site, plot, treatment, genus) %>% 
  summarise(cover = sum(cover)) %>% 
  mutate(family = ifelse(genus %in% c("Agrostis", "Bromus", "Calamagrostis", "Paspallum", "Cortaderia", "Poa", "Poaceae", "Phippsia", "Neurolepis"), "Poaceae", NA)) %>% 
  mutate(family = ifelse(genus %in% c("Carex", "Rhynchospora", "Scirpus"), "Cyperaceae", family)) %>% 
  mutate(family = ifelse(genus %in% c("Luzula", "Juncus"), "Juncaceae", family))


forbs <- read_excel(path = "community/data/2018-03-17_Peru.cover.data_UpdatedForbs.xlsx", sheet = "Cover")
forbs <- forbs %>% mutate(cover = as.numeric(gsub("\\+", "0.5", cover))) %>% 
  select(site, plot, treatment, Family, Genus, cover) %>% 
  group_by(site, plot, treatment, Family, Genus) %>% 
  summarise(cover = sum(cover)) %>% 
  rename(genus = Genus, family = Family)

all.cover <- graminoids %>% 
  rbind(forbs)

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
  
  # Remove non leaf parts of scan
  filter(!(ID == "BZW1631" &  LeafArea == 104.434)) %>% # remove large area, is black area on scan
  filter(!(ID == "EUO3529" & LeafArea == 0.192)) %>% # remove small part, cable on scan
  filter(!(ID == "AOF7984" & LeafArea == 0.187)) %>% # remove part, non leaf on scan
  
  # remove double scan (other leaf is BDJ1110, where ID is also wrong)
  filter(ID != "VJD1110") %>% 
  
  # Fix leafID
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
         ID = gsub("ADP5352", "ADP5320", ID),
         ID = gsub("AEB7575", "AEB7555", ID),
         ID = gsub("AEY3890", "AEY3990", ID),
         ID = gsub("AH01919", "AHO1919", ID),
         ID = gsub("AHF977", "AHY977", ID),
         ID = gsub("AIR5353", "AIH5353", ID),
         ID = gsub("ANP7254", "ANP7205", ID),
         ID = gsub("BSQ6672", "BQS6672", ID),
         ID = gsub("CDF7159", "CFD7159", ID),
         ID = gsub("GNK310.", "GNK3140", ID),
         ID = gsub("HBVO473", "HBV0473", ID),
         ID = gsub("egg2246", "EGG2246", ID)) %>% 
  
  # Sum areas for each ID
  group_by(ID) %>% 
  summarise(Area_cm2 = sum(LeafArea)) %>% 

  # replace LeafArea with NA - empty or corrupted scan
  mutate(Area_cm2 = ifelse(ID == "AUB2092", NA, Area_cm2)) %>% 
  add_row(ID = "BMB7274", Area_cm2 = NA) %>% 
  add_row(ID = "EHP2066", Area_cm2 = NA) %>% 
  add_row(ID = "FDF1809", Area_cm2 = NA)



#### LEAF TRAITS ####
files <- dir(path = "traits/data/", pattern = "\\.xlsx$", full.names = TRUE)
traits.raw <- files[grepl("^(?!~)", basename(files), perl = TRUE)] %>% 
  set_names(basename(.)) %>% 
  map_df(read_excel, col_types = c("text", "date", "text", "numeric", rep("text", 4), rep("numeric", 8), "text", "numeric", "text"), .id = "file")

save(traits.raw, file = "traits/data/traits.raw.Rdata")

# Merge traits and LeafArea  and clean data
traits <- traits.raw %>% 
  ### FIX WROND LEAF ID's
  mutate(ID = gsub("WSY0063", "ESY0063", ID),
         ID = gsub("EEA7841", "EAA7841", ID),
         ID = gsub("EOP2126", "EOP2116", ID),
         ID = gsub("ELN57788", "ELN5788", ID),
         ID = gsub("EIH0038", "EHI0038", ID),
         ID = gsub("EFD9867", "EFP9867", ID),
         ID = gsub("EKB8895", "EBK8895", ID),
         ID = gsub("CJO6932", "COJ6932", ID),
         ID = gsub("DJV0218", "DJV0318", ID),
         ID = gsub("DMM5647", "DMM5645", ID),
         ID = gsub("DGE6973", "DGE6927", ID),
         ID = gsub("DCB", "DCB6902", ID),
         ID = gsub("DHG7363", "DHG7383", ID),
         ID = gsub("CMO0287", "CMO2587", ID),
         ID = gsub("CZV0169", "CZV0159", ID),
         ID = gsub("ELB6979", "ELB6970", ID),
         ID = gsub("FAG3940", "FAG3950", ID),
         ID = gsub("CQ06175", "CQO6175", ID),
         ID = gsub("DQO712", "DQO7122", ID),
         ID = gsub("CZZ7222", "DQO7122", ID),
         ID = gsub("BDG9657", "BGD9657", ID),
         ID = gsub("DBE080", "DBE0880", ID),
         ID = gsub("CHK3202", "CHK3203", ID),
         ID = gsub("CFX6171", "CFX6172", ID),
         ID = gsub("BQT3166", "BQT1366", ID),
         ID = gsub("BLM4402", "BML4402", ID),
         ID = gsub("ALD", "ALD3826", ID),
         ID = gsub("GZM1878", "GMZ1878", ID),
         ID = gsub("ARR112", "ARR1112", ID),
         ID = gsub("GWV45096", "GWV4596", ID),
         ID = gsub("AWR2030", "AWR2040", ID),
         ID = gsub("DDC220", "DDC2200", ID),
         ID = gsub("AVE4872", "AVE4827", ID),
         ID = gsub("BQG5351", "BQH5351", ID),
         ID = gsub("EXP4335", "EXK4335", ID), # maybe UEX4335?
         ID = gsub("CTG2458", "CTG2468", ID),
         ID = gsub("AKB5462", "AKB5492", ID),
         ID = gsub("HKL5161", "HKL5191", ID)) %>% 
  filter(!is.na(ID)) %>% # remove 46 lines that have ID as NA, empty rows
  
  ### JOIN TRAITS WITH LEAF AREA
  left_join(LeafArea2018, by = "ID") %>% 
  
  ### FIX WRONG DATA
  mutate(Site = plyr::mapvalues(Site, c("AJC", "PIL", "WAY", "ACJ", "TRE", "QUE", "Wayqecha", "Way", "QYE"), c("ACJ", "PIL", "WAY", "ACJ", "TRE", "QUE", "WAY", "WAY", "QUE"))) %>% 
  # Fix elevation (WAY:3100m; ACJ:3475m; PIL:3675m ; TRE:3715m; QUE:3888m)
  mutate(Elevation = ifelse(Site == "PIL" & Elevation %in% c(2675, 3600, 3647, 3650, 3670, 3700), 3675, Elevation)) %>% #probably add 3475, but it's ACJ elevation
  mutate(Elevation = ifelse(Site == "ACJ" & Elevation %in% c(3400, 3457, 3465, 3467, 3474, 3487, 3567, 3600, 3440), 3475, Elevation)) %>% 
  mutate(Elevation = ifelse(Site == "TRE" & Elevation %in% c(3700, 3701, 3702, 3710), 3715, Elevation)) %>% 
  mutate(Elevation = ifelse(Site == "QUE" & Elevation %in% c(3800, 3900), 3888, Elevation)) %>% 
  
  # missing experiment
  mutate(Project = ifelse(ID %in% c("EYX1643", "EOT2012"), "SEAN", Project)) %>% 
  mutate(Experiment = ifelse(ID %in% c("EYX1643", "EOT2012"), "", Experiment)) %>% 
  mutate(Experiment = ifelse(ID == "ETC9124", "C", Experiment)) %>% 
  mutate(Experiment = plyr::mapvalues(Experiment, 
                                      c("B", "Burn", "BB", "C", "c", "E", "EARLY", "Early", "early", "early-E", "L", "LATE", "Late", "late", "missing experiment"), 
                                      c("B", "B", "BB", "C", "C","B", "B", "B", "B", "B", "C", "C", "C", "C", NA))) %>% 
  # wrong individual number
  ### CHECK IF I DO NOT OVERWRITE THE ACTUAL IND_NR 5!!!!
  mutate(Individual_nr = ifelse(Site == "WAY" & Genus == "Eriosorus" & Experiment == "C" & Plot == 2, 5, Individual_nr)) %>%  
  
  ### CALCULATE AREA, SLA, etc.
  # Sisyrinchium: leaves are folded: area needs to be doubled and leaf thickness halfed!!!!
  
  
  ### FIXING COMMENTS
  mutate(Comment = ifelse(ID == "ENF3830", paste(Comment, "_dirt"), Comment)) %>% 
  
  ### FLAG DATA
  ## AREAFLAG
  mutate(AreaFlag = ifelse(ID == "BOT1325", "DryLeaf_wrongArea", "")) %>%  # leaf was very dry, wrong area
  mutate(AreaFlag = ifelse(ID %in% c("ECN1410", "EPV0866", "FCC3736", "FDL7538"), "LeafOutside_wrongArea", "")) %>%   # Leaf on ruler or on edge - nothing one can do about it - area flag
  mutate(AreaFlag = ifelse(ID == "EMY0414", "TooLargeArea", "")) %>% # Lycopodiella, more than2 branches scanned
  # Part missing on scan - wrong area
  mutate(AreaFlag = ifelse(ID %in% c("CZL9321", "DUO6664", "DWL3144", "DWV2987", "EFU8488", "EPV0866", "EPW2330", "ERV6823", "ERW0817", "EUG2994", "HHV3850"), "Cut_wrongArea", "")) %>% 

  # Empty or corrupted scan
  mutate(AreaFlag = ifelse(ID == "BMB7274", "EmptyScan_noArea", "")) %>% 
  mutate(AreaFlag = ifelse(ID == "EHP2066", "CorruptedScan_noArea", "")) %>% 
  mutate(AreaFlag = ifelse(ID == "FDF1809", "CorruptedScan_noArea", "")) %>% 
  mutate(AreaFlag = ifelse(ID == "AUB2092", "ScannedOnWrongSide_noArea", "")) %>% 
  
  ## DRYWEIGHTFLAG
  mutate(DryFlag = ifelse(ID == "EMY0414", "TooLargeWeight", "")) %>%  # Lycopodiella, more than2 branches scanned

## WETWEIGHTFLAG
mutate(WetFlag = ifelse(ID == "EMY0414", "TooLargeWeight", "")) # Lycopodiella, more than2 branches scanned

sp <- traits %>% 
  select(Genus) %>% arrange(Genus) %>% distinct(Genus)
write.csv(sp, file = "sp.csv")

traits.fixed.genus <- traits %>% 
  mutate(Genus = gsub("Achemilla|Alchemilla ", "Alchemilla", Genus),
         Genus = gsub("Belonathus|Belonauthus", "Belonanthus", Genus),
         Genus = gsub("Calamagostus", "Calamagrostis", Genus),
         Genus = gsub("Cordateria", "Cortaderia", Genus),
         Genus = gsub("Elaphoglossum ", "Elaphoglossum", Genus),
         Genus = gsub("Gaulteria", "Gaultheria", Genus),
         Genus = gsub("Gentranella", "Gentianella", Genus),
         Genus = gsub("Geufiaua|Geutiana", "Gentiana", Genus),
         Genus = gsub("Hypocheris|Hypoehaens", "Hypochaeris", Genus),
         Genus = gsub("Hypsophila", "Hysophila", Genus),
         Genus = gsub("Lysopomia", "Lysipomia", Genus),
         Genus = gsub("Melpome|Melpone", "Melpomene", Genus),
         Genus = gsub("Nertera |Netera", "Nertera", Genus),
         Genus = gsub("Orchio|Orquidede|Orchid", "Orchidaceae", Genus),
         Genus = gsub("Oritrophilum|Oritrophium|Orithrophium", "Oritrophium", Genus),
         Genus = gsub("Perzia", "Perezia", Genus),
         Genus = gsub("Prenettya", "Pernettya", Genus),
         Genus = gsub("Pterichius|Pterichris", "Pterichis", Genus),
         Genus = gsub("Rhynchosphora|Rhyncosphora|Rhyncospora|Rynchosphora", "Rhynchospora", Genus),
         Genus = gsub("Scripus", "Scirpus", Genus),
         Genus = gsub("Senecia", "Senecio", Genus),
         Genus = gsub("Werneria ", "Werneria", Genus),
         Genus = gsub("new", "New", Genus),
         Genus = gsub("Neurol", "Neurolepis", Genus),
         Genus = gsub("Paspalum", "Paspallum", Genus),
         Genus = gsub("Myconia", "Miconia", Genus),
         Genus = gsub("Gamachaeta", "Gamochaeta", Genus),
         Genus = gsub("Niphogetum", "Niphogeton", Genus),
         Genus = gsub("Oerithales", "Oreithales", Genus)) 
  
save(traits.fixed.genus, file = "traits.fixed.genus.Rdata")



traits.fixed.genus %>% 
  select(Genus) %>% arrange(Genus) %>% distinct(Genus) %>% pn


traits.cover <- traits.fixed.genus %>% 
  left_join(all.cover, by = c("Site" = "site", "Plot" = "plot", "Experiment" = "treatment", "Genus" = "genus"))
save(traits.cover, file = "traits.cover.Rdata")
traits.cover %>% filter(is.na(cover))

library("taxize")
names <- gnr_resolve(names = sp$Genus, db = "tnrs")
tnrs(sp$Genus)

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
