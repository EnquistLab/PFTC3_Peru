#### IMPORT DATA ####

library("readxl")
library("tidyverse")
library("lubridate")
library("tpl")


# check which data can be merged
# add units to variables

#**********************************************************************************
#### ALL_PRODUCTIVITY

### VEGETATION
vegetation <- read_excel("traits/data/immasdata/All_productivity.xlsx", sheet = "Vegetación")

vegetation <- vegetation %>%
  rename(Valley = VALLEY, Site = SITE, Year = YEAR, Month = MONTH, Code = CODE, Fire = FIRE, Fence = TREAT, Plot = PLOT, Column = COLUMN, Quadrant = QUADRANT, Genus = GENDER, Circumference1 = CIRCMF1, Circumference2 = CIRCMF2, Height1 = HEIGHT1, Height2 = HEIGHT2, Diameter1 = DIAMETER1, Diameter2 = DIAMETER2) %>% 
  filter(!is.na(Fire)) %>% 
  mutate(Fire = plyr::mapvalues(Fire, c("B", "U"), c("Burned", "Unburned"))) %>% 
  mutate(Fence = plyr::mapvalues(Fence, c("Uf", "F"), c("Unfenced", "Fenced")))

# create meta data
MetaData <- vegetation %>% 
  distinct(Site, Fire, Fence, Plot, Column, Quadrat)




### ABOVEGROUND BIOMASS
abovegrbiomass <- read_excel("traits/data/immasdata/All_productivity.xlsx", sheet = "Pesos Above Ground")

abovegrbiomass <- abovegrbiomass %>%
  select(1:(ncol(.)-1)) %>% # remove empty column at end
  rename(Valley = VALLEY, Site = SITE, Year = YEAR, Month = MONTH, Code = CODE, Fire = FIRE, Fence = TREAT, Plot = PLOT, Column = COLUMN, Quadrant = QUADRANT, WetMass = `MOST WEIGTH`, DryMass = `DRY WEIGTH`) %>% 
  mutate(Fire = plyr::mapvalues(Fire, c("B", "U"), c("Burned", "Unburned"))) %>% 
  mutate(Fence = plyr::mapvalues(Fence, c("Uf", "F"), c("Unfenced", "Fenced")))


### DEVELOPMENT
development <- read_excel("traits/data/immasdata/All_productivity.xlsx", sheet = "Desarrollo ESP.")

development <- development[,1:12] # remove empty columns at end

development <- development %>%
  rename(Valley = VALLEY, Site = SITE, Year = YEAR, Month = MONTH, Code = CODE, Fire = FIRE, Fence = TREAT, Plot = PLOT, Column = COLUMN, Quadrant = QUADRANT, Genus = GENDER, Height = HEIGHT) %>% 
  mutate(Fire = plyr::mapvalues(Fire, c("B", "U"), c("Burned", "Unburned"))) %>% 
  mutate(Fence = plyr::mapvalues(Fence, c("Uf", "F"), c("Unfenced", "Fenced")))


### BELOWGROUND BIOMASS
belowgrbiomass <- read_excel("traits/data/immasdata/All_productivity.xlsx", sheet = "Pesos Below Ground")

belowgrbiomass <- belowgrbiomass %>%
  rename(Valley = VALLEY, Site = SITE, Year = YEAR, Month = MONTH, Code = CODE, Fire = FIRE, Fence = TREAT, Plot = PLOT, Column = COLUMN, Cylinder = CYLINDER, Time = TIME, SoilOrganic = `SOIL ORG`, SoilInorganic = `SOIL INORG`) %>% 
  mutate(Fire = plyr::mapvalues(Fire, c("B", "U"), c("Burned", "Unburned"))) %>% 
  mutate(Fence = plyr::mapvalues(Fence, c("Uf", "F"), c("Unfenced", "Fenced")))


### CLIMATE
# MISSING COLUMN NAMES!!!
# Only for November
#climate <- read_excel("traits/data/All_productivity.xlsx", sheet = "Humedad-Temperatura")





### RESPIRATION
respiration <- read_excel("traits/data/immasdata/All_productivity.xlsx", sheet = "Respiracion")

respiration <- respiration %>%
  rename(Valley = VALLEY, Site = SITE, Year = YEAR, Code = CODE, Fire = FIRE, Fence = TREAT, Plot = PLOT, Column = COLUMN, Cylinder = CYLINDER, Plot2 = `;Plot`, CO2Ref = `CO2 Ref`, mbRef = `mb Ref`, mbRTemp = `mbR Temp`, InputA = `Input A`, InputB = `Input B`, InputC = `Input C`, InputD = `Input D`, InputE = `Input E`, InputF = `Input F`, InputG = `Input G`, InputH = `Input H`) %>% 
  mutate(Fire = plyr::mapvalues(Fire, c("B", "U"), c("Burned", "Unburned"))) %>% 
  mutate(Fence = plyr::mapvalues(Fence, c("Uf", "F"), c("Unfenced", "Fenced")))


#**********************************************************************************
### SPECIES
sp <- read_excel(path = "traits/data/immasdata/Especies Puna.xlsx")

species <- sp %>% 
  select(Copdigo, Especie) %>% 
  rename(Code.Imma = Copdigo, SpeciesName = Especie)

taxonomy <- plyr::ldply(strsplit(species$SpeciesName, " "), function(x){
  data_frame(speciesName = paste(x[1:2], collapse = " "))
}) %>% 
  mutate(speciesName = gsub("\\(Less.\\)", "spp", speciesName))

# Check names with species list, tpl
tpl.get(taxonomy$speciesName)
# problem species:
# Calamagrostis rupestris was misspelled|replaced synonym Calamagrostis longearistata
# Gaultheria myrsinoides                replaced synonym         Pernettya prostrata


### SPECIES COMPOSITION
# import species list
species <- read_excel("traits/data/immasdata/Species.xlsx")

taxonomy.checks <- tpl.get(taxanomy$Species)
fix.dictionary <- taxonomy.checks %>% 
  filter(note == "was misspelled") %>% 
  select(name, original.search)
#Festuca dolichophylla, Perezia pungens, Halenia bella, Senecio rhizomatus, Ageratina cuzcoensis

taxonomy <- species %>% 
  mutate(Species = gsub(" \\(\\d\\)", "", Species)) %>% 
  left_join(fix.dictionary, by = c("Species" = "original.search")) %>% 
  # replace misspelled names
  mutate(Species = ifelse(!is.na(name), name, Species)) %>% 
  mutate(SpID = substr(Species, 1, 4))


abundance <- read_excel("traits/data/immasdata/Species_comp.xlsx", sheet = "Species_comp")

abundance <- abundance %>% 
  mutate(Year = substr(Date, 9, 10)) %>% 
  mutate(Year = plyr::mapvalues(Year, c("10", "11", "12"), c(2010, 2011, 2012))) %>% 
  mutate(Month = substr(Date, 6, 7)) %>%
  mutate(Year = as.numeric(Year), Month = as.numeric(Month)) %>% 
  gather(key = Species, value = Abundance, -Treatment, -Date) %>% 
  left_join(MetaData, by = c("Treatment" = "Plot"))




#**********************************************************************************
#### ALL_MATRIX

### ABOVEGROUND BIOMASS
agbiomass <- read_excel("traits/data/immasdata/All_matrix_2017-11-08.xlsx", sheet = "AGB_matrix")

agbiomass <- agbiomass %>% 
  rename(Site = SITE, Fire = FIRE, AGbiomass_Mg_ha = AGB.Tnha, AGbiomass_Mg_ha_month = AGB.Tnham) %>% 
  mutate(Fire = plyr::mapvalues(Fire, c("B", "U"), c("Burned", "Unburned"))) %>% 
  mutate(Fence = plyr::mapvalues(Fence, c("Uf", "F"), c("Unfenced", "Fenced")))

### BELOWGROUND BIOMASS
bgbiomass <- read_excel("traits/data/immasdata/All_matrix_2017-11-08.xlsx", sheet = "BGP_matrix")

bgbiomass <- bgbiomass %>% 
  rename(Site = SITE, Fire = FIRE, BGbiomass_Mg_ha = BGB.Tnha, BGbiomass_Mg_ha_month = BGB.Tnha.m) %>% 
  mutate(Fire = plyr::mapvalues(Fire, c("B", "U"), c("Burned", "Unburned"))) %>% 
  mutate(Fence = plyr::mapvalues(Fence, c("Uf", "F"), c("Unfenced", "Fenced")))


### RESPIRATION
respiration <- read_excel("traits/data/immasdata/All_matrix_2017-11-07.xlsx", sheet = "Resp_matrix")

respiration <- respiration %>% 
  rename(Site = SITE, Fire = FIRE, Respiration_MC_C_ha = Rs.Tnha) %>% 
  select(Site, Year, Month, Fire, Fence, Treatment, Transect, Plot, Reading, Respiration_MC_C_ha) %>% 
  mutate(Fire = plyr::mapvalues(Fire, c("B", "U"), c("Burned", "Unburned"))) %>% 
  mutate(Fence = plyr::mapvalues(Fence, c("Uf", "F"), c("Unfenced", "Fenced")))


### NPP
npp <- read_excel("traits/data/immasdata/All_matrix_2017-11-07.xlsx", sheet = "NPP_matrix")

npp <- npp %>% 
  rename(AGP_Tn_ha = AGP, BGP_Tn_ha = BGP, NPP_Tn_ha = NPP) %>% 
  select(Year, Month, Treatment, Transect, AGP_Tn_ha, AGP.se, BGP_Tn_ha, BGP.se, NPP_Tn_ha, Npp.se) 
