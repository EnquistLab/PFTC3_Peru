### MERGE PUNA AND PFTC DATA AND CORRECT SP NAMES


# LOAD LIBRARIES
library("readxl")
library("writexl")
library("tidyverse")
library("tidylog")
library("lubridate")
library("tpl")
library("googledrive")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

pn <- . %>% print(n = Inf)

#### COMMUNITY ####
# Download files from google drive
drive_download("PU.1_Community_Dataset", path = "community/PU.1_Community_Dataset.xlsx")
get_file(node = "7mzjk",
         file = "PFTC3.1_CommunityCover_2018_Peru.csv",
         path = "data_cleaned",
         remote_path = "Peru")


# Load community files
commPU.raw <- read_excel(path = "community/PU.1_Community_Dataset_2018_2019.xlsx")
commPFTC.raw <- read_csv(file = "community/PFTC3.1_CommunityCover_2018_Peru.csv")

# load data dictioniary
dicitionary <- read_excel(path = "community/Real_sp_names.xlsx") %>% 
  fill(family) %>% 
  rename("Family" = "family")

# download and read in coords
drive_download("PU.10_PFTC3.10_2020_Peru_Coordinates.xlsx", path = "traits/data/PU.10_PFTC3.10_2020_Peru_Coordinates.xlsx")
coordinates_Peru_2020 <- read_excel("traits/data/PU.10_PFTC3.10_2020_Peru_Coordinates.xlsx") %>% 
  mutate(PlotID = as.character(PlotID))

commPU.raw %>% 
  rename("PlotID" = "plot", "Treatment" = "treatment", "Site" = "site", "Year" = "year", "Taxon" = "name", "FunctionalGroup" = "functional_group") %>% 
  distinct() %>% # 2 duplicate rows removed 
  mutate(Campaign = "Puna") %>% 
  select(-latitude, -longitude, -elevation) %>% 
  left_join(coordinates_Peru_2020, by = c("Site", "Treatment", "PlotID"))
  #anti_join(dicitionary, by = c("Taxon" = "name", "real_species_name" = "real_species _name"))
  # left_join(dicitionary, by = c("Taxon" = "name", "real_species_name" = "real_species _name")) %>% 
  
commPFTC.raw %>% 
  left_join(coordinates_Peru_2020, by = c("Site", "Treatment", "PlotID")) %>% 
  anti_join(dicitionary, by = c("Taxon" = "name", "Family"))
  ### PROBLEM 239 species do not match!!!
  #left_join(dicitionary, by = c("Taxon" = "name", "Family)) %>% 
  # mutate(Campaign = "PFTC")

# Merge PFTC and Puna data sets
comm <- commPFTC %>% 
  bind_rows(commPU)


#### TRAITS ####
drive_download("PU.7_Traits_Dataset.xlsx", path = "traits/PU.7_Traits_Dataset.xlsx")
get_file(node = "7mzjk",
         file = "PFTC3.7_Traits_2018_Peru_cleaned.csv",
         path = "data_cleaned",
         remote_path = "Peru")

# Load files to R
traitsPU.raw <- read_excel(path = "traits/data/PU.7_Traits_Dataset_2019.xlsx")
traitsPFTC.raw <- read_csv(file = "traits/data/PFTC3.7_Traits_2018_Peru_cleaned.csv")


traitsPU.raw %>% 
  select(-latitude,  -...10, -elevation) %>% 
  rename("Year" = "year", "Project" = "project", "PlotID" = "plot_id", "Treatment" = "treatment", "Site" = "site", "Taxon" = "taxon", "Plant_Height_cm" = "plant_height_cm", "Individual_nr" = "individual_nr", "NrLeaves" = "nr_leaves", "Bulk" = "bulk", "Leaf_Area_Total_cm2" = "leaf_area.y", "Wet_Mass_Total_g" = "wet_mass_total_g", "Dry_Mass_Total_g" = "dry_mass", "Leaf_Thickness_1_mm" = "leaf_thickness_1_mm", "Leaf_Thickness_2_mm" = "leaf_thickness_2_mm", "Leaf_Thickness_3_mm" = "leaf_thickness_3_mm", "comment" = "commet") %>% 
  mutate(Leaf_Area_Total_cm2 = as.numeric(if_else(Leaf_Area_Total_cm2 == "whithouth_scan", NA_character_, Leaf_Area_Total_cm2))) %>% 
  left_join(coordinates_Peru_2020, by = c("Site", "Treatment", "PlotID")) %>% 
  mutate(Country = "PE",
         Campaign = "Puna")

  ### CALCULATE LEAF FUNCTIONAL TRAITS
  # SLA, LDMC
  # Species with leaf number = > Leaf nr = 1 because of calculations below
  #mutate(NrLeaves = ifelse(Genus %in% c("Baccharis", "Lycopodiella", "Lycopodium"), 1, NrLeaves)) %>%
  # Bulk samples contain several leaves. We want mass and area on the leaf level
  mutate(Wet_Mass_g = Wet_Mass_Total_g / NrLeaves,
         Dry_Mass_g = Dry_Mass_Total_g / NrLeaves,
         Leaf_Area_cm2 = Leaf_Area_Total_cm2 / NrLeaves) %>%
  # Special cases
  # Sisyrinchium: leaves are folded: area needs to be doubled and leaf thickness halfed
  # mutate(Leaf_Area_cm2 = ifelse(Genus == "Sisyrinchium", Leaf_Area_cm2 * 2, Leaf_Area_cm2),
  #        Leaf_Thickness_1_mm = ifelse(Genus == "Sisyrinchium", Leaf_Thickness_1_mm / 2, Leaf_Thickness_1_mm),
  #        Leaf_Thickness_2_mm = ifelse(Genus == "Sisyrinchium", Leaf_Thickness_2_mm / 2, Leaf_Thickness_2_mm),
  #        Leaf_Thickness_3_mm = ifelse(Genus == "Sisyrinchium", Leaf_Thickness_3_mm / 2, Leaf_Thickness_3_mm)) %>% 
  # Leaf thickness
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE))

# fix species names
# divide wetmass and drymass by nr leaves


traitsPFTC.raw %>% 
  mutate(Campaign = "PFTC")


# Merge PFTC and Puna data sets
traits <- traitsPFTC %>% 
  bind_rows(traitsPU)

