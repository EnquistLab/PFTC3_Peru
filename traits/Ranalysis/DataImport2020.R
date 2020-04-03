# LOAD LIBRARIES
library("readxl")
library("writexl")
library("tidyverse")
library("tidylog")
library("lubridate")
library("tpl")
library("PFTCFunctions")

pn <- . %>% print(n = Inf)

source("traits/Rdatagathering/TaxonCorrections.R")

#****************************************************************************

# get valid IDs
all_codes <- get_PFTC_envelope_codes(seed = 6)

#****************************************************************************
#### LEAF AREA ####

# Import raw data files
LeafArea.raw <- read_csv(file = "traits/data/2020/RawLeafArea/LeafArea.raw_20-03-12.csv") %>% 
  bind_rows(read_csv(file = "traits/data/2020/RawLeafArea/LeafArea.raw_20-03-13.csv")) %>% 
  bind_rows(read_csv(file = "traits/data/2020/RawLeafArea/LeafArea.raw_20-03-14.csv")) %>% 
  bind_rows(read_csv(file = "traits/data/2020/RawLeafArea/LeafArea.raw_20-03-15.csv")) %>% 
  bind_rows(read_csv(file = "traits/data/2020/RawLeafArea/LeafArea.raw_exodus.csv"))


#LeafArea2020
LeafArea <- LeafArea.raw %>% 
  mutate(ID = substr(ID, 1, 7)) %>% 
  mutate(ID = recode(ID, "SEAN1.j" = "SEAN1"))
  
# Check wrong IDs
#setdiff(LeafArea$ID, all_codes$hashcode)
# 2 problematic IDs: "out.jpe", "2020-03"
# "SEAN1.j" exists in trait data

LeafArea <- LeafArea %>% 
  filter(!c(ID == "BVL6152" & LeafArea %in% c(0.22, 0.005, 0.006)),
         !c(ID == "BWJ5879" & LeafArea %in% c(0.159, 0.006, 0.006)),
         !c(ID == "BXQ0542" & LeafArea %in% c(0.02)),
         !c(ID == "BXU2822" & LeafArea %in% c(0.029)),
         !c(ID == "CXC6033" & LeafArea %in% c(86.3, 0.023)),
         !c(ID == "CUM5583" & LeafArea %in% c(0.105)),
         !c(ID == "BVP4602" & LeafArea %in% c(0.033))) %>% 
  group_by(ID) %>% 
  summarise(LeafArea_cm2 = sum(LeafArea),
            nLeafScan = n()) %>% 
  distinct(ID, LeafArea_cm2, nLeafScan) # no duplicates!

# check
#LeafArea %>% filter(ID == "BNN1844")

  
#****************************************************************************
#### LEAF TRAITS ####

#### load leaf trait data ####
# you might have to adapt path
files <- dir(path = "traits/data/2020/RawTraitData/", pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
traits.raw <- files[grepl("^(?!~)", basename(files), perl = TRUE)] %>% 
  set_names(basename(.)) %>% 
  map_df(read_excel, col_types = c("text", "numeric", "text", "numeric", rep("text", 4), rep("numeric", 11), "text"), .id = "file")
files <- dir(path = "traits/data/2020", pattern = "\\.xlsx$", full.names = TRUE)
traits.raw2 <- files[grepl("^(?!~)", basename(files), perl = TRUE)] %>% 
  set_names(basename(.)) %>% 
  map_df(read_excel, col_types = c("text", "numeric", "text", "numeric", rep("text", 4), rep("numeric", 11), rep("text", 3)), .id = "file")

traits.raw <- traits.raw %>% 
  bind_rows(traits.raw2)

# Load corrections
corrections <- read_excel(path = "traits/data/PFTC5-DataCleaning.xlsx")
leafScanProblems <- read_excel(path = "traits/data/LeafScanProblems.xlsx")

# Source checking file
source("traits/Rdatagathering/CheckSpreadsheet.R")

# Check spreadsheet
CheckSpreadsheet(traits.raw)
CheckSpreadsheet(traits)


# Clean trait data
traits <- traits.raw %>% 
  ### FIX WROND LEAF ID's
  mutate(ID = gsub("ail6011", "AIL6011", ID),
         ID = gsub("ADZ1845", "ADY1845", ID),
         ID = gsub("CZQ3610", "CYQ3610", ID),
         ID = gsub("CZU0738", "CYU0738", ID),
         ID = gsub("CBN2433", "CBM2433", ID),
         ID = gsub("CGY2493", "CGU2493", ID),
         ID = gsub("BLF6180", "BLE6180", ID),
         ID = gsub("CQT3635", "CQT3656", ID),
         ID = gsub("ADY3452", "ADZ3452", ID)) %>% 
  
  #Remove duplicate TRE-C-5-Lachemilla orbiculata
  filter(!ID %in% c("APS3068", "APO8208", "CNV5505", "CWL1543"),
         #Remove duplicate ACJ-BB-2-Calamagrostis tarmensis
         !ID %in% c("ARJ0812", "ARF3252", "ASL0566"),
         # Drop shaky cases for now
         !ID %in% c("AAF7186", "AHH9987", "AFS6587", "COI1685", "CMR2436", "CMU0940")) %>% 
  
  mutate(Site = recode(Site, "ACA" = "ACJ")) %>% 
  
  mutate(Project = if_else(Project %in% c("Trait", "trait"), "T", Project),
         Project = if_else(Project %in% c("Sean", "sean"), "S", Project),
         Project = if_else(Project == "R", "T", Project)) %>% 
  
  mutate(Experiment = recode(Experiment, "c" = "C", "b" = "BB", "bb" = "BB", "B" = "BB", "off-plot" = NA_character_),
         #QUE BB - plot1 Werneria villosa
         Experiment = if_else(ID == "AIR7210", "BB", Experiment)) %>% 
  
  mutate(Leaf_Thickness_2_mm = if_else(Leaf_Thickness_2_mm == -0.545, 0.545, Leaf_Thickness_2_mm)) %>% 
  
  ### fix missing data
  # Site
  mutate(Site = if_else(ID == "BEA0992", "ACJ", Site)) %>% 
  
  # Plot_ID
  left_join(corrections, by = "ID") %>% 
  mutate(Plot_ID = if_else(!is.na(Plot_ID_New), Plot_ID_New, Plot_ID)) %>%
  
  # Individual_nr
  mutate(Individual_nr = if_else(!is.na(Individual_nr_New), Individual_nr_New, Individual_nr)) %>%
  
  # Leaf_nr
  mutate(Leaf_nr = if_else(!is.na(Leaf_nr_New), Leaf_nr_New, Leaf_nr)) %>%

  # Fix height and length
  mutate(Plant_Height_cm = if_else(!is.na(Plant_Height_cm_New), Plant_Height_cm_New, Plant_Height_cm)) %>%
  mutate(Plant_Length_cm = if_else(!is.na(Plant_Length_cm_New), Plant_Length_cm_New, Plant_Length_cm)) %>%
  
  # Fix Genus and species names
  mutate(Genus = plyr::mapvalues(Genus, from = GenusDictionary2020$wrong, to = GenusDictionary2020$right, warn_missing = FALSE)) %>%
  
  mutate(Species = tolower(Species)) %>% 
  mutate(Species = plyr::mapvalues(Species, from = SpeciesDictionary2020$wrong, to = SpeciesDictionary2020$right, warn_missing = FALSE)) %>%
  mutate(Species = if_else(Genus == "Carex" & Species == "bonplandianum", "bonplandii", Species),
         Species = if_else(Genus == "Lachemilla" & Species == "umbellata", "orbiculata", Species)) %>% 
  mutate(Genus = if_else(Genus == "Calamagrostis" & Species == "7", "Anatherostipa", Genus),
         Species = if_else(Genus == "Anatherostipa" & Species == "7", "hans-meyeri", Species),
         Species = if_else(Genus == "Calamagrostis" & Species == "macrochaeta", "cf. macrophylla", Species),
         Species = if_else(Genus == "Vaccinium" & Species == "bonplandianum", "floribundum", Species),
         #TRE C Plot5 - lachemilla
         Species = if_else(ID %in% c("APS3068", "APO8208", "CNV5505", "CWL1543"), "vulcanica", Species)) %>% 
  
  # Fix Plant_Length_cm, Bulk_nr_leaves, Length_cm
  # move length to right column
  mutate(Length_cm = if_else(!is.na(Plant_Length_cm) & Genus %in% c("Hypericum", "Lycopodium"), Plant_Length_cm, Length_cm)) %>% 
  # replace length with NA
  mutate(Plant_Length_cm = ifelse(!is.na(Plant_Length_cm) & Genus %in% c("Halenia", "Oxalis", "Viola", "Vaccinium", "Hypericum", "Lycopodium", "Lachemilla", "Oreomyrhys", "Hypericum", "Lycopodium"), NA_real_, Plant_Length_cm)) %>% 
  # move length to plant length for grasses
  mutate(Plant_Length_cm = if_else(!is.na(Length_cm) & Genus %in% c("Carex", "Paspalum"), Length_cm, Plant_Length_cm)) %>% 
  # remove length for vaccinium
  mutate(Length_cm = if_else(!is.na(Length_cm) & Genus %in% c("Vaccinium", "Carex", "Paspalum"), NA_real_, Length_cm)) %>% 
  
  mutate(Taxon = paste(Genus, Species, sep = "_")) %>% 

  # remove duplicates
  distinct(ID, Day, Site, Taxon, Genus, Species, Project, Experiment, Plot_ID, Individual_nr, Leaf_nr, Plant_Height_cm, Plant_Length_cm, Bulk_nr_leaves, Length_cm, Wet_Mass_g, Leaf_Thickness_1_mm, Leaf_Thickness_2_mm, Leaf_Thickness_3_mm, Remark) %>% 
  
  # Join LeafArea and Traits
  left_join(LeafArea, by = "ID") %>% 
  
  # Remove some special duplicates
  group_by(ID) %>% 
  mutate(n = 1:n()) %>% 
  filter(if(ID %in% c("AAM1673", "AFA0140", "CYE9151", "ABL1039")) {
    n == 2
  } else {
    n == 1
  }) %>%
  select(-n) %>% 

  # Nr of leaves
  left_join(leafScanProblems, by = "ID") %>% 
  mutate(Bulk_nr_leaves = if_else(NrLeaves == "multiple", nLeafScan, Bulk_nr_leaves)) %>% 
  #mutate(Bulk_nr_leaves = coalesce(Bulk_nr_leaves, as.numeric(nLeafScan)))
  # Bulk_nr_leaves: NA for Baccharis, Hypericum, Lycopodium
  mutate(Bulk_nr_leaves = if_else(Genus %in% c("Baccharis", "Hypericum", "Lycopodium"), NA_real_, Bulk_nr_leaves))


  

# Check scans
#Changing bulk number of leaves for TRE C Plot5 - Lachemilla orbiculata
# bulk number of leaves = NA
# CFG0732
# CFK2286
# BKP5877
# AYF7496
# CEE0397
# AYG1241
# CXA8168
# AYI4751
# CEA4386
# CZN4729
# CZJ5895
# CZE7072
# CZF0686
# CZA2309

  
#write_csv(traits_cleaned, path = "traits/PFTC5_Peru_2020_LeafTraits_cleaned_20-03-22.csv")


# TODO!!!
### Trait people
# BMP6395: Check community data, which plot has Carex pinchinchensis; traits %>% filter(is.na(Plot_ID) & Project == "T") %>% arrange(Genus) %>% as.data.frame()


# Unresolved bussines
# "AAF7186": could be ACJ-C-Plot1-ind5, Check envelope. drop for now
# "AHH9987", "AFS6587": Could be TRE-C-Plot1-ind1-leafnr3 and 4, but height is wrong, check envelope, drop for now
# "COI1685": Could be TRE-C-4-1, but date and site is wrong. Check envelope. drop for now
# "CMU0940", "CMR2436": could be TRE-C-1-1, but wrong date and site. Check envelope. drop for now