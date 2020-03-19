# LOAD LIBRARIES
library("readxl")
library("writexl")
library("tidyverse")
library("tidylog")
library("lubridate")
library("tpl")

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
  bind_rows(read_csv(file = "traits/data/2020/RawLeafArea/LeafArea.raw_20-03-15.csv"))


#LeafArea2020
LeafArea <- LeafArea.raw %>% 
  mutate(ID = substr(ID, 1, 7))
  
# Check wrong IDs
setdiff(LeafArea$ID, all_codes$hashcode)
# 2 problematic IDs: "out.jpe" "2020-03"

LeafArea <- LeafArea %>% 
  group_by(ID) %>% 
  summarise(LeafArea_cm2 = sum(LeafArea)) %>% 
  distinct(ID, LeafArea_cm2) # no duplicates!

# check
#LeafArea %>% filter(ID == "BNN1844")

  
#****************************************************************************
#### LEAF TRAITS ####

#### load leaf trait data ####
# you might have to adapt path
files <- dir(path = "traits/data/2020/", pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
traits.raw <- files[grepl("^(?!~)", basename(files), perl = TRUE)] %>% 
  set_names(basename(.)) %>% 
  map_df(read_excel, col_types = c("text", "numeric", "text", "numeric", rep("text", 4), rep("numeric", 11), "text"), .id = "file")


# Source checking file
source("traits/Rdatagathering/CheckSpreadsheet.R")

# Check spreadsheet
CheckSpreadsheet(traits.raw)


# Clean trait data
traits <- traits.raw %>% 
  ### FIX WROND LEAF ID's
  mutate(ID = gsub("ail6011", "AIL6011", ID),
         ID = gsub("ADZ1845", "ADY1845", ID),
         ID = gsub("CZQ3610", "CYQ3610", ID),
         ID = gsub("ADY3452", "ADZ3452", ID)) %>% 
  mutate(Project = if_else(Project %in% c("Trait", "trait"), "T", Project),
         Project = if_else(Project %in% c("Sean", "sean"), "S", Project)) %>% 
  mutate(Experiment = if_else(Experiment %in% c("c"), "C", Experiment),
         Experiment = if_else(Experiment %in% c("b"), "B", Experiment),
         Experiment = if_else(Experiment %in% c("bb"), "BB", Experiment)) %>% 
  mutate(Individual_nr = if_else(ID %in% c("CPQ6887", "CPM4170"), 2, Individual_nr),
         Leaf_nr = if_else(ID %in% c("CPQ6887"), 3, Leaf_nr),
         Leaf_nr = if_else(ID %in% c("CPM4170"), 4, Leaf_nr)) %>% 

  ### remove duplicate plot/ind/leaf nr
  filter(!(ID == "AAH3401" & !is.na(Plant_Length_cm )),
         !(ID == "ATJ5219" & !is.na(Plant_Length_cm == 10.5)),
         !(ID == "ATX9292" & !is.na(Plant_Length_cm == 10.5)),
         !(ID == "ABL1039" & Plant_Height_cm == 11.0),
         !(ID == "AAM1673" & is.na(Leaf_nr)),
         !(ID == "ABD7604" & is.na(Plant_Height_cm)),
         !(ID == "ABE9156" & is.na(Plant_Height_cm)),
         !(ID == "ACD5747" & is.na(Plant_Height_cm)),
         !(ID == "ABH3333" & is.na(Plant_Height_cm)),
         !(ID == "ATS6341" & !is.na(Remark)),
         !(ID == "AAI8954" & Wet_Mass_g == 0.066),
         !(ID == "AEV1251" & !is.na(Remark)),
         !(ID == "AUB2849" & is.na(Plot_ID)),
         !(ID == "ADX3747" & is.na(Plot_ID)),
         # remove duplicate Ind for Halenia-TRE-C-PlotID 4; Ind 1 height = 10, Ind 2 height = 8, remove other leaves
         !ID %in% c("AYN2714", "ATY7432", "CEY7459", "AQF9992", "AQL4649", "AQK5616", "AQJ2521", "AQM0288"),
         # remove all duplicate Lachemilla-TRE-C-PlotID-1-5
         !ID %in% c("CEL6663", "CEP4593", "APU4440", "CWY6193", "CWM3970", "CXK6630", "AQE5913", "CXE6426", "CXA8168", "APS3068", "APO8208", "CNV5505", "CWL1543", "CPJ4884", "CXC6033", "CPB8831", "CPZ4857", "CQD3030"),
         # remove duplicates Lachemialla-ACJ-C-Plot_ID-1-5
         !ID %in% c("AAF7186", "AHP7215"),
         !(ID == "AEE2202" & is.na(Plant_Height_cm))
         ) %>%
  
  ### fix missing data
  # Site
  mutate(Site = if_else(ID == "BEA0992", "ACJ", Site)) %>% 
  # Plot_ID
  mutate(Plot_ID = if_else(ID %in% c("CWN1073", "CWR0183", "CWZ4760", "CWV6534"), 1, Plot_ID),
         Plot_ID = if_else(ID == "AUB2849", 4, Plot_ID)) %>% 
  # Individual_nr
  mutate(Individual_nr = if_else(ID %in% c("CNP7038", "CNX3247", "COB5911"), 2, Individual_nr),
         Individual_nr = if_else(ID == "AUB2849", 3, Individual_nr)) %>% 
  # Leaf_nr
  mutate(
    Leaf_nr = if_else(ID == "ADY1845", 1, Leaf_nr),
    Leaf_nr = if_else(ID == "ATX9292", 2, Leaf_nr),
    Leaf_nr = if_else(ID == "ATJ5219", 3, Leaf_nr),
    Leaf_nr = if_else(ID == "AAH3401", 4, Leaf_nr),
    Leaf_nr = if_else(ID == "ADF3723", 5, Leaf_nr),
    Leaf_nr = if_else(ID == "ABL1039", 5, Leaf_nr),
    Leaf_nr = if_else(ID == "AUB2849", 1, Leaf_nr),
    Leaf_nr = if_else(ID == "CNA9373", 6, Leaf_nr),
    Leaf_nr = if_else(ID == "AUB2849", 4, Leaf_nr)
    ) %>% 
  
  # Fix height
  mutate(Plant_Height_cm = if_else(ID == "CEX5438", 2.6, Plant_Height_cm),
         Plant_Height_cm = if_else(ID == "CEA4386", 8.9, Plant_Height_cm),
         Plant_Height_cm = if_else(ID == "AUG4202", 6.0, Plant_Height_cm),
         Plant_Height_cm = if_else(ID == "ADP7756", 10.8, Plant_Height_cm),
         Plant_Height_cm = if_else(ID == "ACF7763", 9.6, Plant_Height_cm)) %>% 
  
  # Fix Genus and species names
  mutate(Genus = plyr::mapvalues(Genus, from = GenusDictionary2020$wrong, to = GenusDictionary2020$right, warn_missing = FALSE)) %>%
  mutate(Species = tolower(Species)) %>% 
  mutate(Species = plyr::mapvalues(Species, from = SpeciesDictionary2020$wrong, to = SpeciesDictionary2020$right, warn_missing = FALSE)) %>%
  mutate(Species = if_else(Genus == "Carex" & Species == "bonplandianum", "bonplandii", Species),
         Species = if_else(Genus == "Lachemilla" & Species == "umbellata", "orbiculata", Species)) %>% 
  # remove duplicates
  distinct(ID, Day, Site, Genus, Species, Project, Experiment, Plot_ID, Individual_nr, Leaf_nr, Plant_Height_cm, Plant_Length_cm, Bulk_nr_leaves, Length_cm, Wet_Mass_g, Leaf_Thickness_1_mm, Leaf_Thickness_2_mm, Leaf_Thickness_3_mm, Remark)
  
CheckSpreadsheet(traits)


### Join LeafArea and Traits

traits_cleaned <- traits %>% 
  left_join(LeafArea, by = "ID")

write_csv(traits_cleaned, path = "traits/PFTC5_Peru_2020_LeafTraits_cleaned_20-03-19.csv")

# missing Plot_ID
traits %>% filter(is.na(Plot_ID) & Project == "T") %>% arrange(Genus) %>% as.data.frame()

traits %>% filter(Site == "ACJ", Genus == "Halenia", Experiment == "C", Plot_ID == 4) %>% arrange(Plot_ID, Individual_nr, Leaf_nr) %>% as.data.frame()



# TODO!!!
# Check envelopes:
# BEA0992: check if site is ACJ
# AUB2849: check if Ind. nr is 4, could also be plot 4, because there ind 1 and 3 are missing leaf 4. Probably not easy to find if info is missing on envelope.
# AAI8954: check if wet mass is 0.065 or 0.066. And remove the one that is wrong. At the moment I have removed 0.066

### Trait people
# Check genus names: Oreomirhys and Oreomyrhys
# Check genus names: Oriotrophium and Orithrophium
# Jasmesonia aistonia and Jasmesonia alstonia
# BMP6395: Check community data, which plot has Carex pinchinchensis; traits %>% filter(is.na(Plot_ID) & Project == "T") %>% arrange(Genus) %>% as.data.frame()

