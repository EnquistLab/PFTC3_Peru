# LOAD LIBRARIES
library("readxl")
library("writexl")
library("tidyverse")
library("tidylog")
library("lubridate")
library("tpl")
library("googledrive")

pn <- . %>% print(n = Inf)

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
# one leaf scan is called out.jpe -> need to find which leaf it is. Find leaf entered with no scan!

LeafAre <- LeafArea %>% 
  group_by(ID) %>% 
  summarise(LeafArea = sum(LeafArea))



  
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
  mutate(Project = ifelse(Project %in% c("Trait", "trait"), "T", Project),
         Project = ifelse(Project %in% c("Sean", "sean"), "S", Project)) %>% 
  mutate(Experiment = ifelse(Experiment %in% c("c"), "C", Experiment),
         Experiment = ifelse(Experiment %in% c("b"), "B", Experiment),
         Experiment = ifelse(Experiment %in% c("bb"), "BB", Experiment)) %>% 
  mutate(Individual_nr = if_else(ID %in% c("CPQ6887", "CPM4170"), 2, Individual_nr),
         Leaf_nr = if_else(ID %in% c("CPQ6887"), 3, Leaf_nr),
         Leaf_nr = if_else(ID %in% c("CPM4170"), 4, Leaf_nr)) %>% 
  # remove duplicates
  distinct(ID, Day, Site, Elevation, Genus, Species, Project, Experiment, Plot_ID, Individual_nr, Leaf_nr, Plant_Height_cm, Plant_Length_cm, Bulk_nr_leaves, Length_cm, Wet_Mass_g, Leaf_Thickness_1_mm, Leaf_Thickness_2_mm, Leaf_Thickness_3_mm, Remark)
  
CheckSpreadsheet(traits)



