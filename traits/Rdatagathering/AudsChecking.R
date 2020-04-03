#### CHECK FILE NAMES ####

list.of.files.13 <- dir(path = paste0("/Volumes/PFT3/Peru_leaves/18-03-13 PIL"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
list.of.files.14 <- dir(path = paste0("/Volumes/PFT3/Peru_leaves/18-03-14 PIL"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

b13 <- basename(list.of.files.13)
b14 <- basename(list.of.files.14)
setdiff(b13, b14)


#### CHECK DATA ####

# Checking missing scans or data
traits %>% anti_join(LeafArea2018, by = "ID") %>% distinct(ID) %>% pn
LeafArea2018 %>% anti_join(traits, by = "ID") %>% distinct(ID) %>% pn

# Check if ID's are valid
# get all valid IDs
load("traits/Rdatagathering/envelope_codes.Rdata")
ID.list <- all_codes$hashcode
setdiff(LeafArea2018$ID, ID.list)
setdiff(traits$ID, ID.list)


# Find duplicate leaves
traits %>% 
  group_by(Project, Experiment, Site, Plot_ID, Taxon, Individual_nr, Leaf_nr) %>%
  mutate(n = n()) %>% 
  filter(n > 1)

# Find different plant height per individual
dd <- traits %>% 
  group_by(Project, Experiment, Site, Plot_ID, Taxon, Individual_nr) %>%
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  filter(max(Plant_Height_cm) != min(Plant_Height_cm)) %>% 
  arrange(Project, Site, Taxon, Experiment, Plot_ID, Individual_nr, Leaf_nr, Plant_Height_cm) %>% 
  select(ID, Project, Site, Taxon, Experiment, Plot_ID, Individual_nr, Leaf_nr, Plant_Height_cm, Plant_Length_cm) %>% as.data.frame()
#writexl::write_xlsx(dd, path = "traits/dd.xlsx")


#### Source to load libraries, data lists and functions ####
source("traits/Rdatagathering/CheckSpreadsheet.R")
CheckSpreadsheet(dat = traits)


# check values in the cells
unique(traits$Date)
unique(traits$Site)
unique(traits$Elevation)
table(traits$Site, traits$Elevation)
unique(traits$Genus)
unique(traits$Species)
traits %>% 
  mutate(Taxon = paste(Genus, Species, sep = " ")) %>% 
  arrange(Taxon) %>% distinct(Taxon) %>% pn
unique(traits$Experiment)
table(traits$Experiment, traits$Plot)
traits %>% filter(is.na(Experiment))
unique(traits$Plot)
traits %>% filter(is.na(Plot))
table(traits$Plot, traits$Site)
unique(traits$Individual_nr)
traits %>% filter(is.na(Individual_nr))
traits %>% filter(Site == "WAY", Genus == "Eriosorus", Experiment == "C", Plot == 2)


# Missing area or traits
traits %>% filter(is.na(Height_cm)) %>% as.data.frame()
traits %>% filter(is.na(Wet_mass_g)) %>% as.data.frame()
traits %>% filter(is.na(Dry_mass_g)) %>% as.data.frame()
traits %>% filter(is.na(Leaf_thickness_1_mm)) %>% as.data.frame()
traits %>% filter(is.na(Leaf_thickness_2_mm)) %>% as.data.frame()
traits %>% filter(is.na(Leaf_thickness_3_mm)) %>% as.data.frame()
traits %>% filter(is.na(Area_cm2)) %>% distinct(ID)

# Draw plots

traits %>% 
  ggplot(aes(x = Wet_mass_g, y = Area_cm2, color = Genus)) +
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() +
  facet_grid(~ Site) +
  theme(legend.position="none")

traits %>%  
  ggplot(aes(x = Height_cm, y = Wet_mass_g, color = Genus)) +
  geom_point() +
  scale_y_log10() +
  facet_grid(~ Site) +
  theme(legend.position="none")

traits %>% 
  ggplot(aes(x = Leaf_thickness_1_mm, y = Leaf_thickness_3_mm, color = Genus)) +
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() +
  facet_grid(~ Site) +
  theme(legend.position="none")




all.files <- dir(path = paste0("/Volumes/PFT3/Peru_leaves"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
length(all.files)
all.files <- all.files %>% as.tibble
all.files <- all.files %>% 
  mutate(ID = basename(value)) %>% 
  mutate(ID = substr(ID, 1, 7))

dim(traits.raw)
traits.raw %>% 
  anti_join(all.files, by = "ID")


setdiff(all.files$ID, ID.list)
setdiff(traits.raw$ID, ID.list)


# Compare species in trait and community data
traits_cleaned %>% 
  select(Site, Treatment, PlotID, Taxon) %>% 
  anti_join(CommunityCover_2018_Peru, by = c("Site", "Treatment", "PlotID", "Taxon")) %>% 
  distinct(Site, Treatment, Taxon) %>% 
  arrange(Site, Treatment, Taxon) %>% pn

CommunityCover_2018_Peru %>% 
  anti_join(traits_cleaned, by = c("Site", "Treatment", "PlotID", "Taxon")) %>% 
  filter(functionalGroup == "graminoid") %>% 
  filter(Cover < 2) %>%
  #distinct(Site, Treatment, Taxon) %>% 
  arrange(Site, Treatment, Taxon) %>% pn
  
  
