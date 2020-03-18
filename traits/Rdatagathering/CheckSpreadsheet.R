### CHECK DATA IN SPREADSHEET 

# Load libraries
library("tidyverse")
library("lubridate")
library("readxl")
library("assertr")
library("gridExtra")
library("PFTCFunctions")
library("patchwork")


# get valid IDs
all_codes <- get_PFTC_envelope_codes(seed = 6)

# # Read in several tables (might need to adjust this code)
# files <- dir(path = "traits/raw_trait_data/", pattern = "\\.xlsx$", full.names = TRUE)
# traits.raw <- files[grepl("^(?!~)", basename(files), perl = TRUE)] %>% 
#   set_names(basename(.)) %>% 
#   map_df(read_excel, col_types = c("text", "numeric", "text", "numeric", rep("text", 4), rep("numeric", 9), "text"), .id = "file")


# Data lists
nr.col <- 21
ID.list <- all_codes$hashcode # MAKE AN OPTION TO SLECT CERTAIN ROWS
day.list <- c(12, 13, 14, 15, 16, 17)
character.list <- c("file", "ID", "Site", "Genus", "Species", "Project", "Experiment", "Remark")
numeric.list <- c("Day", "Elevation", "Plot_ID", "Individual_nr", "Leaf_nr", "Plant_Height_cm", "Plant_Length_cm", "Bulk_nr_leaves", "Length_cm", "Wet_Mass_g", "Leaf_Thickness_1_mm", "Leaf_Thickness_2_mm", "Leaf_Thickness_3_mm")
site.list <- c("WAY", "ACJ", "PIL", "TRE", "QUE")
elevation.list <- c(3101, 3468, 3676, 3715, 3888)
genus.list <- c()
species.list <- c()
project.list <- c("T", "S", "Drone")
experiment.list <- c("C", "B", "BB")
plot.list <- c(1:5)
individual.list <- c(1:5)

# set bounds for WetMass, DryMass, etc
wet.mass.treshold <- 0.06
sla.upper <- 500
sla.lower <- 5
ldmc.upper <- 1



#### Function to test trait data ####
CheckSpreadsheet <- function(dat){

  out <- dat %>%
    filter(!is.na(ID)) %>% # remove rows with no ID
    verify(ncol(.) == nr.col, error_fun = error_report) %>% # check number of columns
    assert(function(x) nchar(x)==7, ID, error_fun = error_report) %>% # check length of ID
    assert(in_set(ID.list), ID, error_fun = error_report) %>% # check if ID is valid
    assert(is.character, character.list, error_fun = error_report) %>%  # is a character
    assert(is.numeric, numeric.list, error_fun = error_report) %>% # is numeric
    
    # Check if Variables only contain elements from lists defined above
    assert(in_set(day.list), Day, error_fun = error_report) %>% 
    assert(in_set(site.list), Site, error_fun = error_report) %>%
    #assert(in_set(genus.list), Genus, error_fun = error_report) %>% 
    #assert(in_set(species.list), Species, error_fun = error_report) %>% 
    assert(in_set(elevation.list), Elevation, error_fun = error_report) %>% 
    assert(in_set(project.list), Project, error_fun = error_report) %>% 
    assert(in_set(experiment.list), Experiment, error_fun = error_report) %>%
    assert(in_set(plot.list), Plot_ID, error_fun = error_report) %>% 
    assert(in_set(individual.list), Individual_nr, error_fun = error_report) %>%
    
    # check values
    assert(within_bounds(0, Inf), Wet_Mass_g, error_fun = error_report) %>%
    #assert(within_bounds(0, Inf), Dry_Mass_g, error_fun = error_report) %>%
    #assert(within_bounds(0, Inf), Leaf_Area_cm2, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Leaf_Thickness_1_mm, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Leaf_Thickness_2_mm, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Leaf_Thickness_3_mm, error_fun = error_report) %>%
    
    #verify(Wet_Mass_g > wet.mass.treshold, error_fun = error_report) %>% 
    #verify(Wet_Mass_g > Dry_Mass_g, error_fun = error_report) %>% 
    #verify(SLA_cm2_g < sla.upper, error_fun = error_report) %>% 
    #verify(SLA_cm2_g > sla.lower, error_fun = error_report) %>% 
    #verify(LDMC > ldmc.upper, error_fun = error_report) %>% 
  
  # Duplicates
    group_by(ID) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    assert(in_set(1), n, error_fun = error_report)
  
}



#### Function to plot some figures ####

MakeSomePlots <- function(dat){
  
  dat <- dat %>%
    mutate(WetSLA_cm2_g = Leaf_Area_cm2/Wet_Mass_g) 
    #mutate(LDMC = Dry_Mass_g/Wet_Mass_g)
  
  #if(all(is.na(dat$Dry_Mass_g))){
    
    # histogram of Wet_mass_g
    p1 <- ggplot(dat, aes(x = Wet_Mass_g)) + 
      geom_histogram() 
    
    # histgram of Leaf_Area_cm2
    p2 <- ggplot(dat, aes(x = Leaf_Area_cm2)) +
      geom_histogram() +
      geom_vline(xintercept = 1, color = "red")
    
  # } else{
  #   
  #   # wet vs dry mass
  #   p1 <- ggplot(dat, aes(x = Wet_Mass_g, y = Dry_Mass_g)) + 
  #     geom_point() +   
  #     geom_abline(intercept = 0, slope = 1, colour = "red") +
  #     scale_x_log10() + 
  #     scale_y_log10()
  #   
  #   # dry vs area
  #   p2 <- ggplot(dat, aes(x = Dry_Mass_g, y = Leaf_Area_cm2)) + 
  #     geom_point() +   
  #     geom_abline(intercept = 0, slope = 1, colour = "red") +
  #     scale_x_log10() + 
  #     scale_y_log10()
  # }
  
  p3 <- ggplot(dat, aes(x = Wet_Mass_g, y = Leaf_Area_cm2)) + 
      geom_point() +   
      geom_abline(intercept = 0, slope = 1, colour = "red") +
      scale_x_log10() + 
      scale_y_log10()
    
  p4 <- ggplot(dat, aes(x = WetSLA_cm2_g)) +
    geom_histogram()
  
  p5 <- ggplot(traits, aes(x = Leaf_Thickness_1_mm, y = Leaf_Thickness_2_mm)) + 
    geom_point() +   
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    scale_x_log10() + 
    scale_y_log10()
  
  p6 <- ggplot(traits, aes(x = Leaf_Thickness_1_mm, y = Leaf_Thickness_3_mm)) + 
    geom_point() +   
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    scale_x_log10() + 
    scale_y_log10()
  
  p7 <- ggplot(traits, aes(x = Leaf_Thickness_2_mm, y = Leaf_Thickness_3_mm)) + 
    geom_point() +   
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    scale_x_log10() + 
    scale_y_log10()
  
  #grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
  p1 & p2 & p3 & p4 | p5 & p6 & p7
  
}
