### CHECK DATA IN SPREADSHEET 

# Load libraries
library("tidyverse")
library("lubridate")
library("readxl")
library("assertr")

# get all valid IDs
source("traits/Rdatagathering/envelope_codes.R")
all_codes

# Read in several tables
#myfiles <- dir(path = paste0("traits/data"), pattern = "xlsx", recursive = TRUE, full.names = TRUE)
#mdat <- map_df(myfiles, function(n) read_excel(path = n, col_names = TRUE))


# Data lists
nr.col <- 18
ID.list <- all_codes$hashcode # MAKE AN OPTION TO SLECT CERTAIN ROWS
date.list <- ymd(c("2018-03-23"))
character.list <- c("Site", "Genus", "Species", "Project", "Experiment")
numeric.list <- c("Elevation", "Plot", "Individual_nr", "Leaf_nr", "LeafArea", "WetMass", "DryMass", "Thickness1", "Thickness2", "Thickness3")
site.list <- c("WAY", "AJA", "PIL", "TRE")
elevation.list <- c(3085, 3450, 3670, 3900)
genus.list <- c()
species.list <- c()
project.list <- c("Local", "Experiment", "Leaf_T")
experiment.list <- c("None", "Burnt", "Unburnt", "Grazed", "Ungrazed")
plot.list <- c(1:5)
individual.list <- c(1:5)
leaf.list <- c(1:5)

# set bounds for WetMass, DryMass, etc




# Function to test trait data
CheckSpreadsheet <- function(dat){

  out <- dat %>%
    filter(!is.na(ID)) %>% 
    mutate(Date = as.Date(Date)) %>% 
    verify(ncol(.) == nr.col, error_fun = error_report) %>% # check number of columns
    assert(function(x) nchar(x)==7, ID, error_fun = error_report) %>% # check length of ID
    assert(in_set(ID.list), ID, error_fun = error_report) %>% # check if ID is valid
    assert(is.character, character.list, error_fun = error_report) %>%  # is a character
    assert(is.numeric, numeric.list, error_fun = error_report) %>% # is numeric
    # Check if Variables only contain elements from lists defined above
    assert(in_set(date.list), Date, error_fun = error_report) %>% 
    assert(in_set(site.list), Site, error_fun = error_report) %>%
    #assert(in_set(genus.list), Genus, error_fun = error_report) %>% 
    #assert(in_set(species.list), Species, error_fun = error_report) %>% 
    assert(in_set(elevation.list), Elevation, error_fun = error_report) %>% 
    assert(in_set(project.list), Project, error_fun = error_report) %>% 
    assert(in_set(experiment.list), Experiment, error_fun = error_report) %>%
    assert(in_set(plot.list), Plot, error_fun = error_report) %>% 
    assert(in_set(individual.list), Individual_nr, error_fun = error_report) %>%
    assert(in_set(leaf.list), Leaf_nr, error_fun = error_report) %>% 
    
    # check values
    verify(WetMass > DryMass, error_fun = error_report) %>% 
    assert(within_bounds(0, Inf), WetMass, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), DryMass, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), LeafArea, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Thickness1, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Thickness2, error_fun = error_report) %>%
    assert(within_bounds(0, Inf), Thickness3, error_fun = error_report) %>% 
  
  # Observation unique
    group_by(ID) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    assert(in_set(1), n, error_fun = error_report)
  
}


