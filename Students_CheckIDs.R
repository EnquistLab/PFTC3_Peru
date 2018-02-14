####################################
#### CODE TO CHECK SPREADSHEETS ####
####################################

#### LOAD LIBRARIES
library("tidyverse")
library("lubridate")
library("readxl")
library("assertr")
library("gridExtra")

# Source Data list and function
load("traits/Rdatagathering/envelope_codes.Rdata")


# Read in spreadsheet
### CHANGE PATH TO THE SPREADSHEET !!!
traits <- read_excel(path = "traits/data/TraitSpreadsheet_Template.xlsx", col_names = TRUE)


# Check spreadsheet
CheckSpreadsheet(dat = traits)


# Draw some plots
MakeSomePlots(traits)