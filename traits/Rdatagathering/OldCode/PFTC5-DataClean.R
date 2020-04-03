#loading libraries
library(readxl)
library(dplyr)

#loading excel File with the traits
# You will need to set your own file path
raw_data <- read_excel("C:/Users/Tasha-Leigh/Dropbox/PhD/Courses/PFTC 5 - Peru/PFTC5-Intraspecific/PFTC5_Peru_2020_LeafTraits_cleaned_20-03-22.xlsx", 
                       col_types = c("text", "numeric", "text", 
                                     "text", "text", "text", "text", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "text", "numeric", "numeric"))

######### Data cleaning and validation #########
#get rid of duplicates and triplicates entries based on the ID column
#raw_data <-raw_data[-which(duplicated(raw_data[,1])),]

raw_data <- raw_data %>% filter(
  #Remove duplicate ACJ-BB-2-Calamagrostis tarmensis
  #!(ID == "ARJ0812"), !(ID == "ARF3252"), !(ID == "ASL0566"),
  #Remove duplicate TRE-C-5-Lachemilla orbiculata
  #!(ID == "APS3068"),!(ID == "APO8208"),!(ID == "CWL1543"),!(ID == "CNV5505"),
  #Remove questionable IDs
  #!(ID == "COI1685"),!(ID == "CMR2436"),!(ID == "AAF7186"),!(ID == "AHH9987"),!(ID == "AFS6587"),
  #!(ID == "CMU0940"),!(ID == "CRN1464"))

#combine genus and species into one column
#raw_data$genus_species <- paste (raw_data$Genus,raw_data$Species, sep = " ")

#verifying that there is only one leaf for each leaf#/individual#/species/plot/site 
leaf_count <- raw_data %>% 
  group_by(Project, Experiment, Site, Plot_ID, genus_species, Individual_nr, Leaf_nr) %>%
  summarise(n())

#need to rename vaccinium bonplandianum as vaccinium floribundum
#raw_data$Species[raw_data$Genus == "Vaccinium" & raw_data$Species == "bonplandianum"] <- "floribundum"

#QUE BB - plot1 Werneria villosa
#raw_data$Experiment [raw_data$ID == "AIR7210"] <- "BB"

#fixing incorrect plant height
#raw_data$Plant_Height_cm [raw_data$ID == "CJN4110"] <- 13.0

#TRE C Plot5 - lachemilla
# raw_data$Species[raw_data$ID == "APS3068"] <- "vulcanica"
# raw_data$Species[raw_data$ID == "APO8208"] <- "vulcanica"
# raw_data$Species[raw_data$ID == "CNV5505"] <- "vulcanica"
# raw_data$Species[raw_data$ID == "CWL1543"] <- "vulcanica"

#Correcting leaf Numbers
#WAY C Plot5 - Rhynchospora macrochaeta
raw_data$Leaf_nr[raw_data$ID == "CMH0413"] <- "5"
#ACJ C Plot1 - Paspallum bonplandianum
raw_data$Leaf_nr[raw_data$ID == "ABS7148"] <- "5"
#TRE C Plot2 - Halenia umbellata
raw_data$Leaf_nr[raw_data$ID == "BNT3287"] <- "1"
raw_data$Leaf_nr[raw_data$ID == "BOO5640"] <- "2"
raw_data$Leaf_nr[raw_data$ID == "BOB7266"] <- "3"
raw_data$Leaf_nr[raw_data$ID == "BNX0682"] <- "4"
raw_data$Leaf_nr[raw_data$ID == "BOF6005"] <- "5"

#TRE C Plot4 - Halenia ubellata
raw_data$Leaf_nr[raw_data$ID == "AQF9992"] <- "4"
#TRE C Plot4 - vaccinium floribundum
raw_data$Leaf_nr[raw_data$ID == "BKW1860"] <- "5"

#Tre C Plot2 - Lachemilla orbiculara
raw_data$Plot_ID[raw_data$ID == "CPB8831"] <- "2"
raw_data$Plot_ID[raw_data$ID == "CPZ4857"] <- "2"
raw_data$Plot_ID[raw_data$ID == "CDQ3030"] <- "2"
raw_data$Plot_ID[raw_data$ID == "CPJ4884"] <- "2"
raw_data$Plot_ID[raw_data$ID == "CXC6033"] <- "2"

#TRE C Plot4 - Paspallum bonplandianum
raw_data$Plot_ID[raw_data$ID == "BUS1756"] <- "2"


#Changing bulk number of leaves for TRE C Plot5 - Lachemilla orbiculata
raw_data$Bulk_nr_leaves[raw_data$ID == "CFG0732"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "APS3068"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CFK2286"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "BKP5877"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "AYF7496"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "APO8208"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CEE0397"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "AYG1241"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CWL1543"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CNV5505"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CXA8168"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "AYI4751"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CEA4386"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CZN4729"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CZJ5895"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CZE7072"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CZF0686"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CZA2309"] <- "NA"
raw_data$Bulk_nr_leaves[raw_data$ID == "CFG0732"] <- "NA"

#Fixing individual and leaf numbers - to get rid of duplicates or wrong number
raw_data <- raw_data %>% mutate (Individual_nr = case_when(
#WAY C plot1
  #Lachemilla orbiculata
    ID %in% c("AJH6752","CDQ3517","CDU6030", "CDR9450","CAS0411")~11,
    ID %in% c("CDS3004","CAW9605","CDP3189")~12,
  #Paspallum bonplandianum
    ID %in% c("BYH1355","BYR4663","BYO7151","BYQ5420","BYP4529")~11,
#WAY C Plot2 - Lachemilla orbiculata
    ID %in% c("BZH3536") ~2,
#ACJ C Plot1 - Hypericum andinum
    ID %in% c("AZG0737") ~3,
#TRE C PLOT1
  #Gautheria glomerata
    ID %in% c("CYZ6287") ~11,
  #Halenia umbellata
    ID %in% c("AFR0503") ~11,
  #Lachemilla orbiculata
    ID %in% c("APQ5417","APM3849","CET3376","CFB5184","CEX5438") ~11,
    ID %in% c("APU4440") ~12,
  #Paspallum bonplandianum
    ID %in% c("CYF0619","CXS5928","CXW5856","CYA3349","CYE9151") ~11,
  #Rhynchospora macrochaeta
    ID %in% c("CXX4125") ~11,
  #Vaccinium floribundum
    ID %in% c("AGG8353","AGC5581","AFY5122","AFQ4784","AFU4318")~11,
#TRE C Plot2
  #Gautheria glomerata
    ID %in% c("BPR5529") ~11,
  #Halenia umbellata
    ID %in% c("BNT3287","BOO5640","BOB7266","BNX0682","BOF6005") ~11,
  #Lachemilla orbiculata
    ID %in% c("CPB8831","CPZ4857","CDQ3030","CPJ4884","CXC6033") ~11,
    ID %in% c("CDL6900","CBF5268","CBE4301","CAA3576","COY3821") ~12,
    ID %in% c("BOM7625","BMU4639","BMY5426","BLC7580","AFV6468") ~13,
  #Paspallum bonplandianum
    ID %in% c("BOJ2251","BON0490","BNO6456","BNS6580","BOA7727") ~11,
  #Trichophorum rigidum
    ID %in% c("CVL7949") ~11,
  #Vaccinium floribundum
    ID %in% c("CRP0313") ~11,
#TRE C Plot3
  #Gaultheria glomerata
    ID %in% c("CRL7437") ~11,
   #Halenia umbellata
    ID %in% c("CRT9362") ~11,
    ID %in% c("BPV3469") ~12,
    ID %in% c("BPZ6022") ~13,
  #Lachemilla orbiculata
    ID %in% c("BMO5598") ~3,
  #Rhynchospora macrochaeta
    ID %in% c("AGV6997") ~3,
  #Vaccinium floribundum
    ID %in% c("CNW9172") ~3,
    ID %in% c("CJV5222","CJZ6289","CKH7214","CKD0965","CKL1087") ~11,
    ID %in% c("AGN6553") ~12,
#TRE C Plot4
  #Carex pinchinchensis
    ID %in% c("BND3322") ~11,
    ID %in% c("BOK4212") ~12,
    ID %in% c("BOP2043") ~13,
  #Gaultheria glomerata
    ID %in% c("CIX6883","CHM7527","CHQ3814","CHU2037","CHY1829") ~11,
    ID %in% c("BMS4371") ~12,
  #Halenia ubellata
    ID %in% c("AYN2714","ATY7432","CEY7459","AQF9992","AQE5913") ~11,
    ID %in% c("CWW4359","CWS7219","CWO1363","CWK0867","AOW1278") ~12,
  #Lachemilla orbiculata
    ID %in% c("BLQ1414") ~11,
  #Paspallum bonplandianum
    ID %in% c("BLU0713") ~11,
  #Rhynchospora macrochaeta
    ID %in% c("AGR0051") ~11,
  #vaccinium floribundum
    ID %in% c("BMC0849") ~3,
    ID %in% c("CIG8658") ~2,
#TRE C PLOT5
  #Gauthieria glomerata
    ID %in% c("AGS6951") ~11,
  #Lachemilla orbiculata
    ID %in% c("CZA2309","CZF0686","CZE7072","CZJ5895","CZN4729") ~11,
    ID %in% c("CXA8168","CXE6426") ~12,
  #Paspalum bonplandianum
    ID %in% c("CZI4416","AGE7337","AGI7187","AGM1060","AGQ6196") ~11,
    ID %in% c("BNI4584") ~12,
    ID %in% c("BNN1844") ~13,
    ID %in% c("AGZ2031") ~14,
  #Rhynchospora macrochaeta
    ID %in% c("CYB3607") ~11,
#ACJ BB Plot1 - Carex pygmaea
    ID %in% c("BLA0567") ~11,
    ID %in% c("BMT3246") ~12,
    ID %in% c("BLI6737") ~13,
#ACJ BB Plot5 - Cortaderia bifida
    ID %in% c("CSC5324") ~11,
#QUE BB Plot1 
  #Werneria villosa
    ID %in% c("BDA5299") ~11,
    ID %in% c("BDE7008") ~12,
    ID %in% c("BDG8135") ~13,
  #Oreomyrrhis andicola
    ID %in% c("ATA1063","AWF9650","ATU6733","AFB7921","AFC7174") ~11,
#QUE BB Plot2 - Oritrophium hieracioides
    ID %in% c("AKR3541","BCE4857","AZJ1578") ~11,
#TRE BB Plot1 - Carex pygmaea
    ID %in% c("CVQ1316") ~11,
#TRE BB Plot2 - Carex pygmaea
  ID %in% c("ARP4944") ~11,
  ID %in% c("ART2867") ~12,
  ID %in% c("ARX5668") ~13,
  
TRUE ~ Individual_nr))

#verifying that there is only one leaf for each leaf#/individual#/species/plot/site 
leaf_count <- raw_data %>% 
  group_by(Project, Experiment, Site, Plot_ID, genus_species, Individual_nr, Leaf_nr) %>%
  summarise(n())



#'Need to clearn the following names:
#'Carex pinchinchensis
#'Carex pygmeae
#'Gautheria glomerata