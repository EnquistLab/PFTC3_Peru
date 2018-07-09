##Biomass per species and plots####
###Original data collected by Marteen van den Eyndenn, Imma Oliveras and Nelson Cahuana in 2010
###@I.Oliveras 20 March 2018.

#Load raw data
datamar = read.csv("C:\\Users\\Imma\\Dropbox\\Lectures\\Peru Field Course 2018\\Biomass2010.csv")
str(datamar)
head(datamar)
tail(datamar)
datamar <- tbl_df(datamar[-c(3309:3313),])

library(dplyr)

#####Calculating volumes - following Johnsonetal1988#####
###The chose of a model depends on Species architecture
####In Oliveraset al (2014) we used elliptical cone cylinders (ECC)
ecc <- function(height, D1, D2, B1, B2) {
  volume = (pi*height/6)*((D1*D2/4)+ (D2*B1/4) + (D1*D2/2) +(B1*B2/2))
  return(volume)
}

datamar$volumecm3 <- ecc(datamar$H.max.cm, datamar$Can.D1.cm, datamar$Can.D2.cm, (datamar$Basal.D1.mm/10), (datamar$Basal.D2.mm/10))

###Assumption = sum of canopy areas is 100% cover
area = function(r){
  area = pi*r^2
  return(area)
}

datamar$C.area.cm2 = area(datamar$Can.D.av.cm)


###COver by Plot and Species
res <- c()
for (ii in 1:65){
  plot <- as.character(datamar$Plot[[ii]])
  datar <- datamar[datamar$Plot %in% plot,]
  totar <- sum(datar$C.area.cm2)
  cvsp = datar %>% group_by(Species) %>% 
    summarise(relcover= sum(C.area.cm2)/totar, biomass=sum(Dw.g), Volume.Cm3 = sum(volumecm3), Height.cm = mean(H.max.cm))
  n = nrow(cvsp)
  res2 <- data.frame(Plot= rep(plot, n), cvsp)
  res <- rbind(res, res2)
}

#####Max height and Biomass per Treatment and species######
av1 = datamar %>% group_by(Treatment, Species) %>% 
  summarise(Hmax.cm = mean(H.max.cm, na.rm=TRUE), Biomass= sum(Dw.g, na.rm=TRUE), nind = n())

#####Max height and Biomass per Plot and species######
av2 = datamar %>% group_by(Plot, Treatment, Species) %>% 
  summarise(Hmax.cm = mean(H.max.cm, na.rm=TRUE), Biomass= sum(Dw.g, na.rm=TRUE), nind = n())
