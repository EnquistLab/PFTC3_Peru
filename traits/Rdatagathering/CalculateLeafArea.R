
### CALCULATE LEAF AREA

# load libraries
devtools::install_github("richardjtelford/LeafArea")
library(LeafArea)

# Function to calculate leaf area
loop.files <-  function(files){
  
  file.copy(files, new.folder)
  #if(grepl("-NA$", files)){
  #newfile <- basename(files)
  #file.rename(paste0(new.folder, "/", newfile), paste0(new.folder,
  #"/", gsub("-NA$", "", newfile)))
  #}
  print(files)
  area <- try(run.ij(set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 50, trim.pixel2 = 150, save.image = TRUE))
  if(inherits(area, "try-error")){
    return(data.frame(LeafArea = NA))
  }
  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), output.folder)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(ID = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}


# test run.ij
run.ij(set.directory = "~/Desktop/TestLeaf", distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 50, trim.pixel2 = 150, save.image = TRUE)


### DELETE
#list.of.files <- dir(path = paste0("/Volumes/PFT3/Peru_leaves"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
#list.of.files16 <- dir(path = paste0("/Volumes/PFT3/Peru_leaves/16_03_2018"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
#list.of.files17 <- dir(path = paste0("/Volumes/PFT3/Peru_leaves/17_03_2018"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
#list.of.files18 <- dir(path = paste0("/Volumes/PFT3/Peru_leaves/18_03_18"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
#list.of.files19 <- dir(path = paste0("/Volumes/PFT3/Peru_leaves/19_03_18"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
#new.folder <- "/Volumes/PFT3/Temp"
#output.folder <- "/Volumes/PFT3/Output"
#output.folder <- "/Volumes/PFT3/Output16"
#output.folder <- "/Volumes/PFT3/Output17"
#output.folder <- "/Volumes/PFT3/Output18"
#output.folder <- "/Volumes/PFT3/Output19"

#LeafArea <- plyr::ldply(list.of.files, loop.files)
#LeafArea16 <- plyr::ldply(list.of.files16, loop.files)
#LeafArea17 <- plyr::ldply(list.of.files17, loop.files)
#LeafArea18 <- plyr::ldply(list.of.files18, loop.files)
#LeafArea19 <- plyr::ldply(list.of.files19, loop.files)
#LeafArea.raw <- LeafArea.raw %>% 
  #select(ID, LeafArea) %>% 
  #rbind(LeafArea19)


# Calculate leaf area
list.of.files <- dir(path = paste0("/Volumes/PFT3/Peru_leaves"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
new.folder <- "/Volumes/PFT3/Temp"
output.folder <- "/Volumes/PFT3/Output_Peru_28-5-2018"

LeafArea.raw <- plyr::ldply(list.of.files, loop.files)

dim(LeafArea.raw)
save(LeafArea.raw, file = "traits/data/LeafArea.raw.Rdata")

# remove duplicate leaves
LeafArea %>% 
  group_by(ID) %>% 
  filter()

# not whole on scan - area needs to be recalcuated with less croping
#BEE4484
#BEG0417
#CJY6856
#CRU9363
#CVH6840
#DBV0811
#DDM5359
#DED5654
#DEE3207
#DEH6810
#DEW7045
#DFO3811
#DFR5777
#DHQ0857
#DLU5214
#DNV2097
#DOM7296
#DOQ1487
#DOV9618
#DPB9248
#DPW5556
#DTO2944
#DUO6664
#DXD7866
#DXH3658
#DXQ1481
#DYG2933
#EBM5766
#ECM0628
#ECO7096
#EDE1797
#EDP2916
#EDT8254
#EFU8488
#EGP6185
#EJE1667
#EJT4683
#EMF4580
#EOC4565
#EOH0534
#EOJ0642
#EUG2994
#EUK3005
#EUR5376
#EUS8015
#FDA2506
#FDB0882
#EPV0866
#EPW2330
#EQO9652
#EQQ2320
#EQV8337
#ERM9285
#FBV8875
#FDL7538
#BEE4484
#BEG0417
#CJY6856
#EQJ1944
#FDA2506
#bov5280_1


# Scan again!!!
#EHP2066 empty and double - scan is ok
#FDF1809 empty and double - scan is ok
#DOK4761 empty and double - scan is ok



#### Sean leaf areas without loop

file.list.sean <- list.files(path = "C:/Users/cpo082/Desktop/leaf
                             data/SEAN_cropped")

sean_area <- run.ij (set.directory = "C:/Users/cpo082/Desktop/leaf
                     data/SEAN_cropped", distance.pixel = 237, known.distance = 2, log =
                       TRUE, save.image = TRUE, low.size = 0.05)

sean_cropped_LA_new <- data.frame(ID = names(unlist(sean_area
                                                           [[2]])), LeafArea = (unlist(sean_area[[2]])))

save(sean_cropped_LA_new, file = "C:/Users/cpo082/Desktop/leaf
     data/sean_cropped_LA_new.Rdata")



