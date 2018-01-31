check_image <- function(file){
  require("exifr")

  
  file <- basename(file)  
  
  # check extension
  if(!grepl("([^\\s]+(\\.(jpg|jpeg))$)", file, ignore.case = TRUE)){
    stop("File extension on ", file, " not permitted - use '.jpg'")
  }
  
  # check file name is permitted
 
  if(!grepl("^[A-Z]{3}\\d{4}\\.(jpg|jpeg)$", file, ignore.case = TRUE)){
    stop("File name ", file, " not expected format (3-letters, 4-numbers)")
  }
  file_base <- gsub("(^[A-Z]{3}\\d{4}).*", "\\1", file)

  source("R/envelope_codes.R")# makes all_codes  
  if(!file_base %in% all_codes$hashcode){
    stop("File name ", file, " not in list of permitted names")
  }
  

  # check exif information is good
  #correct resolution
  #correct size (with tolerance)
  #colour depth
  
  #imagej check
  
  print("Passed checks")
}
  

check_image("folder/good.jpgx")
check_image("folder/AA1111.jpg")
check_image("folder/AAA1111.jpg")
check_image("folder/AAA4667.jpg")

