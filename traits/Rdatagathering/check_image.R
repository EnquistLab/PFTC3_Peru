check_image <- function(pathfile, check_ij = TRUE){
  resolution <- 300
  imageSize <- "2552x3508"
  BitsPerSample <- 8 # colour depth
  
  file <- basename(pathfile)  
  
  # check extension
  if(!grepl("([^\\s]+(\\.(jpg|jpeg))$)", file, ignore.case = TRUE)){
    stop("File extension on ", file, " not permitted - use '.jpg'")
  }
  
  # check file name is permitted
 
  if(!grepl("^[A-Z]{3}\\d{4}\\.(jpg|jpeg)$", file, ignore.case = TRUE)){
    stop("File name ", file, " not expected format (3-letters, 4-numbers)")
  }
  file_base <- gsub("(^[A-Z]{3}\\d{4}).*", "\\1", file)

  load("traits/Rdatagathering/envelope_codes.Rdata")# makes all_codes  

  if(!file_base %in% all_codes$hashcode){
    stop("File name ", file, " not in list of permitted names")
  }
  

  # check exif information is good
  require("exifr")
  exif <- read_exif(pathfile)
  #correct resolution
  if(exif$XResolution != resolution | exif$YResolution != resolution){
    stop("Scan resolution is ", exif$XResolution, " not expected ", resolution)
  }
  
  #correct size (with tolerance)
  if(exif$ImageSize != imageSize){
    stop("Scan size is ", exif$ImageSize, " pixcels not expected ", imageSize, " (A4)")
  }

    #colour depth
  if(exif$BitsPerSample != BitsPerSample){
    stop("Colour depth is ", exif$BitsPerSample, " bits not expected ", BitsPerSample, " (full colour)")
  }
  
  #imagej check
  if(isTRUE(check_ij)){
    #to do
  }
  
  print("Passed checks")
}
  

# check_image("folder/good.jpgx")
# check_image("folder/AA1111.jpg")
# check_image("folder/AAA1111.jpg")
# check_image("folder/AAA4667.jpg")
