library("baRcodeR")
library("PFTCFunctions")

# get all codes for Peru
all_codes <- get_PFTC_envelope_codes(seed = 1)

# load cleaned trait data
traits_2018_Peru_cleaned <- read_csv(file = "traits/data/traits_2018_Peru_cleaned.csv", col_names = TRUE)

# anti_join 
unusedIDs <- all_codes %>%
  anti_join(traits_2018_Peru_cleaned, by = c("hashcode" = "ID")) %>% 
  slice(1:4000) %>% 
  as.data.frame()
#write.csv(unusedIDs, file = "LeafIDs_Peru_2020.csv", row.names = FALSE)


## specify column from data frame
custom_create_PDF(user = FALSE, Labels = unusedIDs, name = "traits/Peru2020_mylabels",type = "linear")

# only for now...
unusedIDs2 <- unusedIDs[1:48,]

# Function to make and print labels on PDF
# The magic combination for these lables: Avery 4778
#https://www.lyreco.com/webshop/NONO/etiketter-avery-45-7-x-21-2-mm-hvit-eske-c3a0-960-stk-product-000000000002760191.html
custom_create_PDF(Labels = unusedIDs2, name = "traits/LabelsOut",
                  type = "linear", Fsz = 14, Across = TRUE,
                  trunc = TRUE, numrow = 12, numcol = 4,
                  page_width = 8.3, page_height = 11.7, width_margin = 0.2,
                  height_margin = 0.7, label_width = 1.811, label_height = 0.5)


