library("tidyverse")
library("writexl")


# Make List with ID
dat <- all_codes %>% 
  select(hashcode) %>% 
  rename(ID = hashcode)
write_xlsx(dat, path = "UniqueID.xlsx", col_names = TRUE)

