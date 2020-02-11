### SEAN
# 26 leaves

sean <- c("EOK2330", "EOI5046", "EOM0461", "EOQ5582", "EYW8013", "EYX1643", "EOR9773", "EOP2116", "EON6400", "EOT2012", "EOV1633", "EOX9894", "EOU7550", "EOW7113", "EOY8783", "BFX2189", "BLB0373", "BTB2511", "AWD8931", "AWC4073", "AWO1307", "HAL6233", "HAK3559", "HAJ9138", "HAI3690", "HAH2681")

setdiff(sean, ID.list)

DataSean <- traits.raw %>% 
  filter(ID %in% sean) %>% 
  ### JOIN TRAITS WITH LEAF AREA
  left_join(LeafArea2018, by = "ID")

save(traits.raw, file = "DataSean.Rdata")
