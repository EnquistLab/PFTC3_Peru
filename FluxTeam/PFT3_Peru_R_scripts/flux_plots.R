library("ggplot2")

#Change sep to ',' when you actually write csv's
nee <- read.csv("peru_ecosystem_flux_output.csv", sep=",")
soil_r <- read.csv("peru_soil_respiration_output.csv", sep = ",")

photo <- which(nee$Flux == 'P')
resp <- which(nee$Flux == 'R')

### Density plots of k observations (change k to variable, change Type to site, treatment) ###
  ggplot(nee, aes(x = nee_lm, group = Flux, color = Flux)) + 
    geom_density() + 
    facet_wrap(~Site) 

  ggplot(soil_r, aes(x = nee_lm)) + 
    geom_density() + 
    facet_wrap(~Site)

### Scatterplot of data - for exploration  ###


q <- ggplot(nee[photo,], aes(x=PAR, y=nee_lm, colour=Site)) +  
          geom_point(size = 6) +
                    theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black", size=0),
                axis.ticks.length = unit(.25, "cm"),
                axis.ticks = element_line(size=0.75),
                text = element_text(size=25, colour = "black"),
                axis.title.x = element_text(size=25, margin = margin(t = 30, b = 20)), #vjust not working??
                axis.title.y = element_text(size=25, margin = margin(t = 20, b = 20, r = 30)),
                axis.text.x = element_text(margin = margin(t = 15)),
                axis.text.y = element_text(margin = margin(r = 15))) +
          #scale_fill_gradientn(breaks=c("0", "1", "2", "3"), 
                          # labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_x_discrete(breaks=c("0", "1", "2", "3"),
                         #  labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_y_continuous(breaks=seq(0,40,5)) +       ## use expand = c(0,0) here for removing space under bars
          labs(x = (expression(paste(
            "Photosynthetically Active Radiation (PAR)"))), 
               y = (expression(paste(
                 "Carbon flux (ppm)")))) +
          geom_smooth(method=lm,   #or use 'loess' for moving
                      se=TRUE,
                      color="red")

q <- ggplot(nee[photo,], aes(x=Soil_Temp, y=nee_lm, colour=Site)) +  
          geom_point(size = 6) +
                    theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black", size=0),
                axis.ticks.length = unit(.25, "cm"),
                axis.ticks = element_line(size=0.75),
                text = element_text(size=25, colour = "black"),
                axis.title.x = element_text(size=25, margin = margin(t = 30, b = 20)), #vjust not working??
                axis.title.y = element_text(size=25, margin = margin(t = 20, b = 20, r = 30)),
                axis.text.x = element_text(margin = margin(t = 15)),
                axis.text.y = element_text(margin = margin(r = 15))) +
          #scale_fill_gradientn(breaks=c("0", "1", "2", "3"), 
                          # labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_x_discrete(breaks=c("0", "1", "2", "3"),
                         #  labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_y_continuous(breaks=seq(0,40,5)) +       ## use expand = c(0,0) here for removing space under bars
          labs(x = (expression(paste(
            "Soil Temperature (C)"))), 
               y = (expression(paste(
                 "Carbon flux (ppm)")))) +
          geom_smooth(method=lm,   #or use 'loess' for moving
                      se=TRUE,
                      color="red")
 
q <- ggplot(nee[photo,], aes(x=Veg_Temp, y=nee_lm, colour=Site)) +  
          geom_point(size = 6) +
                    theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black", size=0),
                axis.ticks.length = unit(.25, "cm"),
                axis.ticks = element_line(size=0.75),
                text = element_text(size=25, colour = "black"),
                axis.title.x = element_text(size=25, margin = margin(t = 30, b = 20)), #vjust not working??
                axis.title.y = element_text(size=25, margin = margin(t = 20, b = 20, r = 30)),
                axis.text.x = element_text(margin = margin(t = 15)),
                axis.text.y = element_text(margin = margin(r = 15))) +
          #scale_fill_gradientn(breaks=c("0", "1", "2", "3"), 
                          # labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_x_discrete(breaks=c("0", "1", "2", "3"),
                         #  labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_y_continuous(breaks=seq(0,40,5)) +       ## use expand = c(0,0) here for removing space under bars
          labs(x = (expression(paste(
            "Vegetation Temperature (C)"))), 
               y = (expression(paste(
                 "Carbon flux (ppm)")))) +
          geom_smooth(method=lm,   #or use 'loess' for moving
                      se=TRUE,
                      color="red")
        
q <- ggplot(nee[resp,], aes(x=Soil_Temp, y=nee_lm, colour=Site)) +  
          geom_point(size = 6) +
                    theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black", size=0),
                axis.ticks.length = unit(.25, "cm"),
                axis.ticks = element_line(size=0.75),
                text = element_text(size=25, colour = "black"),
                axis.title.x = element_text(size=25, margin = margin(t = 30, b = 20)), #vjust not working??
                axis.title.y = element_text(size=25, margin = margin(t = 20, b = 20, r = 30)),
                axis.text.x = element_text(margin = margin(t = 15)),
                axis.text.y = element_text(margin = margin(r = 15))) +
          #scale_fill_gradientn(breaks=c("0", "1", "2", "3"), 
                          # labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_x_discrete(breaks=c("0", "1", "2", "3"),
                         #  labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_y_continuous(breaks=seq(0,40,5)) +       ## use expand = c(0,0) here for removing space under bars
          labs(x = (expression(paste(
            "Soil Temperature (C)"))), 
               y = (expression(paste(
                 "Carbon flux (ppm)")))) +
          geom_smooth(method=lm,   #or use 'loess' for moving
                      se=TRUE,
                      color="red")
 
q <- ggplot(nee[resp,], aes(x=Veg_Temp, y=nee_lm, colour=Site)) +  
          geom_point(size = 6) +
                    theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black", size=0),
                axis.ticks.length = unit(.25, "cm"),
                axis.ticks = element_line(size=0.75),
                text = element_text(size=25, colour = "black"),
                axis.title.x = element_text(size=25, margin = margin(t = 30, b = 20)), #vjust not working??
                axis.title.y = element_text(size=25, margin = margin(t = 20, b = 20, r = 30)),
                axis.text.x = element_text(margin = margin(t = 15)),
                axis.text.y = element_text(margin = margin(r = 15))) +
          #scale_fill_gradientn(breaks=c("0", "1", "2", "3"), 
                          # labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_x_discrete(breaks=c("0", "1", "2", "3"),
                         #  labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_y_continuous(breaks=seq(0,40,5)) +       ## use expand = c(0,0) here for removing space under bars
          labs(x = (expression(paste(
            "Vegetation Temperature (C)"))), 
               y = (expression(paste(
                 "Carbon flux (ppm)")))) +
          geom_smooth(method=lm,   #or use 'loess' for moving
                      se=TRUE,
                      color="red")

q <- ggplot(soil_r, aes(x=Surface_Temp, y=nee_lm, colour=Site)) +  
          geom_point(size = 6) +
                    theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black", size=0),
                axis.ticks.length = unit(.25, "cm"),
                axis.ticks = element_line(size=0.75),
                text = element_text(size=25, colour = "black"),
                axis.title.x = element_text(size=25, margin = margin(t = 30, b = 20)), #vjust not working??
                axis.title.y = element_text(size=25, margin = margin(t = 20, b = 20, r = 30)),
                axis.text.x = element_text(margin = margin(t = 15)),
                axis.text.y = element_text(margin = margin(r = 15))) +
          #scale_fill_gradientn(breaks=c("0", "1", "2", "3"), 
                          # labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_x_discrete(breaks=c("0", "1", "2", "3"),
                         #  labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_y_continuous(breaks=seq(0,40,5)) +       ## use expand = c(0,0) here for removing space under bars
          labs(x = (expression(paste(
            "Surface Temperature (C)"))), 
               y = (expression(paste(
                 "Carbon flux (ppm)")))) +
          geom_smooth(method=lm,   #or use 'loess' for moving
                      se=TRUE,
                      color="red")
 
q <- ggplot(soil_r[which(soil_r$Ground_Temp < 30),], aes(x=Ground_Temp, y=nee_lm, colour=Site)) +  
          geom_point(size = 6) +
                    theme_classic() +
          theme(panel.border = element_rect(colour = "black", fill=NA, size=0),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black", size=0),
                axis.ticks.length = unit(.25, "cm"),
                axis.ticks = element_line(size=0.75),
                text = element_text(size=25, colour = "black"),
                axis.title.x = element_text(size=25, margin = margin(t = 30, b = 20)), #vjust not working??
                axis.title.y = element_text(size=25, margin = margin(t = 20, b = 20, r = 30)),
                axis.text.x = element_text(margin = margin(t = 15)),
                axis.text.y = element_text(margin = margin(r = 15))) +
          #scale_fill_gradientn(breaks=c("0", "1", "2", "3"), 
                          # labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_x_discrete(breaks=c("0", "1", "2", "3"),
                         #  labels=c("Mixed", "Low", "Medium", "High")) +
          #scale_y_continuous(breaks=seq(0,40,5)) +       ## use expand = c(0,0) here for removing space under bars
          labs(x = (expression(paste(
            "Soil Temperature (C)"))), 
               y = (expression(paste(
                 "Carbon flux (ppm)")))) +
          geom_smooth(method=lm,   #or use 'loess' for moving
                      se=TRUE,
                      color="red")
