library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

soil_r <- data.frame()
dates <- "18_3_1"
summaries <- " summary.csv"
setwd("site_data/soil_r_summaries")
days <- c(2, 4, 5, 6, 7)
for(i in seq(length(days)))
{
	title <- paste(dates, days[i], summaries, sep="")
	print(title)
	new_soil_r <- read.csv(title)
	switch(days[i], NULL,
		new_soil_r$SITE <- rep(as.factor("WAY"), nrow(new_soil_r)),
		NULL,
		new_soil_r$SITE <- rep(as.factor("PIL"), nrow(new_soil_r)),
		new_soil_r$SITE <- rep(as.factor("ACJ"), nrow(new_soil_r)),
		new_soil_r$SITE <- rep(as.factor("QUE"), nrow(new_soil_r)),
		new_soil_r$SITE <- rep(as.factor("TRE"), nrow(new_soil_r)))
	soil_r <- rbind(soil_r, new_soil_r)
}

soil_r$TREAT <- as.factor(unlist(lapply(strsplit(as.character(soil_r$time), split=""), FUN="[[", 1)))
soil_r <- soil_r[which(soil_r['TREAT'] != 'T'),]
soil_r$PLOT <- as.factor(unlist(lapply(strsplit(as.character(soil_r$time), split=""), FUN="[[", 2)))
soil_r$COLLAR <- as.factor(unlist(lapply(strsplit(as.character(soil_r$time), split=""), FUN="[[", 3)))

#Quickly hack away your problems
e1 <- which(soil_r$SITE == "PIL" & soil_r$TREAT == "B" & soil_r$PLOT == '4' & soil_r$COLLAR == 'a')[c(1,2)]

soil_r <- soil_r[-e1,]

#Process environmental data
setwd("../../")
soil_env <- read.csv("soil_env_CTC.csv")
soil_env$Code <- as.factor(paste(soil_env$Site, soil_env$Plot, soil_env$Treatment, soil_env$Collar, sep="_"))

ground_measures <- c("Soil_Temp_1", "Soil_Temp_2")
ground_melt <- melt(soil_env[,c("Code", ground_measures)])
avg_ground <- ddply(ground_melt, .(Code), summarise, Ground_Temp=mean(value, na.rm=TRUE))

surface_measures <- c("Surface_Temp_1", "Surface_Temp_2")
surface_melt <- melt(soil_env[,c("Code", surface_measures)])
avg_surface <- ddply(surface_melt, .(Code), summarise, Surface_Temp=mean(value, na.rm=TRUE))

height_measures <- c("Height_1", "Height_2", "Height_3", "Height_4")
height_melt <- melt(soil_env[,c("Code", height_measures)])
avg_height <- ddply(height_melt, .(Code), summarise, Height=mean(value, na.rm=TRUE))

soil_plot_env <- merge(merge(avg_ground, avg_surface), avg_height)
soil_plot_info <- strsplit(as.character(soil_env$Code), split="_")
soil_plot_env$SITE <- as.factor(unlist(lapply(soil_plot_info, FUN="[[", 1)))
soil_plot_env$PLOT <- as.factor(unlist(lapply(soil_plot_info, FUN="[[", 2)))
soil_plot_env$TREAT <- as.factor(unlist(lapply(soil_plot_info, FUN="[[", 3)))
soil_plot_env$COLLAR <- as.factor(unlist(lapply(soil_plot_info, FUN="[[", 4)))

soil_r <- merge(soil_r, soil_plot_env)
soil_r$Code <- as.factor(paste(soil_env$Site, soil_env$Plot, soil_env$Treatment, sep="_"))
#Average the collars

soil_r <- soil_r %>% group_by(Code) %>% summarise_at(c("nee_lm", "Ground_Temp", "Surface_Temp", "Height"), mean, na.rm=TRUE)
soil_r <- as.data.frame(soil_r)
soil_r_info <- strsplit(as.character(soil_r$Code), split="_")
soil_r$Site <- as.factor(unlist(lapply(soil_r_info, FUN="[[", 1)))
soil_r$Plot <- as.factor(unlist(lapply(soil_r_info, FUN="[[", 2)))
soil_r$Treatment <- as.factor(unlist(lapply(soil_r_info, FUN="[[", 3)))

write.table(soil_r, file="peru_soil_respiration_output.csv", sep=",")