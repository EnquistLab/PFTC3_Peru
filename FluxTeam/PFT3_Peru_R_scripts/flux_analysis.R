library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

setwd("site_data/nee_summaries")
nee <- data.frame()
dates <- "18_3_1"
summaries <- " summary.csv"

days <- c(2, 3, 4, 5, 6, 7)
for(i in seq(length(days)))
{
	title <- paste(dates, days[i], summaries, sep="")
	print(title)
	new_nee <- read.csv(title)
	switch(days[i], NULL,
		new_nee$SITE <- rep(as.factor("WAY"), nrow(new_nee)),
		new_nee$SITE <- rep(as.factor("PIL"), nrow(new_nee)),
		new_nee$SITE <- rep(as.factor("PIL"), nrow(new_nee)),
		new_nee$SITE <- rep(as.factor("ACJ"), nrow(new_nee)),
		new_nee$SITE <- rep(as.factor("TRE"), nrow(new_nee)),
		new_nee$SITE <- rep(as.factor("QUE"), nrow(new_nee)))
	nee <- rbind(nee, new_nee)
}

nee_info <- strsplit(as.character(nee$time), split="")
nee$TREAT <- as.factor(unlist(lapply(nee_info, FUN="[[", 1)))
nee$PLOT <- as.factor(unlist(lapply(nee_info, FUN="[[", 2)))
nee$FLUX <- "P"
fluxes <- grepl("resp", nee$time)
nee[which(fluxes),]$FLUX <- rep("R", length(which(fluxes))) 
nee$FLUX <- as.factor(nee$FLUX)

#Check which observations are redos
nee$REDO <- TRUE
orig <- grep(".txt", nee$time, value=FALSE)
nee[orig,]$REDO = FALSE
nee[which(nee$SITE == "WAY"),]$REDO = FALSE

#nee <- nee[which(nee['TREAT'] != 'T'),]

resp = which(nee['FLUX']=='R')
photo = which(nee['FLUX']!='R')

setwd("../../")
env <- read.csv("flux_env_CTC.csv")
env$Code <- as.factor(paste(env$Site, env$Plot, env$Treatment, env$Flux_type, sep="_"))

par_measures <- c("PAR_1", "PAR_2", "PAR_3", "PAR_4", "PAR_5", "PAR_6", "PAR_7")
par_melt <- melt(env[,c("Code", par_measures)])
avg_par <- ddply(par_melt, .(Code), summarise, PAR=mean(value, na.rm=TRUE))

veg_measures <- c("Veg_temp_1", "Veg_temp_2", "Veg_temp_3", "Veg_temp_4", "Veg_temp_5")
veg_melt <- melt(env[,c("Code", veg_measures)])
avg_veg <- ddply(veg_melt, .(Code), summarise, Veg_Temp=mean(value, na.rm=TRUE))

soil_measures <- c("Soil_tem_1", "Soil_tem_2")
soil_melt <- melt(env[,c("Code", soil_measures)])
avg_soil <- ddply(soil_melt, .(Code), summarise, Soil_Temp=mean(value, na.rm=TRUE))

plot_env <- merge(merge(avg_par, avg_veg), avg_soil)

plot_string <- as.character(plot_env$Code)
env_redo <- grep("redo", plot_string)
plot_env$REDO <- FALSE
plot_env[env_redo,]$REDO <- TRUE
plot_string <- gsub("_redo[2]?", "", plot_string)
plot_string <- gsub("BB", "T", plot_string)

plot_info <- strsplit(plot_string, split="_")

plot_env$SITE <- as.factor(unlist(lapply(plot_info, FUN="[[", 1)))
plot_env$PLOT <- as.factor(unlist(lapply(plot_info, FUN="[[", 2)))
plot_env$TREAT <- as.factor(unlist(lapply(plot_info, FUN="[[", 3)))
plot_env$FLUX <- as.factor(unlist(lapply(plot_info, FUN="[[", 4)))

plot_env$Code <- as.factor(paste(env$Site, env$Plot, env$Treatment, env$Flux_type, sep="_"))

#plot_env <- plot_env[which(plot_env['TREAT'] != 'T'),]

#Exclude observations and average redos - see excluded_observations.txt
e1 <- which(nee$SITE == 'PIL' & nee$TREAT == 'B' & nee$PLOT == '2' & nee$FLUX == 'P' & nee$REDO)
e2 <- which(nee$SITE == 'PIL' & nee$TREAT == 'B' & nee$PLOT == '3' & nee$FLUX == 'P')

#e3 <- which(nee$SITE == 'PIL' & nee$TREAT == 'T' & nee$PLOT == '1' & nee$FLUX == 'P')
#e4 <- which(nee$SITE == 'PIL' & nee$TREAT == 'T' & nee$PLOT == '3' & nee$FLUX == 'P')
#e5 <- which(nee$SITE == 'PIL' & nee$TREAT == 'T' & nee$PLOT == '3' & nee$FLUX == 'R')

#Missing plot 1 control photosynthesis (bad data)
e9 <- which(nee$SITE == 'PIL' & nee$TREAT == 'C' & nee$PLOT == '1' & nee$FLUX == 'P')
nee[e9,]$nee_lm <- NA

#Remove measurements from rainy day
pil_extras <- which(nee$SITE == 'PIL' & nee$TREAT == 'T' & (nee$PLOT == '1' | nee$PLOT == '2') & !nee$REDO)[c(1,2,3,4)]

#Keep the second photosynthesis redo (of 3)
pil_redo <- which(nee$SITE == 'PIL' & nee$TREAT == 'T' & nee$PLOT == '3' & nee$FLUX == 'P' & nee$REDO)[c(1,3)]

#Keep the final photosynthesis redo (of 3)
pil_resp_redo <- which(nee$SITE == 'PIL' & nee$TREAT == 'T' & nee$PLOT == '3' & nee$FLUX == 'R' & nee$REDO)[c(1,2)]

#Keep the first photosynthesis redo
e6 <- which(nee$SITE == 'QUE' & nee$TREAT == 'B' & nee$PLOT == '1' & nee$FLUX == 'P' & nee$REDO)[2]
new_que <- nee[e6,]
new_que$nee_lm <- NA
new_que$FLUX <- as.factor("R")
new_que$REDO <- FALSE
nee <- rbind(nee, new_que)

#Missing plot 1 resp measure
e7 <- which(nee$SITE == 'QUE' & nee$TREAT == 'B' & nee$PLOT == '1' & nee$FLUX == 'P')

#nee[e7,]$nee_lm <- NA

#Use first photo redo measure
e8 <- which(nee$SITE == 'QUE' & nee$TREAT == 'B' & nee$PLOT == '5' & nee$FLUX == 'P' & !nee$REDO)

diff <- setdiff(seq(1,nrow(nee)),c(e1, e6, e8, pil_extras, pil_redo, pil_resp_redo)) 
nee_split <- nee[diff,]

replacement_frame <- data.frame()
split_names <- c("nee_lm", "SITE", "PLOT", "TREAT", "FLUX", "REDO")
nee_split <- nee_split[,split_names]
numerics <- c("nee_lm")
normals <- c()
redos <- which(nee_split$REDO)
for(i in seq(1, length(redos)))
{
	redo <- nee_split[redos[i],]
	normal <- which(nee_split$SITE == redo$SITE & nee_split$TREAT == redo$TREAT & 
		nee_split$PLOT == redo$PLOT & nee_split$FLUX == redo$FLUX & !nee_split$REDO)
	normals <- c(normals, normal)
	mix <- rbind(redo, nee_split[normal,])
	cm <- mean(mix[,numerics])
	nee_split[redos[i],]$nee_lm <- cm
}
nee_split <- nee_split[-c(normals),]
nee_split$Code <- as.factor(paste(nee_split$SITE, nee_split$PLOT, nee_split$TREAT, nee_split$FLUX, sep="_"))

#Should be 10 * WAY + 13 * PIL + 10 * ACJ + 5 * QUE + 5 * TRE = 43 plots * (R,P) = 86 observations
plot_env <- plot_env[-which(plot_env$REDO),]
plot_env$Code <- as.factor(paste(plot_env$SITE, plot_env$PLOT, plot_env$TREAT, plot_env$FLUX, sep="_"))

flux_environment <- merge(nee_split, plot_env, by=c("Code"))
flux_environment$Code <- as.factor(paste(flux_environment$SITE.x, flux_environment$PLOT.x, 
				flux_environment$TREAT.x, sep="_"))
flux_environment[which(flux_environment$FLUX.x == 'R'),]$nee_lm <- 
	flux_environment[which(flux_environment$FLUX.x == 'R'),]$nee_lm * (-1)
ff <- flux_environment %>% group_by(Code) %>% summarise_at(c("nee_lm"), sum, na.rm=TRUE)
flux_environment[which(flux_environment$FLUX.x == "P"),]$nee_lm <- ff$nee_lm
flux_environment[which(flux_environment$FLUX.x == 'R'),]$nee_lm <- 
	flux_environment[which(flux_environment$FLUX.x == 'R'),]$nee_lm * (-1)
flux_environment <- flux_environment[,-c(7, 11, 12, 13, 14, 15)]
colnames(flux_environment) <- c("Code", "nee_lm", "Site", "Plot", "Treatment", "Flux", "PAR", "Veg_Temp", "Soil_Temp")

write.table(flux_environment, file="peru_ecosystem_flux_output.csv", sep=",")
