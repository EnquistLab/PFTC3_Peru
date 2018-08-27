library(ggplot2)

#setwd("site_data")
setwd("E:/PFT3_Peru_R_scripts/site_data")
plots <- data.frame()
dates <- "18_3_1"
summaries <- " summary.csv"

days <- c(2, 3, 4, 5, 6, 7)
for(i in seq(length(days)))
{
	title <- paste(dates, days[i], summaries, sep="")
	print(title)
	new_plots <- read.csv(title)
	switch(days[i], NULL,
		new_plots$SITE <- rep(as.factor("WAY"), nrow(new_plots)),
		new_plots$SITE <- rep(as.factor("PIL"), nrow(new_plots)),
		new_plots$SITE <- rep(as.factor("PIL"), nrow(new_plots)),
		new_plots$SITE <- rep(as.factor("ACJ"), nrow(new_plots)),
		new_plots$SITE <- rep(as.factor("TRE"), nrow(new_plots)),
		new_plots$SITE <- rep(as.factor("QUE"), nrow(new_plots)))
	plots <- rbind(plots, new_plots)
}

plots$TREAT <- as.factor(unlist(lapply(strsplit(as.character(plots$time), split=""), FUN="[[", 1)))
plots <- plots[which(plots['TREAT'] != 'T'),]
plots$PLOT <- as.factor(unlist(lapply(strsplit(as.character(plots$time), split=""), FUN="[[", 2)))
plots$FLUX <- "P"
fluxes <- grepl("resp", plots$time)
plots[which(fluxes),]$FLUX <- rep("R", length(which(fluxes))) 
plots$FLUX <- as.factor(plots$FLUX)

resp = which(plots['FLUX']=='R')
photo = which(plots['FLUX']!='R')

#### NEE - ecosystem exchange


  # Run only for proper TIFF figure out

      tiff("Ecosystem_NEE.tiff", width = 10, 
        height = 6, 
        units = 'in', 
        res = 600)

  # Figure coding 

gp <- ggplot(plots, aes(x=factor(SITE, levels=c('WAY',
                                                'ACJ',
                                                'PIL',
                                                'TRE',
                                                'QUE'),ordered=TRUE), 
                        y=nee_lm, fill = SITE))
gop <- gp + geom_boxplot() + scale_y_continuous(limits=c(-7.5,7.5)) +
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
        axis.text.y = element_text(margin = margin(r = 15)),
        legend.position="none") +
  labs(x = "Individual sites", y = "NEE (umol CO2/m2/s)")
gop

# Run this bit for figure export end
dev.off() 
  

#### Ecosystem respiration

  # Run only for proper TIFF figure out

      tiff("Ecosystem_Resp.tiff", width = 10, 
         height = 6, 
          units = 'in', 
            res = 600)
  
  # Figure coding 

gp <- ggplot(plots[resp,], aes(x=factor(SITE, levels=c('WAY',
                                                        'ACJ',
                                                       'PIL',
                                                       'TRE',
                                                       'QUE'),ordered=TRUE),
                                y=nee_lm, fill = SITE))
gop <- gp + geom_boxplot() + 
  scale_y_continuous(limits=c(-8,0)) +
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
        axis.text.y = element_text(margin = margin(r = 15)),
        legend.position="none") +
  labs(x = "Individual sites", y = "Re (umol CO2/m2/s)")
gop

  # Run this bit for figure out
    dev.off() 

    
#### GPP ??? some photosynthesis calculation    
    
    # Run only for proper TIFF figure out
    
    tiff("Ecosystem_GPP.tiff", width = 10, 
         height = 6, 
         units = 'in', 
         res = 600)
    
gp <- ggplot(plots[photo,], aes(x=factor(SITE, levels=c('WAY',
                                                       'ACJ',
                                                       'PIL',
                                                       'TRE',
                                                       'QUE'),ordered=TRUE),
                               y=nee_lm, fill = SITE))

gop <- gp + geom_boxplot() + 
  scale_y_continuous(limits=c(0,8)) +
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
        axis.text.y = element_text(margin = margin(r = 15)),
        legend.position="none") +
  labs(x = "Individual sites", y = "GPP (umol CO2/m2/s)")
gop

# Run this bit for figure out
dev.off() 