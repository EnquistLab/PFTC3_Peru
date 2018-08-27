source("LICOR_7500_Flux_Analysis.R")
source("GAS_HOUND_ANALYSIS.R")

readline("Please set the working directory to the folder \n that contains the LiCOR files to be analyzed. \n Do so with the upcoming prompt. \n Note that you must choose a(any) file in the \n folder that you want to set as the working directory. \n Please press 'return' to continue.")
setwd(dirname(file.choose()))

directory <- dir()
stats.df <- c()

#Check for soil respiration data
soil.names <- grep("soil", dir(), value=TRUE)
if(length(soil.names) == 0)
{
	photo.names <- grep("[^resp].txt", grep("[^_a]\\.txt", dir(), value = TRUE), value = TRUE)
	ambient.names <- grep("a.txt", dir(), value = TRUE)
	resp.names <- grep("resp.txt", dir(), value = TRUE)	
	
	for (i in 1:length(photo.names))
	{
    		stats.df <- rbind(stats.df, nee.fit(photo.names[i]))
	}
  
	if (length(resp.names) > 0)
	{
		print("Performing ecosystem respiration fits")
      	for (i in 1:length(resp.names))
		{
      		stats.df <- rbind(stats.df, nee.fit(resp.names[i]))
      	}
	}
	stats.df <- as.data.frame(stats.df)
	names.vec <- c("tstart", "tfinish", "time", "camb", "tav", "pav", "wav", "nee_lm", "nee_exp", "LM rsqd", "non-linear sigma", "aic_lm", "aic_nlm")
	for(i in 1:length(names.vec))
	{
    		names(stats.df)[i] <- names.vec[i]
	}
	write.csv(stats.df, file = paste(paste(strsplit(getwd(), "/")[[1]][length(strsplit(getwd(), "/")[[1]])], "summary", sep = " "), ".csv", sep = ""))
}else
{
	for (i in 1:length(soil.names))
	{
    		stats.df <- rbind(stats.df, soil_r_fit(soil.names[i]))
	}
	stats.df <- as.data.frame(stats.df)
	names.vec <- c("tstart", "tfinish", "time", "tav", "pav", "wav", "nee_lm", "LM rsqd", "aic_lm")
	for(i in 1:length(names.vec))
	{
    		names(stats.df)[i] <- names.vec[i]
	}
	write.csv(stats.df, file = paste(paste(strsplit(getwd(), "/")[[1]][length(strsplit(getwd(), "/")[[1]])], "summary", sep = " "), ".csv", sep = ""))
}
  