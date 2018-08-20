nee.calc <- function(){
  ## This version is from 11/3/16, it is the only NEE fitting code I have that includes the AIC test.  Copy those lines and paste into China (Inge, Claire, Sun) and RMBL (Lorah) files.  Check to see if current version will fail on china 2016 data.  If so, then next comment must be incorporated regarding summary statement, and error control must be improved.
  ## Additional needed corrections are...printing summary statement during crash to prevent complete analysis restart.
  ##This function is meant to perform the linear and non-linear fitting to nee data.  It will produce a plot of the data, queery the user if they would like to modify the time interval over which the data is fit, then print the paste statement of data values.  Currently, the function is written so that it works for a given filename.  From there, we can extend to a folder/directory.
  
  ## These packages are necessary.
  library("proto")
  library("nls2")
  
  ####----Insert here the readline() command for querying user for the director of LiCOR files.--------
  ## For querying user to define the working directory in which the LiCOR files are located.
  readline("Please set the working directory to the folder \n that contains the LiCOR files to be analyzed. \n Do so with the upcoming prompt. \n Note that you must choose a(any) file in the \n folder that you want to set as the working directory. \n Please press 'return' to continue.")
  setwd(dirname(file.choose()))
  
  ## Define directory as an object that contains the dir() item names as a vector.
  directory <- dir()
  
  ## For reading the .txt files, replace the ".txt" paste with ".txt"
#   photo.names <- directory[grep(paste("[0-9]", ".txt", sep = ""), dir(), ignore.case = TRUE, value = FALSE)]
#   ambient.names <- directory[grep(paste("[0-9]", "a", ".txt", sep = ""), dir(), ignore.case = TRUE, value = FALSE)]
#   resp.names <- directory[grep("resp", dir(), ignore.case = TRUE, value = FALSE)]

  photo.names <- grep("[^resp].txt", grep("[^_a]\\.txt", dir(), value = TRUE), value = TRUE)
  ambient.names <- grep("a.txt", dir(), value = TRUE)
  resp.names <- grep("_[[:digit:]]resp.txt", dir(), value = TRUE)
  
  nee.fit <- function(filename){
    ## For reading the .txt files, replace read.csv with read.table, and add a skip = 9 parameter to the function.
    input <- read.table(filename, header = FALSE, skip = 9)
    ## For reading the .txt files, these input subsetting commands may not be necessary.
#     input <- input[-1,]
#     input <- input[,-1]
    
    if(length(grep("resp", filename, ignore.case = TRUE, value = FALSE)) == 1){
      ambient <- read.table(paste(strsplit(filename, "resp.txt"), "a.txt", sep = ""), header = FALSE, skip = 9)
    } else{
      ambient <- read.table(paste(strsplit(filename, ".txt"), "a.txt", sep = ""), header = FALSE, skip = 9)
    }
#     ambient <- ambient[,-1]
#     ambient <- ambient[-1,]
    
    #  /// define constants - for Enquist Tent///
    vol = 2.197   # m^3, tent volume
    area = 1.69   # m^2, tent area
    R = 8.314472 	# J/mol K
    
    ## Define vectors to work with
    
    ## The data files are currently being read into R as factors, hence the awkward as.numeric(as.character()) class coercion.
    time <- as.numeric(as.character(input[,1])) #s
    co2 <- as.numeric(as.character(input[,8])) #umol/mol
    h2o <- as.numeric(as.character(input[,12])) #mmol/mol
    par <- as.numeric(as.character(input[,4])) # don't think there's any data here
    press <- as.numeric(as.character(input[,3])) #kPa
    temp <- as.numeric(as.character(input[,2])) #C
    
    #  /// average T and P per measurement ///
    tav <- mean(temp)
    pav <- mean(press)
    wav <- mean(h2o)
    
    ## What is cprime?
    
    #ambient
    cprime <- co2/(1-(h2o/1000))
    camb <- mean(as.numeric(as.character(ambient[,8])))
    
    ## I think this is just the ideal gas law, but not clear to me why ideal gaw law is necessary for converting from whatever the old units were (find out and enter) to the new units.
    
    camb <- camb*R*(tav+273.15)/(44*pav)    # change to umol/mol or ppm
    camb
    
    #  /// Plotting the CO2 vs. Time Curve //
    
    plot(cprime~(time), main = filename)
    
    ## Queery user for start time for fitting.  Default is set to 10 in the if() statement
    tstart <- readline("Enter preferred start time for fitting. \n Do not include units. \n Round to nearest integer second. \n Do not use 0. \n  If default of 10s is preferred, press 'return':")
    if(!grepl("^[0-9]+$", tstart)){
      tstart <- 10
    }
    tstart <- as.integer(tstart)
    
    ## Queery user for finish time for fitting.  Default is set to 80 in the if() statement
    tfinish <- readline("Enter preferred finish time for fitting. \n Do not include units. \n Round to nearest integer second. \n  If default of 80s is preferred, press 'return':")
    if(!grepl("^[0-9]+$", tfinish)){
      tfinish <- 80
    }
    tfinish <- as.integer(tfinish)
    
    ## Linear fitting code.

    linear.fit <- lm(cprime[tstart:tfinish]~(time[tstart:tfinish]))

    ## AIC value for linear model for later comparison with non-linear model.
    aic.lm <- AIC(linear.fit)

    # Calculate intercept
    inter<- as.numeric(linear.fit$coeff[1])
    
    # Calculate slope
    dcdt <- as.numeric(linear.fit$coeff[2])
    
    # Calculate r-squared (we're not reporting chi-squared significance from the non-linear fit, so this may not be so necessary)
    rsqd <- summary(linear.fit)$r.sq
    
    # Calculate nee from linear model
    nee_lm <- -(vol*pav*(1000-wav)*dcdt) / (R*area*(tav + 273.15))	# in umol/m2/s
    
    # Make plot of line.
    abline(inter,dcdt, col=6)
    
    ## Non-linear fitting code.
    # Set cnot to the actual first value of cprime used in the fitting time domain.
    cnot = cprime[tstart]
    
    # Define a temporary data frame from which the functional variables come from.
    df = data.frame(cprime, time)
    
    # Define a subset category from the tstart and tfinish variables.
    subsettime <- time > tstart & time < tfinish
    
    # Define boundaries of parameter grid.
    strt <- data.frame(A = c(150, 850), B = c(0, 1000))
    
    # Use nls2() to scan through parameter grid, searching for "best" actual starting points.  control variable is set to prevent warnings from ending loop.
    optimize.start <- nls2(cprime ~ (cnot - A)*exp(-time/B) + A, data = df, start=strt, subset = subsettime, algorithm = "brute-force", control = nls.control(warnOnly = TRUE)) #(A=375, B=40)
    
    # Run nls() with the optimized starting values from previous nls2().  Control variable is set to prevent warnings from ending loop.  However, they will still be printed at end of run.  When this happens, it is indicative of the fact that the function parameters (A and B) are large (non-physical) for the fitting, yet still produce a fit.  This is worth further investigation.  However, it appears that the nee value produced by the exponential model in such circumstances does not deviate from the linear model by much more than half a percent.  Add a "trace = TRUE" parameter setting to the nls() function to be able to watch the values of A and B change with each iteration.
    uptake.fm <- nls(cprime ~ (cnot - A)*exp(-time/B) + A, data = df, start = coef(optimize.start), subset = subsettime, control = nls.control(warnOnly = TRUE))

    ##
    sigma <- summary(uptake.fm)$sigma

    ## AIC value for non-linear model for later comparison with linear model.
    aic.nlm <- AIC(uptake.fm)
    
    Css = summary(uptake.fm)$param[1]  
    tau = summary(uptake.fm)$param[2]
    nee_exp <- ((camb-Css)/(area*tau))*(vol*pav*(1000-wav)/(R*(tav + 273.15))) #equation 4 in Saleska 1999
    
    curve((cnot - Css)*exp(-(x-time[tstart])/tau) + Css, col = 4, add = TRUE)	#equation 3 in Saleska 1999 to plot for visual inspection.##
    
    
    ## nee_lm formula produces a negative value of nee_lm, but we are interested in PLANT uptake, so the minus sign must be coerced to positive when saving the values to spreadsheet via the final paste() function call at end of script.
    
    time <- unlist(strsplit(filename, "_"))[3]
    
    if(length(grep("resp", filename, ignore.case = TRUE, value = FALSE)) == 1){
      time <- paste(time, "resp", sep = "")
    }
    
    print(data.frame("tstart" = tstart, "tfinish" = tfinish, "time" = time, "nee_lm" = nee_lm, "nee_exp" = nee_exp, "rsqd" = rsqd, "sigma" = sigma, "aic.lm" = aic.lm, "aic.nlm" = aic.nlm))
    replicate <- readline("Would you like to redo the fitting with \n a different time domain? (y/n)")
    if(replicate == "y"){
      nee.fit(filename)
    } else {
      return(c(tstart,tfinish,time,camb,tav,pav,wav,nee_lm,nee_exp,rsqd,sigma,aic.lm, aic.nlm))
    }
  }
  
  stats.df <- c()
  
  for (i in 1:length(photo.names)){
    stats.df <- rbind(stats.df, nee.fit(photo.names[i]))
    
  }
  
  if (length(resp.names) > 1){
    for (i in 1:length(resp.names)){
      stats.df <- rbind(stats.df, nee.fit(resp.names[i]))
    }
  }
  
  stats.df <- as.data.frame(stats.df)
  names.vec <- c("tstart", "tfinish", "time", "camb", "tav", "pav", "wav", "nee_lm", "nee_exp", "LM rsqd", "non-linear sigma", "aic_lm", "aic_nlm")
  for(i in 1:length(names.vec)){
    names(stats.df)[i] <- names.vec[i]
  }

stats.df
write.csv(stats.df, file = paste(paste(strsplit(getwd(), "/")[[1]][length(strsplit(getwd(), "/")[[1]])], "summary", sep = " "), ".csv", sep = ""))

}