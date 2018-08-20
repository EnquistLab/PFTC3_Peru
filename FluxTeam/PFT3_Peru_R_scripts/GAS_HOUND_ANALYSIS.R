library("proto")
library("nls2")

soil_r_fit <- function(filename)
{
print(filename)
input <- read.table(filename, header = FALSE, skip = 2)
sr_vars <- c("Date", "Time", "CO2ppm", "H2Oppt", "H2O_Temp", "Cell_Temp", "Cell_Pressure", "CO2_Absorption", "H2O_Absorption")
colnames(input) <- sr_vars

input <- as.data.frame(input)

#Extract relative time from LICOR timestamps        
time_split <- strsplit(as.character(input[,2]), split=":")
seconds <- as.numeric(unlist(lapply(time_split, FUN="[[", 3)))

minutes <- as.numeric(unlist(lapply(time_split, FUN="[[", 2)))
first_minute = minutes[1]

hours <- as.numeric(unlist(lapply(time_split, FUN="[[", 1)))
first_hour = hours[1]

#Normalize by the first seconds value to peg relative time at zero
add_minutes <- ((hours - first_hour)*60)
add_seconds <- 60 * (minutes + add_minutes - first_minute)
first_second = seconds[1]
time <- seconds + add_seconds - first_second

co2 <- as.numeric(as.character(input[,3])) #umol/mol
h2o <- as.numeric(as.character(input[,4])) #mmol/mol
press <- as.numeric(as.character(input[,7])) #kPa
temp <- as.numeric(as.character(input[,6])) #C

#avg_height = mean(collar_heights)
avg_height = 8

#Need to get collar height data
area = (5.25^2) * pi
vol = area * avg_height
R = 8.314472 	# J/mol K
   
#  /// average T and P per measurement ///
tav <- mean(temp)
pav <- mean(press)
wav <- mean(h2o)
    
cprime <- co2/(1-(h2o/1000))

plot(cprime~(time), main = filename)
## Query user for start time for fitting.  Default is set to 10 in the if() statement
tstart <- readline("Enter preferred start time for fitting. \n Do not include units. \n Round to nearest integer second. \n Do not use 0. \n  If default of 10s is preferred, press 'return':")
if(!grepl("^[0-9]+$", tstart))
{
	tstart <- 10
}
tstart <- as.integer(tstart)

## Queery user for finish time for fitting.  Default is set to 80 in the if() statement
tfinish <- readline("Enter preferred finish time for fitting. \n Do not include units. \n Round to nearest integer second. \n  If default of 80s is preferred, press 'return':")
if(!grepl("^[0-9]+$", tfinish))
{
	tfinish <- 80
}
tfinish <- as.integer(tfinish)

## Linear fitting code
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
        
## nee_lm formula produces a negative value of nee_lm, but we are interested in PLANT uptake, so the minus sign must be coerced to positive when saving the values to spreadsheet via the final paste() function call at end of script.
time <- unlist(strsplit(filename, "_"))[5]
    
if(length(grep("resp", filename, ignore.case = TRUE, value = FALSE)) == 1)
{
      time <- paste(time, "resp", sep = "")
}
    
print(data.frame("tstart" = tstart, "tfinish" = tfinish, "time" = time, "nee_lm" = nee_lm, "rsqd" = rsqd, "aic.lm" = aic.lm))
replicate <- readline("Would you like to redo the fitting with \n a different time domain? (y/n)")
if(replicate == "y"){
      soil_r_fit(filename)
    } else {
#!!!!!!
return(c(tstart,tfinish,time,tav,pav,wav,nee_lm,rsqd,aic.lm))
    }
}





