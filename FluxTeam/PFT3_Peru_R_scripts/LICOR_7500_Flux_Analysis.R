library("proto")
library("nls2")

nee.fit <- function(filename)
{
input <- read.table(filename, header = FALSE, skip = 10)
if(length(grep("resp", filename, ignore.case = TRUE, value = FALSE)) == 1){
   ambient <- read.table(paste(strsplit(filename, "resp.txt"), "a.txt", sep = ""), header = FALSE, skip = 10)
} else{
   ambient <- read.table(paste(strsplit(filename, ".txt"), "a.txt", sep = ""), header = FALSE, skip = 10)
}

licor_vars <- c("Relative_Time", "Temperature", "Pressure", "Aux_Input", 
	"CO2_Absorptance", "CO2_mmol/m3", "CO2_mg/m3", "CO2_umol/mol", 
	"H2O_Absorptance", "H2O_mmol/m3", "H2O_g/m3", "H2O_mmol/mol", 
	"H2O(C)", "Cooler_Voltage")
colnames(input) <- licor_vars
colnames(ambient) <- licor_vars

input <- as.data.frame(input)
ambient <- as.data.frame(ambient)

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
    
cprime <- co2/(1-(h2o/1000))
camb <- mean(as.numeric(as.character(ambient[,8])))
camb <- camb*R*(tav+273.15)/(44*pav)

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
    
## Non-linear fitting code.
# Set cnot to the actual first value of cprime used in the fitting time domain.
cnot = cprime[tstart]
    
# Define a temporary data frame from which the functional variables come from.
df = data.frame(cprime, time)
    
# Define a subset category from the tstart and tfinish variables.
 subsettime <- time > tstart & time < tfinish
    
# Define boundaries of parameter grid.
strt <- data.frame(A = c(150, 850), B = c(0, 1000))
    
# Use nls2() to scan through parameter grid, 
#searching for "best" actual starting points.  
#control variable is set to prevent warnings from ending loop.
#(A=375, B=40)
optimize.start <- 
	nls2(cprime ~ (cnot - A)*exp(-time/B) + A, data = df, start=strt, 
	subset = subsettime, algorithm = "brute-force", 
	control = nls.control(warnOnly = TRUE)) 
    
# Run nls() with the optimized starting values from previous nls2().  
# Control variable is set to prevent warnings from ending loop.  
# However, they will still be printed at end of run.  
# When this happens, it is indicative of the fact that the function parameters (A and B) are large (non-physical) for the fitting, yet still produce a fit.  
# This is worth further investigation.  
# However, it appears that the nee value produced by the exponential model in such circumstances does not deviate from the linear model by much more than half a percent.  
# Add a "trace = TRUE" parameter setting to the nls() function to be able to watch the values of A and B change with each iteration.
uptake.fm <- 
	nls(cprime ~ (cnot - A)*exp(-time/B) + A, data = df, start = coef(optimize.start), 
		subset = subsettime, control = nls.control(warnOnly = TRUE))

#the square root of the estimated variance of the random error (nls2 documentation)
sigma <- summary(uptake.fm)$sigma

## AIC value for non-linear model for later comparison with linear model.
aic.nlm <- AIC(uptake.fm)
    
Css = summary(uptake.fm)$param[1]  
tau = summary(uptake.fm)$param[2]

#equation 4 in Saleska 1999
nee_exp <- ((camb-Css)/(area*tau))*(vol*pav*(1000-wav)/(R*(tav + 273.15))) 

#equation 3 in Saleska 1999 to plot for visual inspection.##    
curve((cnot - Css)*exp(-(x-time[tstart])/tau) + Css, col = 4, add = TRUE)	
    
## nee_lm formula produces a negative value of nee_lm, but we are interested in PLANT uptake, so the minus sign must be coerced to positive when saving the values to spreadsheet via the final paste() function call at end of script.
time <- unlist(strsplit(filename, "_"))[3]
    
if(length(grep("resp", filename, ignore.case = TRUE, value = FALSE)) == 1)
{
      time <- paste(time, "resp", sep = "")
}
    
print(data.frame("tstart" = tstart, "tfinish" = tfinish, "time" = time, "nee_lm" = nee_lm, "nee_exp" = nee_exp, "rsqd" = rsqd, "sigma" = sigma, "aic.lm" = aic.lm, "aic.nlm" = aic.nlm))
replicate <- readline("Would you like to redo the fitting with \n a different time domain? (y/n)")
if(replicate == "y"){
      nee.fit(filename)
    } else {
#!!!!!!
return(c(tstart,tfinish,time,camb,tav,pav,wav,nee_lm,nee_exp,rsqd,sigma,aic.lm, aic.nlm))
    }
}





