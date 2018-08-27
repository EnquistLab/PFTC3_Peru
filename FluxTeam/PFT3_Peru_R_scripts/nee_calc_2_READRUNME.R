##  This is a read me/executable file for running the nee_calc.R script for fitting LiCOR 7500 CO2 Flux data to both linear and exponential (leaky-fit) models.  For particulars on how the fits are being performed, open and read through the nee_calc.R script.  Otherwise, follow the steps listed below for executing.

## IMPORTANT!!!  BE SURE THAT YOUR .txt FILE NAMES HAVE THE FOLLOWING FORMATS!!!
## site_season_time_date_plot#.txt        or...
## site_season_time_date_plot#a.txt       for ambient, or...
## site_season_time_date_plot#resp.txt    for daytime respiration

## FIRST: Execute the nee_calc.R file in R so that the function nee.calc() exists in your Global Environment.

## SECOND: Run the next line of code below.  You will shortly be promted to set the working directory, define the starting and finishing time intervals, and finally whether or not you would like to re-run the fitting algorithms with different time intervals.  Upon completion, a table will be printed that can be copied and pastred to whatever summary spreadsheet is being used.

nee.calc()

