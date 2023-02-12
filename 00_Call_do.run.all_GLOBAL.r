# ------------------
# Clean environment
# ------------------

rm(list=ls())
gc()

#------------
# Directories
#------------

# Root
main.wd <- c("C:/Users/martrepi/OneDrive/1-Pierre/3-Projets/En cours/AgMIP/AgMIP_4/agmip_phase4_globalsites/")
# R Scripts
rscript.wd <- paste0(main.wd, "R scripts2/") 
# In comming
incoming.wd <- paste0(main.wd, "Incoming Data/")
# Data analysis
DataAnalysis.wd <- paste0(main.wd, "Data_Analysis/") 
# Experimental datasets
ExpDatasets.wd <- paste0(main.wd, "ExpDatasets/") 

# ------------------------------
# automatically install (if not
# already installed) and load
# required libraries 
# ------------------------------

source(paste(rscript.wd , "Snipet\\automatically_install_package.r" , sep = ""))
install_package(pack = c("doBy", "readbulk", "data.table", "jpeg", "ggplot2", "sf", "gridExtra","cowplot",
                         "rnaturalearth", "rnaturalearthdata", "rgeos", "ggrepel", "zoo", "dplyr", "tidyr",
                         "readxl"))

#---------------
# Source snippet
# scripts
#---------------

source(paste0(rscript.wd, "Snipet\\addImg.r")) # add an image in a plot
source(paste0(rscript.wd, "Snipet\\root finding for linear and spline interpolation.r"))
source(paste0(rscript.wd, "Snipet\\moveme.r")) # move columns

#-----------
# Parameters
#-----------

# model(s) to remove (if no model to remove comment the line below)
#------------------------------------------------------------------
# Model PG removed because at several sites simulated yield showed no response to N fertilizer dose
# Model WG removed as it did not report CropN or GrainN
rm.model <- c("DS","CS", "D4", "MC", "NC", "NG", "NP", "NS", "S2", "SA", "SP", "WO", "PG", "WG")

# Model for which crop N, grain N, and GPC are not considered (if no model to remove comment the line below)
#----------------------------------------------------------------------------------------------------------
#             # Models which do not simulate Grain N
 rm.gpc <- c("AQ","CS", "MC", "SP", "WG", "WO",
#             # Models which assume a constant grain protein concentration
             "LI", "L6", "NS", "SS")

# Variable to keep in the analysis
#---------------------------------
# all variables
# var.rearrange <- c("MaxLAI", "cumPARi", "CumET", "Transp", "FLN", "se", "sa", "ea", "segf", "eegf", "aegf", "Biom.an",
#                    "Biom.ma", "Yield", "HI", "CroN.an", "CroN.ma", "GrainN", "nhi", "GNumber", "GDW", "GNC", "GPC",
#                    "SoilAvW", "WDrain", "Runoff", "SoilN", "Nmin", "Nvol", "Nimmo",  "Nden", "Nleac")

var.rearrange <- c("sa", "segf", "Yield", "MaxLAI", "cumPARi", 
                   "Biom.ma", "Biom.an", "CroN.ma", "CroN.an", "GrainN", "GPC", "HI", "GNumber", 
                   "Nleac", "Nmin", "Nvol", "Nimmo", "SoilN", "Nden",
                   "NUE", "Nav", "NUtE", "NUpE", "NNI.an")

# Parameters for plotting
#------------------------
mycol <- data.frame(col=c("darkgoldenrod4","darkmagenta"), row.names =c("m","s"), stringsAsFactors = FALSE)

# Parameters for fitting N response curves
#-----------------------------------------
f.opti     <- 0.9   # Fraction of maximum simulated yield at which optimum yield is calculated (0.975; 0.95)
target.gpc <- 12    # Minimum target grain protein concentration (GPC) (11%, 12%)
gmc        <- 0.13  # Grain moisture content

#-----------------------
# step(s) to analyze
# ("Step01", "Step02",
# and/or "Step_test")
#-----------------------

step <- c("Step02")  

# -----------------------
# Source selected scripts 
# in do.run.all.r
# -----------------------
start_time <- Sys.time()

  for(s in step)
  {
    Step <- s
    
    if(Step == last(step)) { endLoopStep <- "T" } else { endLoopStep <- "F" }
    
    # Selectt the scripts to execute
    merge.models       <- "F"  # Set to "T" to merge all daily and summary simulation .txt files in the 'In coming' folder
    rearrange.sim      <- "F"  # Set to "T" to rearrange merged simulation .txt files and calculate extra variables
    CalOptiCropN       <- "F"  # Set to "T" to calculate Crop N that maximize yield for each treatment and model
    boxplots           <- "F"  # Set to "T" to do boxplot and scatter plots of plot yield and GPC for all models and sites
    PlotNResponseCurve <- "F"  # Set to "T" to plot N reponse curve per site for each model
    PlotOptiN          <- "F"  # Set to "T" to plot optimum N boxplots
    PlotSimVsFAOYield  <- "F"  # Set to "T" to plot simulated yield at national N application rate vs. FAO national yield
    CalPercentChange   <- "F"  # Set to "T" to calculate percent change (climate change impact) and adaptation for each treatment and model
    boxplotOptiNAbsval <- "F"  # Set to "T" to plot boxplot of absolute value of multiple variables  per site at optimum N
    boxplotOptiN.pc    <- "F"  # Set to "T" to plot boxplot of percent change of of multiple variables  per site at optimum N
    mapAbsYield        <- "T"  # Set to "T" to plot maps
    mapPCYield         <- "F"  # Set to "T" to plot maps
    PlotGPCvsGYNUtE    <- "F"  # Set to "T  to plot grain protein versus grain yield and NUtE
    SoilChar34sites    <- "F"  # Set to "T  to plot soil characteristics at the 34 global sites 
    
    source(paste(rscript.wd , "\\01_do.run.all.r" , sep = ""))
  }

end_time <- Sys.time()
print(paste0("That's all folks! - all scripts were executed in:"))
print(end_time - start_time)