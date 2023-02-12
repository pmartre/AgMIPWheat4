###################################################################################################################
####################################   Merge model output txt  files     ##########################################
###################################################################################################################

if(merge.models == "T")
{
    print("Start - Merge files")
  
    source(paste0(rscript.wd , "merge models.r"))
  
    print("End - Merge files")
}

###################################################################################################################
#######################################     Rearrange merged simulation files    ##################################
###################################################################################################################

if(rearrange.sim == "T")   
{
  print("Start - rearrange")
  
  source(paste0(rscript.wd , "rearrange.r"))
  
  print("End - rearrangee")
}

###################################################################################################################
################################     Read data and combine Step 1 and 2    ########################################
###################################################################################################################

if(endLoopStep == "T")
{
  print("Start - Reading rearranged data")
  
  # Step 1
  data.s1     <- readRDS(paste0(DataAnalysis.wd, "merged summary_Step01(formated).RDS"))
  data.30y.s1 <- readRDS(paste0(DataAnalysis.wd, "30y.mean_Step01.RDS"))
  
  # Step 2
  data     <- readRDS(paste0(DataAnalysis.wd, "merged summary_Step02(formated).RDS"))
  data.30y <- readRDS(paste0(DataAnalysis.wd, "30y.mean_Step02.RDS"))
  
  # Combine Step 1 and Step 2
  data.s1$step  <- 1
  data.s1$ferti <- -999 
  data.s1 <- data.s1[moveme(names(data.s1), "ferti after rcp")]
  data$step <- 2
  data <- rbind(data.s1, data)
  data <- data[moveme(names(data), "step after model")]
  rm(data.s1)
  
  data.30y.s1$step  <- 1
  data.30y.s1$ferti <- -999 
  data.30y.s1 <- data.30y.s1[moveme(names(data.30y.s1), "ferti after rcp")]
  data.30y$step <- 2
  data.30y <- rbind(data.30y.s1, data.30y)
  data.30y <- data.30y[moveme(names(data.30y), "step after model")]
  rm(data.30y.s1)
  
  print("End - Reading rearranged data")
}

###################################################################################################################
####################################     Calculate crop N at optimum yield    #####################################
###################################################################################################################

if(CalOptiCropN == "T" & Step == "Step02")
{
  print(paste("Start - Calculating Crop N at optimum yield", Step))
  
  # Calculate for 30 years mean results
  ave.30 <- "T"
  dt <- data.30y
  dt <- dt[-which(dt$model%in%c("e.median","q.25","q.75")),]
  # Select response variables for which to calculate the values at the target N dose or yield
  VAR <- c("Yield", "MaxLAI", "cumPARi","HI", "GNumber","CroN.ma", "NNI.an", "GPC", "NUE", "NUtE", "NUpE", "Nav", "SoilN", "Nmin", "Nimmo", "Nvol", "Nleac", "Nden")
  source(paste0(rscript.wd , "Calculate optimal N_v2.r"))
  rm(VAR, ave.30, dt)

  print(paste("End - Calculating Crop N at optimum yield", Step))
}

###################################################################################################################
#######################################     Box plots - preliminary analysis    ###################################
###################################################################################################################

if(boxplots == "T" & Step == "Step01")
{
  print(paste("Start - plot preliminary analysis boxplot", Step))
  
  # Plot Step 1
  s <- "Step 1"
  dt     <- data[data$step==1,]
  dt.30y <- data.30y[data.30y$step==1,]
  var.to.plot <- c("Yield",                   "CroN.ma",            "MaxLAI",                 "cumPARi",                            "HI",                       "GNumber",                  "GPC",                                             "sa",                               "segf")
  y.lab       <- c("Grain yield (t/ha)",      "Crop N (kg N/ha)",   "Maximum LAI (m²/m²)",    "Season cumulative PARi (MJ/m²)",     "Harvest index (-)",         "Grain number (grain/m²)", "Grain protein concentration (%)",                 "Sowing to anthsis (days)",         "Sowing to maturity (days)")
  y.lab.pc    <- c("% change in grain yield", "% change in crop N", "%change in maximum LAI", "% change in season cumulative PARi", "% change in harvest index", "% change in grain number","Change in grain protein concentration (% point)", "Change in days to anthsis (days)", "Change in days to maturity (days)")
  cex.m = 0.6
  source(paste0(rscript.wd , "do.preliminary.analysis.boxplot.r"))
  rm(dt, dt.30y, var.to.plot, y.lab,  y.lab.pc, cex.m)
  
  # Read Simulation interpolated on the Yield vs. N dose relationship
  # res.opt     <- readRDS(paste0(DataAnalysis.wd,  "CropNtoMaximizeYield.RDS"))
  res.opt.30y <- readRDS(paste0(DataAnalysis.wd,  "CropNtoMaximizeYield_30yrsAve.RDS"))
  
  # Plot Step 2 (at optimum N does for yield)
  s <- "Step 2 (optimum N)"
  dt     <- as.data.frame(res.opt[["opt"]])
  dt.30y <- as.data.frame(res.opt.30y[["opt"]])
  var.to.plot <- c("Yield",                   "GPC",                                             "CroN.ma",            "NUE",                            "NUtE",                                   "NUpE",                             "ferti.eff",                          "NNI.an",                       "MaxLAI",                 "cumPARi",                            "HI",                        "GNumber",                  "dose",                                  "SoilN",              "Nav",                          "Nmin",                              "Nimmo",                             "Nvol",                              "Nleac",                       "Nden")
  y.lab       <- c("Grain yield (t/ha)",      "Grain protein concentration (%)",                 "Crop N (kg N/ha)",   "N use efficiencty (kg DM/kg N)", "N utilisation efficiencty (kg N/kg DM)", "N uptake efficiencty (kg N/kg N)", "Fertilizer efficiency (kg DM/kg N)", "N nutrition index (-)",        "Maximum LAI (m²/m²)",    "Season cumulative PARi (MJ/m²)",     "Harvest index (-)",         "Grain number (grain/m²)",  "Optimum N fertilizer rate (kg N/ha)",   "Soil N (kg N/ ha)",  "Available soil N (kg N/ha)",   "Soil N mineralization (kg N/ha)",  "Soil N immobilization (kg N/ha)",   "Soil N immobilization (kg N/ha)",   "Soil N leaching (kg N/ha)",   "Soil N denitrification (kg N/ha)")
  y.lab.pc    <- c("% change in grain yield", "Change in grain protein concentration (% point)", "% change in crop N", "% change in N use efficiency",   "% change in N utilisation efficiencty",  "% change in N uptake efficiencty", "% change in Fertilizer efficiency",  "%change in N nutrition index", "%change in maximum LAI", "% change in season cumulative PARi", "% change in harvest index", "% change in grain number", "% change in optimum N fertilizer rate", "% change in soil N", "% change in available soil N", "% change in soil N mineralization", "% change in soil N immobilization", "% change in soil N immobilization", "% change in soil N leaching", "% change in  soil N denitrification")
  cex.m = 2
  source(paste0(rscript.wd , "do.preliminary.analysis.boxplot.r"))
  rm(dt, dt.30y, var.to.plot, y.lab,  y.lab.pc, cex.m)
  
  # Plot Step 2 (at optimum N dose for yield with a minimum GPC threshold)
  s <- "Step 2 (optimum N w. GPC thereshold)"
  dt      <- as.data.frame(res.opt[["opt.gpc"]])
  dt.30.y <- as.data.frame(res.opt[["opt.gpc"]])
  var.to.plot <- c("Yield",                   "GPC",                                             "CroN.ma",            "NUE",                            "NUtE",                                   "NUpE",                             "ferti.eff",                          "NNI.an",                       "MaxLAI",                 "cumPARi",                            "HI",                        "GNumber",                  "dose",                                  "SoilN",              "Nav",                          "Nmin",                              "Nimmo",                             "Nvol",                              "Nleac",                       "Nden")
  y.lab       <- c("Grain yield (t/ha)",      "Grain protein concentration (%)",                 "Crop N (kg N/ha)",   "N use efficiencty (kg DM/kg N)", "N utilisation efficiencty (kg N/kg DM)", "N uptake efficiencty (kg N/kg N)", "Fertilizer efficiency (kg DM/kg N)", "N nutrition index (-)",        "Maximum LAI (m²/m²)",    "Season cumulative PARi (MJ/m²)",     "Harvest index (-)",         "Grain number (grain/m²)",  "Optimum N fertilizer rate (kg N/ha)",   "Soil N (kg N/ ha)",  "Available soil N (kg N/ha)",   "Soil N mineralization (kg N/ha)",  "Soil N immobilization (kg N/ha)",   "Soil N immobilization (kg N/ha)",   "Soil N leaching (kg N/ha)",   "Soil N denitrification (kg N/ha)")
  y.lab.pc    <- c("% change in grain yield", "Change in grain protein concentration (% point)", "% change in crop N", "% change in N use efficiency",   "% change in N utilisation efficiencty",  "% change in N uptake efficiencty", "% change in Fertilizer efficiency",  "%change in N nutrition index", "%change in maximum LAI", "% change in season cumulative PARi", "% change in harvest index", "% change in grain number", "% change in optimum N fertilizer rate", "% change in soil N", "% change in available soil N", "% change in soil N mineralization", "% change in soil N immobilization", "% change in soil N immobilization", "% change in soil N leaching", "% change in  soil N denitrification")
  cex.m = 2
  source(paste0(rscript.wd , "do.preliminary.analysis.boxplot.r"))
  rm(dt, dt.30y, var.to.plot, y.lab,  y.lab.pc, cex.m)
  
  print(paste("End - plot preliminary analysis boxplot", Step))
}

###################################################################################################################
###########################################     Plot N response curve    ##########################################
###################################################################################################################

if(PlotNResponseCurve == "T" & Step == "Step02")
{
  print(paste("Start - Plotting N response curve", Step))
  
  dt <- data.30y[which(data.30y$step==2), ]   
  var <-   c("Yield",                 "CroN.ma",          "MaxLAI",              "cumPARi",                        "HI",                "GNumber",                 "GPC",                             "NNI.an",                            "Nav",                        "Nmin",                            "NUE",                          "NUpE",                            "NUtE") 
  y.lab <- c("Grain yield (t/ha)", "Crop N (kg N/ha)", "Maximum LAI (m²/m²)", "Season cumulative PARi (MJ/m²)", "Harvest index (-)", "Grain number (grain/m²)", "Grain protein concentration (%)", "N nutrition index at anthesis (-)", "Available Soil N (kg N/ha)", "Soil N mineralization (kg N/ha)", "N use efficiency (kg grain/kg N)", "N uptake efficiency (kg N/kg N)", "N utilization efficiency (kg grain/kg N)" )  
  y.lab <- data.frame(var, y.lab)
  source(paste0(rscript.wd , "Plot N response curve.r"))
  rm(dt, var, y.lab)
  
  print(paste("End - Plotting N response curve", Step))     
}

###################################################################################################################
################################     Plot yield, crop N, and GPC at optimum yield    ##############################
###################################################################################################################

if(PlotOptiN =="T")
{
  print(paste("Start - Plotting yield, crop N and GPC at optimum yield", Step))
  
  dt <- readRDS(paste0(DataAnalysis.wd, "CropNtoMaximizeYield_30yrsAve.RDS"))
  source(paste0(rscript.wd , "Plot optimal N.r"))
  
  print(paste("End - Plotting yield, crop N and GPC at optimum yield", Step))
}

###################################################################################################################
################     Plot simulated yield at national N application rate vs. FAO national yield    ################
###################################################################################################################
if(PlotSimVsFAOYield =="T")
{
  print(paste("Start - Plotting sim yield vs FAO country yield", Step))
  
  # Read site information and FAO country yield
  site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")
  # Read simulated yield interpolated at national fertilization rate 
  res.opt.30y <- readRDS(paste0(DataAnalysis.wd,  "CropNtoMaximizeYield_30yrsAve.RDS"))
  # Read N fertilization rate (dose) at FAO national yield
  source(paste0(rscript.wd , "Plot simulated vs FAO yields.r"))
  
  print(paste("Start - Plotting sim yield vs FAO country yield", Step))
}
###################################################################################################################
###################################     Calculate percent change and adaptation    ################################
###################################################################################################################

if(CalPercentChange=="T")
{
  print(paste("Start - Calculating percent change and adaptation", Step))
  
  # read data
  res.opt.30y <- readRDS(paste0(DataAnalysis.wd, "CropNtoMaximizeYield_30yrsAve.RDS"))
  source(paste0(rscript.wd , "Caculate percent changes and adaptation.r"))
  
  print(paste("End - Calculating percent change and adaptation", Step))
}

###################################################################################################################
#################################     Boxplot absolute value at optimum N per site    #############################
###################################################################################################################

if(boxplotOptiNAbsval == "T")
{
  print(paste("Start - Boxplot absolute value at optimum N per site", Step))
    
  # read data  
  dt <- readRDS(paste0(DataAnalysis.wd, "30y.mean_PercentageChange_Adaptation.RDS"))
  var      <- c("Yield",                   "GPC",                                             "CroN.ma",            "NUE",                            "NUtE",                                   "NUpE",                             "ferti.eff",                          "NNI.an",                       "MaxLAI",                 "cumPARi",                            "HI",                        "GNumber",                  "dose",                                  "SoilN",              "Nav",                          "Nmin",                              "Nimmo",                             "Nvol",                              "Nleac",                       "Nden")
  var.name <- c("grain yield (t/ha)",      "Grain protein concentration (%)",                 "Crop N (kg N/ha)",   "N use efficiencty (kg DM/kg N)", "N utilisation efficiencty (kg N/kg DM)", "N uptake efficiencty (kg N/kg N)", "Fertilizer efficiency (kg DM/kg N)", "N nutrition index (-)",        "Maximum LAI (m²/m²)",    "Season cumulative PARi (MJ/m²)",     "Harvest index (-)",         "Grain number (grain/m²)",  "Optimum N fertilizer rate (kg N/ha)",   "Soil N (kg N/ ha)",  "Available soil N (kg N/ha)",   "Soil N mineralization (kg N/ha)",  "Soil N immobilization (kg N/ha)",   "Soil N immobilization (kg N/ha)",   "Soil N leaching (kg N/ha)",   "Soil N denitrification (kg N/ha)")
  source(paste0(rscript.wd , "Boxplot optimal N per site.r"))
  rm(dt, var)
  
  print(paste("End - Boxplot absolute value at optimum N per site", Step))
}

###################################################################################################################
#################################     Boxplot percent change at optimum N per site    #############################
###################################################################################################################

if(boxplotOptiN.pc == "T")
{
  print(paste("Start - Boxplot percent change at optimum N per site", Step))
  
  # read data
  dt <- readRDS(paste0(DataAnalysis.wd, "30y.mean_PercentageChange_Adaptation.RDS"))
  # Variables to plot
  var      <- c("Yield",       "GPC",                         "CroN.ma", "NUE") #               "NUtE",                      "NUpE",                 "ferti.eff",               "NNI.an",            "MaxLAI",      "cumPARi",                 "HI",          "GNumber",       "dose",                      "SoilN",   "Nav",              "Nmin",                  "Nimmo",                 "Nvol",                  "Nleac",           "Nden")
  var.name <- c("grain yield", "grain protein concentration", "crop N",  "N use efficiencty") #  "N utilisation efficiencty", "N uptake efficiencty", "N fertilizer efficiency", "N nutrition index", "maximum LAI", "season cumulative PARi", "harvest index","grain number",  "optimum N fertilizer rate", "soil N",  "available soil N", "soil N mineralization", "soil N immobilization", "soil N immobilization", "soil N leaching", "soil N denitrification")
  source(paste0(rscript.wd , "Boxplot percentage optimal N per site.r"))
  rm(var)
  
  print(paste("End - Boxplot percent change at optimum N per site", Step))
}

###################################################################################################################
##########################     Boxplot percent change in global production and N demand    ########################
###################################################################################################################

if(boxplotOptiN.pc == "T")
{
  print(paste("Start - Boxplot percent change in global production and N demand", Step))
  
  # read data simulation results
  Nres <- data.30y[which(data.30y$step==2 & data.30y$model=="e.median"), ]   
  # read data % change in response to high yield traits and climate change adaptation
  dt <- readRDS(paste0(DataAnalysis.wd, "30y.mean_PercentageChange_Adaptation.RDS"))
  
  var <-   c("Yield", "NUE", "GPC", "Yield", "dose", "CroN.ma") 
  # var <-   c("Yield", "NUE", "dose") 
  # N response ot yield and NUE, and % change in global production and N demand
  title.pdf <- "Fig. 3 - N response and percentage change global production and N demand"
  source(paste0(rscript.wd , "Fig. 3 - N response and percentage change global production and N demand.r"))
  
  # Including adaptation to climate change
  title.pdf <- "Fig. 3 - N response and percentage change and adapatation global production and N demand"
  source(paste0(rscript.wd , "Boxlplot percentage change and adaptation of production and N demand.r"))
  
  print(paste("End - Boxplot percent change in global production and N demand", Step))
}

###################################################################################################################
##########################     Boxplot global yield and grain protein concentration     ###########################
###################################################################################################################

if(boxplotOptiN.pc == "T")
{
  print(paste("Start - oxplot global yield and grain protein concentration", Step))
  
  # read data % change in response to high yield traits and climate change adaptation
  dt <- readRDS(paste0(DataAnalysis.wd, "30y.mean_PercentageChange_Adaptation.RDS"))
  var <-   c("Yield", "GPC") 
  # N response ot yield and NUE, and % change in global production and N demand
  title.pdf <- "Fig. S10 - Global yield and grain protein concentration"
  source(paste0(rscript.wd , "Fig. S10 - Boxlplot yield - GPC std cultivar and DH lines.r"))
  
  print(paste("End - oxplot global yield and grain protein concentration", Step))
}

###################################################################################################################
########     Boxplot global yield, N demand and grain protein concentration adaptation to climate change     ######
###################################################################################################################

if(boxplotOptiN.pc == "T")
{
  print(paste("Start - plot global yield and grain protein concentration", Step))
  
    # read data % change in response to high yield traits and climate change adaptation
    dt <- readRDS(paste0(DataAnalysis.wd, "30y.mean_PercentageChange_Adaptation.RDS"))
    var <-   c("Yield", "Nav","GPC") 
    # N response ot yield and NUE, and % change in global production and N demand
    title.pdf <- "Fig. S11 - Global yield, N demand and grain protein concentration adaptation"
    source(paste0(rscript.wd , "Boxlplot yield - N demand - GPC adaptation to CC.r"))
  
  print(paste("End - plot global yield and grain protein concentration", Step))
}
###################################################################################################################
#####################################     Plot maps of absolute yields    #########################################
###################################################################################################################

if(mapAbsYield == "T")
{
  print(paste("Start - plotting maps of absolute yield", Step))
 
  # read data
  dt <- readRDS(paste0(DataAnalysis.wd, "30y.mean_PercentageChange_Adaptation.RDS"))
  
  # read MME results 
  step.mme.expdataset <- 2
  # Source code to rearrange experimental data set
  source(paste0(rscript.wd , "Read and rearrange experimental dataset.r"))
  step.mme.expdataset <- 2
  mme <- readRDS(paste0(ExpDatasets.wd, "MME for Expdataset_Step", step.mme.expdataset, " models.RDS"))
  rm(step.mme.expdataset)
  # Add 13% moisture content
  for(exp in c("nz","fr","va","ba","co")) # exp= "nz"
  {
    for(tt in c(3:5)) # tt = 3
    {
      mme[[exp]][tt] <- 1 / (1 - gmc) * mme[[exp]][tt] 
    }
  }
  
  title <- "Fig. 1 - Map of potential yield"
  var <-  "Yield"
  y.lim <- c(0,25) 
  leg.label <- "Water and N\nunlimited\ngrain\nyield\n(t /ha)"
  axis.title <- "Yield (t /ha)"
  source(paste(rscript.wd , "Fig. 1 - map absolute yield.r" , sep = ""))

  print(paste("End - plotting maps of absolute yield", Step))
}

###################################################################################################################
###############################     Plot grain yield vs GPC at the 34 global sites    #############################
###################################################################################################################

if(PlotGPCvsGYNUtE == "T")
{
  print(paste("Start - plotting grain protein vs grain yield and NUtE", Step))
  
    source(paste(rscript.wd , "Plot grain protein concentration vs grain yield per site.r" , sep = ""))
  
  print(paste("End - plotting grain protein vs grain yield and NUtE", Step))
}
###################################################################################################################
##############################     Plot soil characteristics at the 34 global sites    #############################
###################################################################################################################

if(SoilChar34sites == "T")
{
  print(paste("Start - plotting soil characteristics at the 34 global sites", Step))
  
  # Variables to plot
  var   <- c("slll",                                "sloc") #,                              "sksat",                                             "slbdm",                                  "slphw")
  # Variable names for axis titles
  x.lab <- c("Soil water content (mm3/mm3)",         "Soil organic c (g C/100g)") #,        "Saturated hydraulic conductivity (cm/h)",           "Apparent bulk density (g/cm3)",          "Soil pH in water (-)", "")
  # pdf file name
  pdf.name <- c("Fig S3. soil water retention.pdf",  "Fig S4. soil organic carbone.pdf") #, "Fig Sx. soil saturated hydraulic conducitvity.pdf", "Fig Sx. soil appparent bulk density.pdf", "Fig Sx. soil pH.pdf")
  
  # get site information
  site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),
                            header=TRUE,stringsAsFactors=FALSE, sep="\t")
                     
  # Read soil characteristics data
  soil <- as.data.frame(read_excel("C:/Users/martrepi/OneDrive/1-Pierre/3-Projets/En cours/AgMIP/AgMIP_4/Global sites/Distributed files/AgMIPWheat4_Soil_34_Global_sites_update2.xlsx",
                                   skip = 52,
                                   col_names=c("site","sllb","slll","sldul","slsat","slrgf","sksat","slbdm","SLOM","sloc","slni","slcly","slsil","slsnd","slphw","ICH2O","ICNH4M",
                                               "ICNH4M","AWC", "AWCprofile")))
  source(paste(rscript.wd , "Plot soil characteristics at 34 global sites.r" , sep = ""))

    print(paste("End - soil characteristics at the 34 global sites", Step))
}

