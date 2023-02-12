# create a data frame for each mode of calculation of optimum N
N450      <- as.data.frame(dt[["N450"]])
UL        <- as.data.frame(dt[["UL"]])
fao.dose  <- as.data.frame(dt[["fao.dose"]])
fao.yield <- as.data.frame(dt[["fao.yield"]])
opt       <- as.data.frame(dt[["opt"]])
opt.N     <- as.data.frame(dt[["opt.N"]])
opt.gpc   <- as.data.frame(dt[["opt.gpc"]])
opt.gpc.N <- as.data.frame(dt[["opt.gpc.N"]])

#---------
# Boxplots
# function
#---------

col.t      <- c("black","red","blue","green4")

do.boxplot <- function(df,y,y.lab){ # df = data ; y="Yield" ; y.lab="Grain yield at optimal\nN fertilizer dose (t/ha)"
  ymax <- quantile(df[,y],na.rm=T,probs=0.999)
  d <- df[df$trait=="N" & df$rcp=="00", ]
  boxplot(d[, y] ~ d[, "site"],
          ylab=y.lab,
          ylim=c(0,ymax),
          col=NA,
          outline=FALSE)
  index <- 0
  for(i in c("00","45","85"))
  { # i = "85"
    index <- index + 1
    d <- df[df$trait=="T" & df$rcp==i, ]
    boxplot(d[, y] ~ d[,"site"],
            add=TRUE,
            border=col.t[index+1],
            ann=FALSE,
            col=NA,
            boxwex=0.5,
            outline=FALSE)
  }
  legend("topleft",legend=c("N Baseline", "T Baseline", "T RCP4.5", "T RCP8.5"), bty="n", fill=col.t)
  if(y%in%c("GPC")) abline(h=11)
}

#---------------------
# Function to call the 
# function do.boxplot
#---------------------

call.do.boxplot <- function(data, is.opt, name.pdf){
  pdf(paste0(DataAnalysis.wd,"Boxplot - optimum N/", name.pdf))
  par(mfrow=c(3,1),mar=c(3, 7, 2, 2), tck=0.02,las=1)
    do.boxplot(data,   "Yield",     "Grain yield (t/ha)")
    do.boxplot(data,   "CroN.ma",   "Final crop N (kg N/ha)")
    if(is.opt == "T")
    {
      do.boxplot(N450,  "CroN.ma",   "Final crop N for 450 kg N/ha\nN fertilizer (kg N/ha)")
      do.boxplot(UL,   "CroN.ma",   "Final crop N for unlimited N (kg N/ha)")
    }
    do.boxplot(data,   "dose",      "Optimal N fertilizer rate\n(kg N/ha)")
    do.boxplot(data,   "GPC",       "Grain protein concentration(kg DM/ Kg N)")
    do.boxplot(data,   "NUE",       "N use efficiency (kg N/ha)")
    do.boxplot(data,   "ferti.eff", "N Fertilizer efficiency (kg DM/ Kg N))")
  dev.off()
}

# Plot optimal N for yield
#-------------------------
call.do.boxplot(data = as.data.frame(dt[["opt"]]), is.opt = "T", name.pdf = "Opti_N_rate_for_yield.pdf")

# Plot optimal N for yield with GPC threshold
#--------------------------------------------
call.do.boxplot(data = as.data.frame(dt[["opt.gpc"]]), is.opt = "F", name.pdf = "Opti_N_rate_for_yield_gpc.pdf") 

# Plot optimal N for yield for std cultivar
#------------------------------------------
call.do.boxplot(data = as.data.frame(dt[["opt.N"]]), is.opt = "F", name.pdf = "opti_N_rate_for_yield_for_st_cv.pdf") 

# Plot optimal N for yield with GPC threshold for std cultivar
#-------------------------------------------------------------
call.do.boxplot(data = as.data.frame(dt[["opt.gpc.N"]]), is.opt = "F", name.pdf = "opti_N_rate_for_yield_GPC_for_std_cv.pdf") 

#----------------------
# Calculate % changes
# in response to traits
#----------------------

calculate.pc.trait <- function(data){
  pc.trait <- data[data$trait=="N",] 
  pc.trait[,c(8:ncol(pc.trait))] <- NA
  pc.trait <- pc.trait[,-grep("trait",names(pc.trait))]
  
  t <- data[data$trait=="T",]  ;  t <- t[order(t$model.site.rcp.gcm),]
  n <- data[data$trait=="N",]  ;  n <- n[order(n$model.site.rcp.gcm),]
  
  for(i in 7:ncol(pc.trait))   #   i = 26
  { 
    if(names(pc.trait)[i] %in% c("GPC"))
    {
      pc.trait[,i] <- t[,i+1] - n[,i+1]
      if(length(which(is.infinite(pc.trait[,i]))) != 0) pc.trait[which(is.infinite(pc.trait[,i])), i] <- NA
    }else
    {
      pc.trait[,i] <- 100 * ( t[,i+1] - n[,i+1] ) / n[,i+1]
      if(length(which(is.infinite(pc.trait[,i]))) != 0) pc.trait[which(is.infinite(pc.trait[,i])), i] <- NA
    }
  }
  return(pc.trait)
}

pc.trait.N450      <- calculate.pc.trait(data=N450)
pc.trait.UL        <- calculate.pc.trait(data=UL)
pc.trait.fao.dose  <- calculate.pc.trait(data=fao.dose)
pc.trait.fao.yield <- calculate.pc.trait(data=fao.yield)
pc.trait.opt       <- calculate.pc.trait(data=opt)
pc.trait.opt.N     <- calculate.pc.trait(data=opt.N)
pc.trait.opt.gpc   <- calculate.pc.trait(data=opt.gpc)
pc.trait.opt.gpc.N <- calculate.pc.trait(data=opt.gpc.N)

do.boxplot.pc <- function(df,y,y.lab){ # df = data ; y="dose" ; y.lab="Grain yield at optimal\nN fertilizer dose (t/ha)"
  ymax <- quantile(df[,y],na.rm=T,probs=0.999)
  ymin <- quantile(df[,y],na.rm=T,probs=0.001)
  boxplot(df[df$rcp=="00", y] ~ df[df$rcp=="00", "site"],
          ylab=y.lab,
          ylim=c(ymin,ymax),
          col=NA,
          outline=FALSE)
  index <- 0
  for(i in c("45","85"))
  { # i = "85"
    index <- index + 1
    boxplot(df[df$rcp==i, y] ~ df[df$rcp==i,"site"],
            add=TRUE,
            border=col.t[index+1],
            ann=FALSE,
            col=NA,
            boxwex=0.5,
            outline=FALSE)
  }
  legend("topleft",legend=c("Baseline", "RCP4.5", "RCP8.5"), bty="n", fill=col.t)
  abline(h=0)
}

call.do.boxplot.pc <- function(data, is.opt, name.pdf){
  pdf(paste0(DataAnalysis.wd,"Boxplot - optimum N/", name.pdf))
  par(mfrow=c(3,1),mar=c(3, 7, 2, 2), tck=0.02,las=1)
  do.boxplot.pc(data,   "Yield",     "Grain yield (t/ha)")
  do.boxplot.pc(data,   "CroN.ma",   "Final crop N (kg N/ha)")
  if(is.opt == "T")
  {
    do.boxplot.pc(N450,  "CroN.ma",   "Final crop N for 450 kg N/ha\nN fertilizer (kg N/ha)")
    do.boxplot.pc(UL,    "CroN.ma",   "Final crop N for unlimited \nN fertilizer (kg N/ha)")
  }
  do.boxplot.pc(data,   "dose",      "Optimal N fertilizer rate\n(kg N/ha)")
  do.boxplot.pc(data,   "GPC",       "Grain protein concentration(kg DM/ Kg N)")
  do.boxplot.pc(data,   "NUE",       "N use efficiency (kg N/ha)")
  do.boxplot.pc(data,   "ferti.eff", "N Fertilizer efficiency (kg DM/ Kg N))")
  dev.off()
}

# Plot optimal N for yield
#-------------------------
call.do.boxplot.pc(data = pc.trait.opt, is.opt = "T", name.pdf = "Opti_N_PercentChange_rate_for_yield.pdf")

# Plot optimal N for yield with GPC threshold
#--------------------------------------------
call.do.boxplot.pc(data = pc.trait.opt.gpc, is.opt = "F", name.pdf = "Opti_N_PercentChange_rate_for_yield_gpc.pdf") 

# Plot optimal N for yield for std cultivar
#------------------------------------------
call.do.boxplot.pc(data = pc.trait.opt.N, is.opt = "F", name.pdf = "opti_N_rate_PercentChange_for_yield_for_st_cv.pdf") 

# Plot optimal N for yield with GPC threshold for std cultivar
#-------------------------------------------------------------
call.do.boxplot.pc(data = pc.trait.opt.gpc.N, is.opt = "F", name.pdf = "opti_N_rate_PercentChange_for_yield_GPC_for_std_cv.pdf") 


#------------------------------------------
# Compare Crop N demand estimated either
# as Crop N at optimal yield or as Crop N
# with with unlimited N fertilizer (step 1)
#------------------------------------------

pdf(paste0(main.wd , "Data_Analysis\\Boxplot - optimum N\\", "cropN_opt_vs_cropN_unlimited.pdf"))
par(mfrow=c(3,1),mar=c(3, 7, 2, 2), tck=0.02,las=1)

# Boxplot of crop N with unlimied N (step 1)
boxplot(UL[UL$trait=="N" & UL$rcp=="00", "CroN.ma"] ~ UL[UL$trait=="N" & UL$rcp=="00", "site"],
        ylab="Final crop N (kgN/ha)", 
        ylim=c(0,max(UL[, "CroN.ma"],na.rm=T)),
        col=NA,
        outline=FALSE,
        main="(Step 1 - unlimited N supply for baseline)")

boxplot(UL[UL$trait=="T" & UL$rcp=="00", "CroN.ma"] ~ UL[UL$trait=="T" & UL$rcp=="00", "site"],
        add=TRUE,
        border="red",
        ann=FALSE,
        col=NA,
        boxwex=0.5,
        outline=FALSE)

legend("topleft", legend=c("Standard cultivar", "Very high yielding cultivar"), bty="n", fill=c("black","red"))

# Boxplot of in optimal crop N estimated from N response curves
boxplot(opt.gpc[opt.gpc$trait=="N" & opt.gpc$rcp=="00", "CroN.ma"] ~ opt.gpc[opt.gpc$trait=="N"& opt.gpc$rcp=="00", "site"],
        ylab="Optimal crop N (kgN/ha)",
        ylim=c(0,max(opt.gpc["CroN.ma"],na.rm=T)),
        col=NA,
        outline=FALSE,
        main="(Step 2 - at N dose that maximize yield with a minimum\ngrain protein concentration of 11% for baseline)")

boxplot(opt.gpc[opt.gpc$trait=="T"& opt.gpc$rcp=="00", "CroN.ma"] ~ opt.gpc[opt.gpc$trait=="T"& opt.gpc$rcp=="00", "site"],
        add=TRUE,
        border="red",
        ann=FALSE,
        col=NA,
        boxwex=0.5,
        outline=FALSE)

legend("topleft", legend=c("Standard cultivar", "Very high yielding cultivar"), bty="n", fill=c("black","red"))

# Boxplot of % change in optimal cropN estimated from N response curves and crop N with unlimied N (step 1)
boxplot(pc.trait.opt[pc.trait.opt$rcp=="00","CroN.ma"] ~ pc.trait.opt[pc.trait.opt$rcp=="00","site"],
        ylab="% change in final crop N",
        col=NA,
        outline=FALSE,
        main="(Step 2 - at N dose that maximize yield with a minimum\ngrain protein concentration of 11%))")

boxplot(pc.trait.UL[pc.trait.UL$rcp=="00","CroN.ma"] ~ pc.trait.UL[pc.trait.UL$rcp=="00","site"],
        add=TRUE,
        border="red",
        ann=FALSE,
        col=NA,
        boxwex=0.5,
        outline=FALSE)

legend("topright", legend=c("Optimal N rate", "Unlimited N"), bty="n", fill=c("black","red"))
abline(h=0)

# Boxplot of the difference between Crop N for unlimited N supply and N supply for optimum yield
pc.trait.opt$diff.Opti.croN <- NA
pc.trait.opt$diff.Opti.croN <- pc.trait.UL$CroN.ma - pc.trait.opt$CroN.ma 

boxplot(pc.trait.opt[pc.trait.opt$rcp=="00", "diff.Opti.croN"] ~ pc.trait.opt[pc.trait.opt$rcp=="00", "site"],
        ylab="Crop N for unlimited N - crop N\nfor optimum yield (kg N/ha)",
        outline=FALSE)
abline(h=0)

# RRMSE of optimal crop N estimated from N response curves and crop N with unlimited N
rrmse.croN <- data.frame(site=1:34, rrmse=NA)
for(i in 1:34)  # i = 1
{ 
  rrmse.croN[rrmse.croN$site==i,"rrmse"] <- 100 * sqrt( mean(
        ( UL[UL$site==i & UL$rcp=="00", "CroN.ma"] - opt.gpc[opt.gpc$site==i & opt.gpc$rcp=="00", "CroN.ma"] )^2, na.rm=T) ) /
    mean(opt.gpc[opt.gpc$site==i & opt.gpc$rcp=="00", "CroN.ma"],na.rm=T)
}
overall.rrmse <- round(100*sqrt(mean((UL[UL$rcp=="00","CroN.ma"] - opt.gpc[opt.gpc$rcp=="00","CroN.ma"])^2, na.rm=T)) / 
                         mean(opt.gpc[opt.gpc$rcp=="00","CroN.ma"],na.rm=TRUE),1)

plot(rrmse.croN[,"rrmse"] ~ rrmse.croN[,"site"],
     ylab="RRMSE for optimum crop N (%)",
     ylim=c(0,max(rrmse.croN[,"rrmse"],na.rm=T)),
     main = paste0("overall RRMSE = ", overall.rrmse, "%"))

dev.off()

#-------------------------------
# Comparison of yield and crop N
# with unlimited N (Step 1) and
# optimal N (Step 2)
#-------------------------------

# Add a column with color value per model
model <- unique(opt$model)
mcol <- data.frame(model=model, mcol=1:length(model))
opt$mcol <- mcol$mcol[match(opt$model, mcol$model)]

# Plot function
do.plot <- function(xy, nplot, line1.1, var, lab, unit){  
  # xy="T" ; nplot=1 ; line1.1="T"; xvar="gpc" ; yvar="gpc.UL" ; xlab="Grain protein concentration" ; xunit="(%)" ; ylab="Grain protein concentration" ; xyunit="(%)"
  for(i in 1:nplot)   # i=1
  {
    if(xy == "T")
    {
      xrange <- range(opt[,var[i]], UL[,var[i]], na.rm=T)
      yrange <- xrange
    }
    if(xy == "F")
    {
      xrange <- range(OptiN00[, xvar[i]], na.rm=T)
      yrange <- range(OptiN00[, yvar[i]], na.rm=T)
    }
    plot(opt[,var[i]],
         UL[,var[i]],
         xlim=xrange,
         ylim=yrange,
         xlab=paste0(lab[i], " with\noptimal N fertilizer rate ", unit[i]),
         ylab=paste0(lab[i], " with\nunlimited N ",               unit[i]),
         pch="")
    text(opt[,var[i]],
         UL[,var[i]],
         labels=opt$model,
         col=opt$mcol,
         cex=0.5)
    abline(lm(UL[,var[i]] ~opt[,var[i]]))
    if(line1.1=="T")  abline(0, 1, lty=2)
  }
}

pdf(paste0(main.wd , "Data_Analysis\\Boxplot - optimum N\\", "yield CropN GPC - Optimum N vs unlimited N.pdf"))

# Plot yield and crop N at optimal N estimated from N response curves vs crop N with unlimited N
#-----------------------------------------------------------------------------------------------
par(mfrow=c(2,2), mar=c(5, 4, 4, 2) + 0.1, tck=0.02,las=1,pty="s")

do.plot(xy = "T", nplot  = 3, line1.1="T",
        
        var  = c("Yield",   "CroN.ma",  "GPC"),
        lab  = c("Grain yield", "Final crop N", "Grain protein concentration"),
        unit = c("(t/ha)",      "(kg N/ha)",    "(%)"))
dev.off()

rm(rrmse.croN,overall.rrmse, col.t)
