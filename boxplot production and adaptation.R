#--------------------
# Boxplot of % change
# in yield with and
# witouth trait
# adapatation
#--------------------

x.pos <- c(1, 5, 3, 1.5, 3.5, 5.5)
icol  <- c(1, 1, 1, 2,   2,   2)

# Function for boxplot of global production
do.boxplot <- function(df, y, y.lim, y.lab, letter, main){ # df = dt ; y="Yield" ; y.lim=c(0,20) ; y.lab="% change in yield (t/ha)"; letter="B"
  
  if(letter!="A") main <- NA
  boxplot(df[, paste0(y, ".00.N")], at=1,     border=mycol[1,1], xlim=c(0.5,6), ylim=y.lim, ylab=y.lab, outline=FALSE, main=main)
  axis(1, at=c(1.25,3.25,5.5), labels=c("Baseline","RCP4.5","RCP8.5"),cex.axis=0.9)
  
  index <- 0
  for(t in c("N","T"))
  {
    for(r in c(".00.",".45.",".85."))
    { 
      index <- index + 1
      boxplot(df[, paste0(y,r,t)], at = x.pos[index], border=mycol[icol[index],1], add=TRUE, ann=FALSE, col=NA, outline=FALSE)
    }
  }
  legend("topleft",legend=paste0("(",letter,")"),bty="n")
  if(letter=="A")
    legend("topright",
           legend=c("Standard cultivars", "high yield culivars"),
           pch=0,
           text.col=mycol[,1],
           bty="n",
           cex=0.75,
           col=mycol[,1])
}

# Function for boxplot of change in global production
do.prx.boxplot <- function(df, y, y.lim, y.lab, letter, main){ # df = dt ; y="pc.prx.Yield" ; y.lim=c(0,20) ; y.lab="% change in yield (t/ha)"; letter="B"
  
  if(letter!="A") main <- NA
  boxplot(df[, paste0(y, ".00.T")], at=1, border=mycol[1,1], xlim=c(0.5,6), ylim=y.lim, ylab=y.lab, outline=FALSE, main=main)
  boxplot(df[, paste0(y, ".45.T")], at=2, border=mycol[icol[index],1], add=TRUE, ann=FALSE, col=NA, outline=FALSE)
  boxplot(df[, paste0(y, ".85.T")], at=3, border=mycol[icol[index],1], add=TRUE, ann=FALSE, col=NA, outline=FALSE)
  axis(1, at=c(1.25,3.25,5.5), labels=c("Baseline","RCP4.5","RCP8.5"),cex.axis=0.9)
  abline(h=0)
  if(letter=="A")
    legend("topright",
           legend=c("Standard cultivars", "high yield culivars"),
           pch=0,
           text.col=mycol[,1],
           bty="n",
           cex=0.75,
           col=mycol[,1])
}

# Function for boxplot of adpatation
do.adaptation.boxplot <- function(df, y, y.lim, y.lab, letter){ # df=dt ; y="adapt.Yield" ; y.lim=c(0,20) ; y.lab="% change in yield (t/ha)"; letter="A"
  boxplot(df[, paste0(y, ".45.T")], at=1, xlim=c(0.5,2.5), ylim=y.lim, ylab=y.lab, outline=FALSE)
  boxplot(df[, paste0(y, ".45.T")], at=2, add=TRUE, ann=FALSE, col=NA, outline=FALSE)
  axis(1, at=c(1,2), labels=c("RCP4.5","RCP8.5"),cex.axis=0.9)
  abline(h=0)
  legend("topleft",legend=paste0("(",letter,")"),bty="n")
}

#------------------
# Boxplot of global
# produciton and
# adaptation
#------------------

n <- length(unique(dt$model))

pdf(file = "Figure 3 (absolute).pdf")
par(mfrow=c(2,2),las=1,pty="s",tck=0.02)
do.boxplot(            dt, "Yield",         c(0,28),     "Global production (Mg/year)",           "A", paste0(n, " Models"))
do.adaptation.boxplot( dt, "adapt.Yield",   c(-1,1),   "Global production\nadaptation (Mg/year)", "B")
do.boxplot(            dt, "CroN.ma",       c(0,650),    "Global N demand (Mg N/year)",           "C")
do.adaptation.boxplot( dt, "adapt.CroN.ma", c(-20,20), "Global N demand\nadaptation (Mg N/year)", "D")
dev.off()

pdf(file = "Figure 3 (absolute per model).pdf")
par(mfrow=c(2,2),las=1,pty="s",tck=0.02)
for(m in unique(data$model))
{
  dt.1m <- dt[dt$model==m,]
  do.boxplot(            dt.1m, "Yield",          c(0,28),     " Grain yield (t/ha)",             "A", paste0("Model: ", m))
  do.adaptation.boxplot( dt.1m, "adapt.Yield",    c(-10,10),   "Grain yield \nadaptation (t/ha)", "B")
  do.boxplot(            dt.1m, "CroN.ma",        c(0,650),    "Crop N (kg N/ha)",             "C")
  do.adaptation.boxplot( dt.1m, "adapt.CroN.ma",  c(-200,200), "Crop N \nadaptation (kg N/ha)", "D")
}
dev.off()

#---------------------
# Boxplot of change in 
# global produciton
# and adaptation
#---------------------

n <- length(unique(dt$model))

pdf(file = "Figure 3 (relative).pdf")
par(mfrow=c(2,2),las=1,pty="s",tck=0.02)
do.prx.boxplot(        dt, "pc.prx.Yield",   c(-50,120),     "Change in global production (%)",           "A", paste0(n, " Models"))
do.adaptation.boxplot( dt, "rel.adapt.Yield",   c(-10,10),   "Global\nproduction adaptation (ù)", "B")
do.prx.boxplot(        dt, "pc.prx.CroN.ma", c(-50,80),    "Change in global N demand (%)",           "C")
do.adaptation.boxplot( dt, "rel.adapt.CroN.ma", c(-10,10), "Global N demand\nadaptation (%)", "D")
dev.off()

pdf(file = "Figure 3 (relative per model).pdf")
for(m in unique(data$model))
{ # m = "SQ"
  par(mfrow=c(2,2),las=1,pty="s",tck=0.02)
  dt.1m <- dt[dt$model==m,]
  do.prx.boxplot(            dt.1m, "pc.prx.Yield",    c(-50,120),     "Change in grain yield (t/ha)",             "A", paste0("Model: ", m))
  do.adaptation.boxplot( dt.1m, "rel.adapt.Yield",    c(-10,10),   "Grain yield \nadaptation (t/ha)", "B")
  do.prx.boxplot(            dt.1m, "pc.prx.CroN.ma",  c(-50,80),    "Change in crop N (%)",             "C")
  do.adaptation.boxplot( dt.1m, "rel.adapt.CroN.ma",  c(-200,200), "Crop N \nadaptation (%)", "D")
}
dev.off()