# get site information add scaled wheat area
site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")


# Read simulated yield interpolated at national fertilization rate 
res.opt.30y <- readRDS(paste0(DataAnalysis.wd,  "CropNtoMaximizeYield_30yrsAve.RDS"))

# Get simulated yield interpolated at national fertilization rate from list  
get.data <- function(data){ # data="current.dose"
  dt <- res.opt.30y[[data]]
  dt <- dt[which(dt$model%in% c("e.median") ), c("site","trait","rcp","gcm","model","Yield", "GPC", "NUtE")]
  # Calculate means over the 5 GCMS
  dt <- as.data.frame(data.table(dt)[, lapply(.SD,  mean), by=list(site, trait, rcp, model), .SDcols=c("Yield", "GPC", "NUtE")])
  # Calculate grain N yield taking into account that grain yield is expressed at 13% moisture content
  dt$GrainN <- ( dt$GPC * ( 1000 * ( 1 - gmc ) * dt$Yield ) ) / ( 100 * 5.62 )
  return(dt)
}

current.dose    <- get.data(data="current.dose")
opt             <- get.data(data="opt")
opt.gpc         <- get.data(data="opt.gpc")

#---------------------------------
# Plot grain protein concentration
# vs grain yield
#---------------------------------

n.dose <-c("current.dose", "opt")

pdf(paste0(main.wd , "Data_Analysis\\Figures for paper\\Fig S2. ", "GPC", " vs ", "NUtE", "- ", "(all sites).pdf"))
par(mar=rep(0.3,4), oma=c(4,4,0,0), tck=0.02 , xaxs="i", yaxs="i",las=1, pty="s")
layout(matrix(c(1:2), 2, 1, byrow=T), heights = c(1,1,1,1), widths = c(1,1,1,1), respect = T)

ylim <- c(0.95 * min(current.dose$GPC,  opt$GPC),  1.05 * max(current.dose$GPC,  opt$GPC))
xlim <- c(0.95 * min(current.dose$NUtE, opt$NUtE), 1.05 * max(current.dose$NUtE, opt$NUtE))

plot.index <- 0

for(i.dose in 1:2) # i.dose = 1
{
  plot.index <- plot.index + 1
  
  # Create empty plot without axis
  plot(0, 0,
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim = ylim,
       xlim = xlim,
       pch="")
  
  # Add equation of GPC vs NUtE from Savin et al., (2019) - https://doi.org/10.1016/j.fcr.2019.107573 
  NUtE <- seq(10,100, 1)
  GPC  <- 5.62 * ( 6.98 - 1.36 * log(NUtE))
  lines(NUtE, GPC, lwd=0.8, col = "grey")
  
  # Add regression to both standard and high yield traits and current and optimum N fertilizer dose
  x    <- c(current.dose$NUtE, opt$NUtE)
  y    <- c(current.dose$GPC, opt$GPC)
  fit <- lm(y ~ log(x))
  lines(seq(1,100,1),predict(fit,data.frame(x=seq(1,100,1))))
  summary(fit)
  # Function to add data points
  add.data <- function(dt){
    # Add data for standard traits 
    points(dt[which(dt$trait=="N"& dt$rcp=="00"), "NUtE"], dt[which(dt$trait=="N"& dt$rcp=="00"), "GPC"], pch = 1, col = "goldenrod1", cex=0.8)
    points(dt[which(dt$trait=="N"& dt$rcp=="45"), "NUtE"], dt[which(dt$trait=="N"& dt$rcp=="45"), "GPC"], pch = 1, col =  "magenta", cex=0.8)
    points(dt[which(dt$trait=="N"& dt$rcp=="85"), "NUtE"], dt[which(dt$trait=="N"& dt$rcp=="85"), "GPC"], pch = 1, col =  "deepskyblue", cex=0.8)
    
    # Add data for high yield traits
    points(dt[which(dt$trait=="T"& dt$rcp=="00"), "NUtE"], dt[which(dt$trait=="T"& dt$rcp=="00"), "GPC"], pch = 19, col = "goldenrod1", cex=0.8)
    points(dt[which(dt$trait=="T"& dt$rcp=="45"), "NUtE"], dt[which(dt$trait=="T"& dt$rcp=="45"), "GPC"], pch = 19, col = "magenta", cex=0.8)
    points(dt[which(dt$trait=="T"& dt$rcp=="85"), "NUtE"], dt[which(dt$trait=="T"& dt$rcp=="85"), "GPC"], pch = 19, col = "deepskyblue", cex=0.8)
   }
  
  if(i.dose == 1)  
  {
    # Add points
    add.data(current.dose)
    # Add plot letter
    legend("topleft", legend=paste0("(", LETTERS[plot.index], ")     Current N application"), bty="n", cex=0.8)  
  }
  
  if(i.dose == 2)
  {
    # Add points
    add.data(opt)
    # Add plot letter     
    legend("topleft", legend=paste0("(", LETTERS[plot.index], ")     N application to maximize grain yield"), bty="n", cex=0.8)  
  }
  
  # Add Y axis
  axis(2, cex.axis=0.8, las=1)
  if(i.dose == 1)
  {
    # Baseline
    text(57,   12, "base-\nline", cex=0.6)
    legend(55.5, 11.9, legend=rep("",2), pch=c(1, 19), bty="n",cex=0.8, col="goldenrod1")
    # RCP45
    text(61,   12, "RCP\n4.5", cex=0.6)
    legend(59.72, 11.9,  legend=rep("",2), pch=c(1, 19), bty="n",cex=0.8, col="magenta")
    # RCP8.5
    text(65,      12, "RCP\n8.5", cex=0.6)
    legend(63.5, 11.9,  legend=rep("",2), pch=c(1, 19), bty="n",cex=0.8, col="deepskyblue")
    x=14 ; y = 7.1 ; s = 1.7
    text(50, 11.52, c("Standard elite cultivar"), cex=0.6)
    text(50.8, 11.1, c("High yield ideotype"),    cex=0.6)
  }
}  

# Add x axis
axis(1, cex.axis=0.8, las=0)

# Add axis titles
mtext("Simulated grain protein concentration (%)",                            side=2, adj=0.5, cex=0.8, las=0, line=-6, outer=T)
mtext("Simulated nitrogen utilisation efficiency\n(kg grain DM / kg crop N)", side=1, adj=0.5, cex=0.8, las=0, line=2.5, outer=T)

dev.off()
