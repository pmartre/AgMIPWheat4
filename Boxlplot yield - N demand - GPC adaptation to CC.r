# create a data frame for each mode
no.N.adapt <- as.data.frame(dt[["fao.yield"]])
no.N.adapt <- no.N.adapt[-which(no.N.adapt$model %in% c("e.median","q.25","q.75")),]
N.adapt    <- as.data.frame(dt[["opt"]])
N.adapt    <- N.adapt[-which(N.adapt$model %in% c("e.median","q.25","q.75")),]
N.adapt.gpc    <- as.data.frame(dt[["opt.gpc"]])
N.adapt.gpc    <- N.adapt.gpc[-which(N.adapt.gpc$model %in% c("e.median","q.25","q.75")),]

#----------------------
# get site information
# add scaled wheat area
#----------------------

site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

#---------------------------
# Scale percent change in 
# yield to yield NUE to the 
# global on based production
#---------------------------

scale.to.globe <- function(data){   # data = no.N.adapt 
  # Create an empty data frame with model and GCM columns
  global       <- data.frame(model.gcm = paste0(rep(unique(data$model),each=length(unique(data$gcm))), ".", rep(unique(data$gcm),length(unique(data$model)))), stringsAsFactors = F)
  global$model <- substr(global$model.gcm, 1,2)
  global$gcm   <- substr(global$model.gcm, 4,4)
  
  # Scale to yield and NUE to the  globe based on production
  for(v in c(paste0("r.adapt.",var, "(45)"), paste0("r.adapt.",var, "(85)")))   # v = "r.adapt.Yield(45)"
  {
    # Scale % change in yield and NUE to the globe based on production
    data$production.weight <- site$production.weight[match(data$site, site$Site)]
    wht.v <- paste0("wht.", v)
    data[, wht.v] <- data$production.weight * data[, v]
    # Calculate % change in yield and NUE to the globe based on production area (for each model and gcm)
    global.v <- paste0("global.", v)
    global[, global.v]  <- round(as.data.frame(data.table(data)[, lapply(.SD,  sum), by=list(model, gcm), .SDcols=wht.v])[,3] , 2)
  }
  return(global)
}

global.no.N.adapt  <- scale.to.globe(no.N.adapt)
global.N.adapt     <- scale.to.globe(N.adapt)
global.N.adapt.gpc <- scale.to.globe(N.adapt.gpc)

#-----
# Plot
#-----

# Define plot layout    
plotmat <- matrix(c(1,2,3), 3, 1, byrow=T)
# layout.show(layout(plotmat))

l.wd <- 1.5  # width of the boxplot lines
wd <- 1.1  # box width
my.pch    <- rep(c(21, 24, 22), each=2) 
my.lty    <- rep(c(1,4), 3) 
let <- c("(A)", "(B)", "(C)")

pdf(paste0(DataAnalysis.wd , "Figures for paper/", title.pdf, ".pdf"), paper="a4", width=9, height=13)
par(mar=c(3.5,3.3,0.5,1.5), oma=c(3,9.3,2,6), tck=0.02 , xaxs="i", yaxs="i", las=1)
layout(plotmat , heights = c(1,1,1,1), widths = c(1,1,1))

my.col.b <- rep(c("goldenrod4", "darkmagenta", "blue2"), 2)
my.col.f <- rep(c("goldenrod1", "magenta",     "deepskyblue"),2)

y.max <- c(0.17, 0.17, 0.17)
y.min <- c(-0.32, -0.32, -0.32)
at <- c(1, 5)

i.v <- 0
index <- 0
for(v in unique(var))  # v = "Yield"
{
  i.v <- i.v + 1
  i.r <- 0
  for(r in c("45","85"))  #  r ="45"
  {
    index <- index + 1
    i.r <- i.r + 1
    ifelse(r == "45", add <- F, add <- T)
    # Variables calculated the national N rate for standard cultivar
    boxplot(global.no.N.adapt[, paste0("global.r.adapt.", v, "(",r,")")],   
            at=at[i.r], col=my.col.f[index], border=my.col.b[index], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.2,8), ylim=c(y.min[i.v],y.max[i.v]), 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=add)
    # Variables calculated the national N rate for standard cultivar
    boxplot(global.N.adapt[, paste0("global.r.adapt.", v, "(",r,")")],   
            at=at[i.r]+1, col=my.col.f[index+1], border=my.col.b[index+1], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.2,12), ylim=c(y.min[i.v],y.max[i.v]), 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    boxplot(global.N.adapt.gpc[, paste0("global.r.adapt.", v, "(",r,")")],   
            at=at[i.r]+2, col=my.col.f[index+2], border=my.col.b[index+2], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.2,12), ylim=c(y.min[i.v],y.max[i.v]), 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    index <- index +2
  }
  index <- 0
  abline(0,0)
  # Add letter
  legend(x=0, y=1.1*y.max[i.v], legend=let[i.v], bty="n", cex=2.5)
  # Add Y axis
  axis(2, cex.axis=2, las=1) 
}
# Add X axis titles
axis(1, at=c(2,6), labels=c("RCP4.5", "RCP8.5"), cex.axis=2, las=0, mgp=c(4,1.4,0))

# Add legend
if(i.v == 3)
{
  legend(x=0.5, y=-0.06, pch=15, col=my.col.f[1], bty="n", cex=2, pt.cex=4, 
         legend="Current N application rate")
  legend(x=0.5, y=-0.12, pch=15, col=my.col.f[2], bty="n", cex=2, pt.cex=4,
         legend="N application rate to maximize grain yield")
  legend(x=0.5, y=-0.17, pch=15, col=my.col.f[3], bty="n", cex=2, pt.cex=4,
         legend="N application rate to maximize grain yield\nwith grain protein > 12%")
}
# Add axis titles
mtext("Climate scenario",                                                              side=1, cex=1.5, outer = T, line =1.2)
mtext("Relative climate change\n adaptation for global   \nproduction (%)      ",              side=2, cex=1.5, outer = T, adj=0.95, las=0, line=3)
mtext("Relative climate change\nadaptation for global\nN demand (%)",                  side=2, cex=1.5, outer = T, adj=0.5,  las=0, line=3)
mtext(" Relative climate change\n   adaptation for global\nprotein concentration (%)", side=2, cex=1.5, outer = T, adj=0.08, las=0, line=3)

dev.off()
