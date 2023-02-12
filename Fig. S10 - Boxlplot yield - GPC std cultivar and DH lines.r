# create a data frame for each mode - unlimited N from step 1
UL   <- as.data.frame(dt[["UL"]])
UL <- UL[-which(UL$model %in% c("e.median","q.25","q.75")),]

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
  for(v in c(paste0(var, "(00N)"), paste0(var, "(45N)"), paste0(var, "(85N)"),
             paste0(var, "(00T)"), paste0(var, "(45T)"), paste0(var, "(85T)")))   # v = "Yield(00N)"
  {
    # Scale % change in yield and NUE to the globe based on production
    data$area.weight <- site$area.weight[match(data$site, site$Site)]
    wht.v <- paste0("wht.", v)
    data[, wht.v] <- data$area.weight * data[, v]
    # Calculate % change in yield and NUE to the globe based on production area (for each model and gcm)
    global.v <- paste0("global.", v)
    global[, global.v]  <- round(as.data.frame(data.table(data)[, lapply(.SD,  sum), by=list(model, gcm), .SDcols=wht.v])[,3] , 2)
  }
  return(global)
}
global.UL <- scale.to.globe(UL)

#-----
# Plot
#-----

# Define plot layout    
plotmat <- matrix(c(1:2), 2, 1, byrow=T)
# layout.show(layout(plotmat))

l.wd <- 1.5  # width of the boxplot lines
wd <- 1.1  # box width
my.pch    <- rep(c(21, 24, 22), each=2) 
my.lty    <- rep(c(1,4), 3) 
let <- c("(A)", "(B)")

pdf(paste0(main.wd , "Data_Analysis/Figures for paper/", title.pdf, ".pdf"), paper="a4", width=9, height=13)
par(mar=c(1.5,0.3,0.5,1.5), oma=c(3,6.3,2,6), tck=0.02 , xaxs="i", yaxs="i", las=1)
layout(plotmat , heights = c(1,1,1,1), widths = c(1,1,1))

my.col.b <- c("goldenrod4", "goldenrod4", "darkmagenta", "darkmagenta", "blue2", "blue2")
my.col.f <- c("white",      "goldenrod1", "white",       "magenta",     "white", "deepskyblue")


y.min <- c(0, 0)
y.max <- c(17, 21)
at <- c(1, 5, 9)

i.v <- 0
index <- 0
for(v in unique(var))  # v = "Yield"
{
  i.v <- i.v + 1
  i.r <- 0
  for(r in c("00","45","85"))  #  r ="00"
  {
    index <- index + 1
    i.r <- i.r + 1
    ifelse(r == "00", add <- F, add <- T)
    # Variables calculated the national N rate for standard cultivar
    boxplot(global.UL[, paste0("global.", v, "(",r,"N)")],   
            at=at[i.r], col=my.col.f[index], border=my.col.b[index], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.2,12), ylim=c(y.min[i.v],y.max[i.v]), 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=add)
    # Variables calculated the national N rate for standard cultivar
    boxplot(global.UL[, paste0("global.", v, "(",r,"T)")],   
            at=at[i.r]+1, col=my.col.f[index+1], border=my.col.b[index+1], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.2,12), ylim=c(y.min[i.v],y.max[i.v]), 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    index <- index +1
  }
  index <- 0
  # Add letter
  legend(x=-0.5, y=1*y.max[i.v], legend=let[i.v], bty="n", cex=1.5)
  # Add Y axis
  axis(2, cex.axis=1.5, las=1) 
}
# Add X axis titles
axis(1, at=c(2,6,10), labels=c("Base-\nline", "RCP\n4.5", "RCP\n8.5"), cex.axis=1.5, las=0, mgp=c(3,2.4,0))

# Add legend
legend(x=0.4, y=8, legend=c("Standard elite cultivar"),              pch=c(0),  bty="n", cex=1.5, pt.cex=3)
legend(x=0.4, y=5, legend=c("High-yield ideotype"), pch=c(15), bty="n", cex=1.5, pt.cex=3)

# Add axis titles
mtext("Climate scenario",                                                  side=1, cex=1.5, outer = T, line =2)
mtext("Global nitrogen        \nunlimited grain yield (t/ha)",               side=2, cex=1.5, outer=T,   adj=0.9,  las=0, line=3)
mtext("   Global nitrogen unlimited\ngrain protein concentration (%)",    side=2, cex=1.5, outer = T, adj=0.13, las=0, line=3.2)

dev.off()
