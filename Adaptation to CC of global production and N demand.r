#----------------------
# get site information
# add scaled wheat area
#----------------------

site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

#-----------------------------
# Calculate global yield and
# NUE response to N fertilizer
#-----------------------------

Nres$area.weight <- site$area.weight[match(Nres$site, site$Site)]

Nres$Yield.weight <- Nres$Yield * Nres$area.weight
Nres$NUE.weight   <- Nres$NUE   * Nres$area.weight

global.Nres <- as.data.frame(data.table(Nres)       [, lapply(.SD,  sum),  by=list(trait, rcp, ferti, gcm), .SDcols=c("Yield.weight","NUE.weight")])
global.Nres <- as.data.frame(data.table(global.Nres)[, lapply(.SD,  mean), by=list(trait, rcp, ferti),      .SDcols=c("Yield.weight","NUE.weight")])

global.Nres$trait.rcp <- paste0(global.Nres$trait, ".", global.Nres$rcp)


#-------------------------
# Calculate percent change
# in global production and 
# N demand
#-------------------------

global       <- data.frame(model.gcm = paste0(rep(unique(dt$model),each=length(unique(dt$gcm))), ".", rep(unique(dt$gcm),length(unique(dt$model)))), stringsAsFactors = F)
global$model <- substr(global$model.gcm, 1,2)
global$rcp   <- substr(global$model.gcm, 4,4)

dt$area.weight   <- site$area.weight[match(dt$site, site$Site)]

# Scaling to the globe using area data
for(v in c(paste0("adapt.", var, ".N(45)"), paste0("adapt.", var, "(45)"), 
           paste0("adapt.", var, ".N(85)"), paste0("adapt.", var, "(85)")))   # v="adapt.yield.opt.N(45)"
{
  # Absolute change in  production or N demand at each site
  adapt.wht.v <- paste0("adapt.wht", substr(v, 6, nchar(v)))
  dt[, adapt.wht.v] <- dt$area.weight * dt[, v]
  
  # Absolute change in global production or N demand (for each model and gcm)
  adapt.global <- paste0("adapt.global", substr(v, 6, nchar(v)))
  global[, adapt.global]  <- round( as.data.frame(data.table(dt)[, lapply(.SD,  sum), by=list(model,gcm), .SDcols=adapt.wht.v])[,3] , 2)
}
rm(v, adapt.wht.v, adapt.global)


#-----
# Plot
#-----

# Define plot layout
pl <- c(1:4)
plotmat <- matrix(pl, 2, 2, byrow=T)
# layout.show(layout(plotmat))

my.col.b <- rep(c("goldenrod4","darkmagenta", "blue2"), 2)
my.col.f <- rep(c("goldenrod1","magenta", "deepskyblue"), 2)
l.wd <- 1.5  # width of the boxplot lines
wd <- 1.1  # box width
at <- c(1, 5, 9) 
ymin <- c(-40, -10) 
ymax <- c(100, 20)

pdf(paste0(main.wd , "Data_Analysis\\Boxplot percent change per site\\", title.pdf, ".pdf"), paper="a4", width=9, height=13)
par(mar=c(4,0.3,2,0.3), oma=c(10,6,2,6), tck=0.02 , xaxs="i", yaxs="i",las=1)
layout(plotmat , heights = c(1,1,1,1), widths = c(1,1,1))


# Plot Yield and NUE response to N fertilizer
#--------------------------------------------

my.col.b <- rep(c("goldenrod4","darkmagenta", "blue2"), each=2)
my.col.f <- c("white", "goldenrod1", "white", "magenta", "white", "deepskyblue")
my.pch    <- rep(c(21, 24, 22), each=2) 
my.lty    <- rep(c(1,4), 3) 
let <- c("(A)", "(B)", "(C)","(D)")
      
i.v <- 0           
for(v in c("Yield.weight","NUE.weight"))  # v="Yield.weight"
{
  i.v <- i.v + 1
  i.t <- 0
 y.max <- max(global.Nres[,v])
 for(t in unique(global.Nres$trait.rcp))   # t = "N.00"
 { 
   i.t   <- i.t + 1
   # Plot data
   y <- global.Nres[global.Nres$trait.rcp==t, v]
   x <- seq(0,450,50)
   if(t == "N.00")  plot(x, y, 
                         ylab="", 
                         xlab="",
                         xaxt="n",
                         yaxt="n",
                         ylim=c(0, 1.05 * y.max),
                         cex=0.8,
                         col=my.col.b[i.t],
                         # bg=my.col.f[i.t],
                         pch=mypch[i.t])
   if(t != "N.00")  points(x, y, cex=0.8, col=my.col.b[i.t], pch=mypch[i.t])
   # Fit a spline 
   p_y.VS.dose <- predict(smooth.spline(y ~ x, cv=FALSE, all.knots = FALSE), x= seq(0,450, by=1) )
   lines(p_y.VS.dose, type="l", lty=my.lty[i.t], col=my.col.b[i.t])
 }
 # Add X axis labels
             axis(1, at=seq(0,400,100), labels=seq(0,400,100), cex.axis=1.5, mgp=c(3,1,0), tck=0.02)
 if(i.v==1)  axis(2, at=seq(0,12,3),    labels=seq(0,12,3),    cex.axis=1.5, mgp=c(3,1,0), tck=0.02)
 if(i.v==2)  axis(4, at=seq(0,40,10),   labels=seq(0,40,10),   cex.axis=1.5, mgp=c(3,1,0), tck=0.02)
 # Add letter
 legend(x=0, y=1.05*y.max, legend=let[i.v],bty="n", cex=1.5)
}

# Add Y axis titles
mtext("N fertilizer rate (kg N/ha)", side=1, cex=1.5, line =-28.5, outer = T)
mtext("Grain yield (t DM/ha)",      side=2, cex=1.5, line =3,   outer = T, adj=0.80, las=0)
mtext("N use efficiency (t/kg DM)", side=4, cex=1.5, line =4,   outer = T, adj=0.85, las=0)

# plot %change in yield and N demand  
#-----------------------------------               

i.v <- 0
for(v in unique(var))  # v = "yield.opt"
{
  i.v <- i.v + 1
  for(i in unique(c(paste0("adapt.global.", v, ".N(45)"), paste0("adapt.global.", v, "(45)"),
                    paste0("adapt.global.", v, ".N(85)"), paste0("adapt.global.", v, "(85)"))))   #  i=paste0("adapt.global.", v, ".N(45)")
  {
    y <- global[,i]
    IQR <- summary(y)[5] -summary(y)[2] 
    tmp <- c(summary(y)[2] - 2 * IQR, summary(y)[5] + 2 * IQR)
    if(exists('TMP') == F) { TMP <- tmp } else { TMP <- c(TMP, tmp) }
  }
  y.lim <- range(TMP)
  rm(i, IQR,TMP, tmp)
  
  boxplot(global[paste0("adapt.global.", v, ".N(45)")],   
          at=1, col="white",   border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.5,5.5), ylim=y.lim, 
          width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd)
  boxplot(global[paste0("adapt.global.", v, "(45)")], 
          at=2, col=my.col.f[2], border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', 
          width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
  abline(h=0)
  boxplot(global[paste0("adapt.global.", v, ".N(85)")],   
          at=4, col="white",   border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
          width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
  boxplot(global[paste0("adapt.global.", v, "(85)")], 
          at=5, col=my.col.f[3], border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
          width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
  
  # Add letter
  legend(x=0, y=1.05*y.lim[2], legend=let[i.v+2],bty="n", cex=1.5)
  
  # Add Y axis
  if(i.v==1)  axis(2, cex.axis=1.5, las=1) 
  if(i.v==2)  axis(4, cex.axis=1.5, las=1) 
  
  # Add legend
  if(i.v == 1) legend(x=2, y=1.05*y.lim[2], legend=c("No N adaptation","With N adaptation"), pch=c(0,15), bty="n", cex=1.5, pt.cex=4)

  # Add X axis titles
  axis(1, at=c(1.5,4.5), labels=c("RCP 4.5", "RCP 8.5"), cex.axis=1.5, las=0, mgp=c(3,1,0))
}
    

# Add Y axis titles
mtext(y.lab[1, "y.lab"], side=2, adj=0.15, cex=1.5, las=0, line =4, outer = T)
mtext(y.lab[2, "y.lab"], side=4, adj=0.15, cex=1.5, las=0, line =4, outer = T)

dev.off()

rm(v, site, pl, plotmat, my.col.b, my.col.f, l.wd, wd, at, i.v, i.t, y, y.lim, ymax, ymin, let, global)
