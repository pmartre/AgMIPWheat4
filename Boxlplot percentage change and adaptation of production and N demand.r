# create a data frame for each mode of calculation of optimum N
no.N.adapt   <- as.data.frame(dt[["fao.dose"]])
no.N.adapt   <- no.N.adapt[-which(no.N.adapt$model %in% c("e.median","q.25","q.75")),]
N.adapt      <- as.data.frame(dt[["opt"]])
N.adapt      <- N.adapt[-which(N.adapt$model      %in% c("e.median","q.25","q.75")),]
N.adapt.gpc  <- as.data.frame(dt[["opt.gpc"]])
N.adapt.gpc  <- N.adapt.gpc[-which(N.adapt.gpc$model      %in% c("e.median","q.25","q.75")),]

#----------------------
# get site information
# add scaled wheat area
#----------------------

site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

#---------------------------
# Scale simulated yield and
# NUE to the global based on 
# production area
#---------------------------

Nres$area.weight <- site$area.weight[match(Nres$site, site$Site)]
Nres$Yield.weight <- Nres$Yield * Nres$area.weight
Nres$NUE.weight   <- Nres$NUE   * Nres$area.weight
var.weigth <- paste0(var[c(1,2)],".weight")
global.Nres <- as.data.frame(data.table(Nres)       [, lapply(.SD,  sum),  by=list(model, gcm, trait, rcp, ferti), .SDcols=var.weigth])
global.Nres <- as.data.frame(data.table(global.Nres)[, lapply(.SD,  mean), by=list(             trait, rcp, ferti), .SDcols=var.weigth])
global.Nres$trait.rcp <- paste0(global.Nres$trait, ".", global.Nres$rcp)

#---------------------------
# Scale percent change in 
# yield to yield NUE to the 
# global on based production
#---------------------------

scale.pc <- function(data){   # data = no.N.adapt 
  # Create an empty data frame with model and GCM columns
  global       <- data.frame(model.gcm = paste0(rep(unique(data$model),each=length(unique(data$gcm))), ".", rep(unique(data$gcm),length(unique(data$model)))), stringsAsFactors = F)
  global$model <- substr(global$model.gcm, 1,2)
  global$gcm   <- substr(global$model.gcm, 4,4)
  
  # Scale to yield and NUE to the  globe based on production
  for(v in c(paste0("pc.", var, "(00)"), paste0("pc.", var, "(45)"), paste0("pc.", var, "(85)")))   # v = "pc.Yield(00)"
  {
    # Scale % change in yield and NUE to the globe based on production
    data$production.weight <- site$production.weight[match(data$site, site$Site)]
    pc.wht.v <- paste0("pc.wht", substr(v, 3, nchar(v)))
    data[, pc.wht.v] <- data$production.weight * data[, v]
    # Calculate % change in yield and NUE to the globe based on production area (for each model and gcm)
    pc.global <- paste0("pc.global", substr(v, 3, nchar(v)))
    global[, pc.global]  <- round(as.data.frame(data.table(data)[, lapply(.SD,  sum), by=list(model, gcm), .SDcols=pc.wht.v])[,3] , 2)
  }
  return(global)
}
pc.no.N.adapt  <- scale.pc(no.N.adapt)
pc.N.adapt     <- scale.pc(N.adapt)
pc.N.adapt.gpc <- scale.pc(N.adapt.gpc)


#-----------------------------
# Scale relative adaptation in 
# yield to yield NUE to the 
# global on based production
#-----------------------------

scale.r.adapt <- function(data){   # data = no.N.adapt 
  # Create an empty data frame with model and GCM columns
  global       <- data.frame(model.gcm = paste0(rep(unique(data$model),each=length(unique(data$gcm))), ".", rep(unique(data$gcm),length(unique(data$model)))), stringsAsFactors = F)
  global$model <- substr(global$model.gcm, 1,2)
  global$gcm   <- substr(global$model.gcm, 4,4)
  
  # Scale to yield and NUE to the  globe based on production
  for(v in c(paste0("r.adapt.", var, "(45)"), paste0("r.adapt.", var, "(85)")))   # v = "r.adapt.Yield(45)"
  {
    # Scale % change in yield and NUE to the globe based on production
    data$production.weight <- site$production.weight[match(data$site, site$Site)]
    r.adapt.wht.v <- paste0("r.adapt.wht", substr(v, 8, nchar(v)))
    data[, r.adapt.wht.v] <- data$production.weight * data[, v]
    # Calculate % change in yield and NUE to the globe based on production area (for each model and gcm)
    r.adapt.global <- paste0("r.adapt.global", substr(v, 8, nchar(v)))
    global[, r.adapt.global]  <- round(as.data.frame(data.table(data)[, lapply(.SD,  sum), by=list(model, gcm), .SDcols=r.adapt.wht.v])[,3] , 2)
  }
  return(global)
}
r.adapt.no.N.adapt  <- scale.r.adapt(no.N.adapt)
r.adapt.N.adapt     <- scale.r.adapt(N.adapt)
r.adapt.N.adapt.gpc <- scale.r.adapt(N.adapt.gpc)

#-----
# Plot
#-----

# Define plot layout    
plotmat <- matrix(c(1:6), 3, 2, byrow=T)
layout.show(layout(plotmat))

l.wd <- 1.5  # width of the boxplot lines
wd <- 1.1  # box width
my.pch    <- rep(c(21, 24, 22), each=2) 
my.lty    <- rep(c(1,4), 3) 
let <- c("(A)", "(B)", "(C)","(D)", "(E)", "(F)")

pdf(paste0(main.wd , "Data_Analysis\\Figures for paper\\", title.pdf, ".pdf"), paper="a4", width=9, height=13)
par(mar=c(4,0.3,2,1.5), oma=c(10,6,2,6), tck=0.02 , xaxs="i", yaxs="i", las=1)
layout(plotmat , heights = c(1,1,1,1), widths = c(1,1,1))

# Plot Yield and NUE response to N fertilizer
#--------------------------------------------
my.col.b <- rep(c("goldenrod4","darkmagenta", "blue2"), each=2)
my.col.f <- c("white", "goldenrod1", "white", "magenta", "white", "deepskyblue")
y.max <- c(13,43)

i.v <- 0           
for(v in c("Yield.weight","NUE.weight"))  # v="Yield.weight"
{
    i.v <- i.v + 1
    i.t <- 0
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
                            ylim=c(0, y.max[i.v]),
                            xlim=c(0,500),
                            cex=1.5,
                            col=my.col.b[i.t],
                            bg=my.col.f[i.t],
                            pch=my.pch[i.t])
      if(t != "N.00")  points(x, y, cex=1.5, col=my.col.b[i.t], bg=my.col.f[i.t], pch=my.pch[i.t])
      # Fit a spline 
      p_y.VS.dose <- predict(smooth.spline(y ~ x, cv=FALSE, all.knots = FALSE), x= seq(0,450, by=1) )
      lines(p_y.VS.dose, type="l", lty=my.lty[i.t], col=my.col.b[i.t])
    }
  
  # Add X axis labels
  axis(1, at=seq(0,500,100), labels=seq(0,500,100), cex.axis=1.5, mgp=c(3,1,0), tck=0.02)
  if(i.v==1)  axis(2, at=seq(0,12,3),    labels=seq(0,12,3),    cex.axis=1.5, mgp=c(3,1,0), tck=0.02)
  if(i.v==2)  axis(4, at=seq(0,40,10),   labels=seq(0,40,10),   cex.axis=1.5, mgp=c(3,1,0), tck=0.02)
  
  # Add letter
  legend(x=-40, y=y.max[i.v], legend=let[i.v],   bty="n", cex=1.5)

  # add legend
  if(i.v==1)  
  {
    text(235,   3.5, c("Base\nline"), cex=1.2)
    text(335,   3.5, c("RCP\n4.5"), cex=1.2)
    text(430,   3.5, c("RCP\n8.5"), cex=1.2)
    legend(200, 3,    legend=rep("",3), pch=unique(my.pch), col=my.col.b,                 lty=1, ncol=3, bty="n", cex=1.2, pt.cex=2)
    text(120,   2.5,   c("Standard\ncultivar"), cex=1.2)
    legend(200, 1.57, legend=rep("",3), pch=unique(my.pch), col=my.col.b, pt.bg=my.col.f[c(2,4,6)], lty=4, ncol=3, bty="n", cex=1.2, pt.cex=2)
    text(120,   1,   c("High yield\npotential"), cex=1.2)
  }
}

# Add axis titles
mtext("N fertilizer rate (kg N/ha)",  side=1, cex=1.5, line=-28.5, outer=T)
mtext("Grain yield (t /ha)",        side=2, cex=1.5, line=3,     outer=T, adj=0.85, las=0)
mtext("N use efficiency (kg grain /kg N)", side=4, cex=1.5, line=2,     outer=T, adj=0.90, las=0)

# plot %change in yield and N demand  
#-----------------------------------               

my.col.b <- c("goldenrod4","darkmagenta", "blue2")
my.col.f <- c("goldenrod1","magenta", "deepskyblue")

y.min <- c(-13, -5)
y.max <- c(120, 75)
at <- c(1, 5, 9)
i.v <- 0
for(v in unique(var[c(1,3)]))  # v = "Yield"
{
  i.v <- i.v + 1
  index <- 0
  for(r in c("00","45","85"))  #  r ="00"
  {
    index <-  index + 1
    ifelse(r == "00", add <- F, add <- T)
    # % change in yield due to high yield trait for variables calculated as  national N rate 
    boxplot(pc.no.N.adapt[, paste0("pc.global.", v, "(",r,")")],   
            at=at[index], col=my.col.f[1], border=my.col.b[1], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.2,12), ylim=c(y.min[i.v],y.max[i.v]), 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=add)
    # % change in yield due to high yield trait for variables calculated as  national N rate 
    boxplot(pc.N.adapt[, paste0("pc.global.", v, "(",r,")")],  
            at=at[index]+1, col=my.col.f[2], border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    # Variables calculated the national N rate for high yielding cultivar
    boxplot(pc.N.adapt.gpc[, paste0("pc.global.", v, "(",r,")")],  
            at=at[index]+2, col=my.col.f[3], border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
  }
  abline(h=0)
  
  # Add letter
  legend(x=-0.5, y=1*y.max[i.v], legend=let[i.v+2], bty="n", cex=1.5)
  
  # Add Y axis
  if(i.v == 1) axis(2, cex.axis=1.5, las=1) 
  if(i.v == 2) axis(4, cex.axis=1.5, las=1) 
  
  # Add X axis titles
  axis(1, at=c(2,6,10), labels=c("Base-\nline", "RCP\n4.5", "RCP\n8.5"), cex.axis=1.5, las=0, mgp=c(3,2,0))

  # Add legend
  if(i.v == 1) legend(x=0.4, y=0.94*y.max[1], legend=c("No N adaptation", "With N adpatation", "With N adpatation + minimum\ngrain protein concentration"),
                      pch=15, col=my.col.f, bty="n", cex=1.2, pt.cex=3)
}

# Add axis titles
mtext("Climate scenario",  side=1, cex=1.5, outer = T)
mtext("% change in global production",  side=2, adj=0.125, cex=1.5, las=0, line=3, outer = T)
mtext("% change in global N demand",    side=4, adj=0.125, cex=1.5, las=0, line=2, outer = T)

# plot Relative adaptation in yield and N demand  
#-----------------------------------               

my.col.b <- c("darkmagenta", "blue2")
my.col.f <- c("magenta", "deepskyblue")

y.min <- c(-3, -3)
y.max <- c(3, 3)
at <- c(5, 9)
i.v <- 0
for(v in unique(var[c(1,3)]))  # v = "Yield"
{
  i.v <- i.v + 1
  index <- 0
  for(r in c("45","85"))  #  r ="45"
  {
    index <-  index + 1
    ifelse(r == "45", add <- F, add <- T)
    # % change in yield due to high yield trait for variables calculated as  national N rate 
    boxplot(r.adapt.no.N.adapt[, paste0("r.adapt.global.", v, "(",r,")")],   
            at=at[index], col=my.col.f[1], border=my.col.b[1], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.2,12), ylim=c(y.min[i.v],y.max[i.v]), 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=add)
    # % change in yield due to high yield trait for variables calculated as  national N rate 
    boxplot(r.adapt.N.adapt[, paste0("r.adapt.global.", v, "(",r,")")],
            at=at[index]+1, col=my.col.f[2], border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    # Variables calculated the national N rate for high yielding cultivar
    boxplot(r.adapt.N.adapt.gpc[, paste0("r.adapt.global.", v, "(",r,")")], 
            at=at[index]+2, col=my.col.f[3], border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
  }
  abline(h=0)
  
  # Add letter
  legend(x=-0.5, y=1*y.max[i.v], legend=let[i.v+4], bty="n", cex=1.5)
  
  # Add Y axis
  if(i.v == 1) axis(2, cex.axis=1.5, las=1) 
  if(i.v == 2) axis(4, cex.axis=1.5, las=1) 
  
  # Add X axis titles
  axis(1, at=c(2,6,10), labels=c("Base-\nline", "RCP\n4.5", "RCP\n8.5"), cex.axis=1.5, las=0, mgp=c(3,2,0))
  
  # Add legend
  # if(i.v == 1) legend(x=0.4, y=0.94*y.max[1], legend=c("No N adaptation", "With N adpatation", "With N adpatation + minimum\ngrain protein concentration"),
  #                     pch=15, col=my.col.f, bty="n", cex=1.2, pt.cex=3)
}

# Add axis titles
mtext("Climate scenario",  side=1, cex=1.5, outer = T)
mtext("Relative adaptation to climate change for global grain yield (%)",  side=2, adj=0.125, cex=1.5, las=0, line=3, outer = T)
mtext("Relative adaptation to climate change for global N demand (%)",    side=4, adj=0.125, cex=1.5, las=0, line=2, outer = T)

dev.off()

# rm(v, site, pl, plotmat, my.col.b, my.col.f, l.wd, wd, at, i.v, i.t, y, y.lim, ymax, ymin, let, global)
