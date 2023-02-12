# create a data frame for each mode - unlimited N from step 1
no.N.adapt     <- as.data.frame(dt[["current.dose"]])
no.N.adapt     <- no.N.adapt[-which(no.N.adapt$model %in% c("e.median","q.25","q.75")),]
N.adapt        <- as.data.frame(dt[["opt"]])
N.adapt        <- N.adapt[-which(N.adapt$model %in% c("e.median","q.25","q.75")),]
N.adapt.gpc    <- as.data.frame(dt[["opt.gpc"]])
N.adapt.gpc    <- N.adapt.gpc[-which(N.adapt.gpc$model %in% c("e.median","q.25","q.75")),]

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

Nres$area.weight      <- site$area.weight[match(Nres$site, site$Site)]
Nres$Yield.weight     <- Nres$Yield * Nres$area.weight
Nres$NUE.weight       <- Nres$NUE   * Nres$area.weight
Nres$GPC.weight       <- Nres$GPC   * Nres$area.weight
Nres$CroN.ma.weight   <- Nres$CroN.ma * Nres$area.weight

var.weigth <- paste0(var[c(1:4)],".weight")
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
pc.no.N.adapt   <- scale.pc(no.N.adapt)
pc.N.adapt      <- scale.pc(N.adapt)
pc.N.adapt.gpc  <- scale.pc(N.adapt.gpc)

#-----
# Plot
#-----

# Define plot layout    
plotmat <- matrix(c(1:6), 2, 3, byrow=T)
# layout.show(layout(plotmat, respect = TRUE))

l.wd <- 1.5  # width of the boxplot lines
wd <- 1.1  # box width
my.pch    <- rep(c(21, 24, 22), each=2) 
my.lty    <- rep(c(1,4), 3) 
let <- c("(A)", "(B)", "(C)","(D)", "(E)", "(F)", "(G)", "(H)")

pdf(paste0(main.wd , "Data_Analysis\\Figures for paper\\", title.pdf, ".pdf")) #, paper="a4", width=9, height=13)
par(mar=c(0,4.3,0,0.8), oma=c(0,0,0,0), tck=0.02 , xaxs="i", yaxs="i", las=1 , pty="s")
layout(plotmat , respect = TRUE)

# Plot Yield and NUE response to N fertilizer
#--------------------------------------------
my.col.b <- rep(c("goldenrod4","darkmagenta", "blue2"), each=2)
my.col.f <- c("white", "goldenrod1", "white", "magenta", "white", "deepskyblue")
y.max <- c(13,37,15)

i.v <- 0           
for(v in c("Yield.weight","NUE.weight", "GPC.weight"))  # v="Yield.weight"
{
  i.v <- i.v + 1
  i.t <- 0
  for(t in  unique(global.Nres$trait.rcp))   # t = "N.00"
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
                          cex=1,
                          col=my.col.b[i.t],
                          bg=my.col.f[i.t],
                          pch=my.pch[i.t])
    if(t != "N.00")  points(x, y, cex=1, col=my.col.b[i.t], bg=my.col.f[i.t], pch=my.pch[i.t])
    # Fit a spline 
    p_y.VS.dose <- predict(smooth.spline(y ~ x, cv=FALSE, all.knots = FALSE), x= seq(0,450, by=1) )
    lines(p_y.VS.dose, type="l", lty=my.lty[i.t], col=my.col.b[i.t])
    if(t == "T.85")
    {
      # Add horizontal and vertical lines for global yield and N application rate, respectively
      Global.yield <- 3.45
      Global.N.rate <- sum(site$area.weight * site$nat.n.fertilizer)
      Global.NUE <- -9999 # 0.49 * 1000 * Global.yield / Global.N.rate  
      if(v == "Yield.weight")
      {
        points(Global.N.rate, Global.yield, pch = 4, col='red', cex = 2)
      } else
      {
        points( Global.N.rate, Global.NUE, pch = 4, col='red', cex = 2)
      }
    }
  }
  
  # Add X axis labels
  axis(1, at=seq(0,500,100), labels=seq(0,500,100), cex.axis=1, mgp=c(0,0.5,0), tck=0.02)
  #  ?dd Y axis labels
  if(i.v==1)  axis(2, at=seq(0,13,3),  labels=seq(0,13,3),  cex.axis=1, mgp=c(0,0.5,0), tck=0.02)
  if(i.v==2)  axis(2, at=seq(0,37,10), labels=seq(0,37,10), cex.axis=1, mgp=c(0,0.5,0), tck=0.02)
  if(i.v==3)  axis(2, at=seq(0,15,3),  labels=seq(0,15,3),  cex.axis=1, mgp=c(0,0.5,0), tck=0.02)
  
  # Add letter
  legend(x=-30, y=y.max[i.v], legend=let[i.v],   bty="n", cex=1)
  
  # add legend
  if(i.v==3)  
  {
    text(320,   7,   c("Base\nline"),             cex=0.9)
    text(395,   7,   c("RCP\n4.5"),               cex=0.9)
    text(465,   7,   c("RCP\n8.5"),               cex=0.9)
    legend(295, 6,   legend=rep("",3), pch=unique(my.pch), col=my.col.b,                           lty=1, ncol=3, bty="n", cex=0.75, pt.cex=1.5)
    text(155,   5,   c("Standard elite cultivar"), cex=0.9)
    legend(295, 4,   legend=rep("",3), pch=unique(my.pch), col=my.col.b, pt.bg=my.col.f[c(2,4,6)], lty=4, ncol=3, bty="n", cex=0.75, pt.cex=1.5)
    text(170,   3,   c("High-yield ideotype"),     cex=0.9)
    text(195,   1.5, c("Reported mean"), cex=0.9)
    legend(305, 2.5, legend="",       pch=4,              col="red",      bty="n", cex=0.75, pt.cex=1.5)
  }

}

# Add axis titles
mtext("N fertilizer rate (kg N/ha)",                                  side=1,  cex=0.75, line=-26.5,   outer=T, adj=0.55)
mtext("Global average\n   grain yield (t /ha)",                       side=2,  cex=0.75, line=-2.3,  outer=T, adj=0.70, las=0)
mtext("Global average N use  \nefficiency (kg grain /kg N)",          side=2,  cex=0.75, line=-20,   outer=T, adj=0.70, las=0)
mtext("Global average grain protein \nconcentration (%)", side=2, cex=0.75, line=-37.7, outer=T, adj=0.70, las=0)

# plot %change in yield and N demand  
#-----------------------------------               

my.col.b <- c("goldenrod4","darkmagenta", "blue2")
my.col.f <- c("goldenrod1","magenta", "deepskyblue")

y.min <- c(-15, -15, -15)
y.max <- c(150, 150, 150)
at <- c(1, 5, 9)
i.v <- 0
for(v in unique(var[c(4:6)]))  # v = "Yield"
{
  i.v <- i.v + 1
  index <- 0
  for(r in c("00","45","85"))  #  r ="45"
  {
    index <-  index + 1
    ifelse(r == "00", add <- F, add <- T)
    # % change in yield due to high yield trait for variables calculated as  national N rate 
    boxplot(pc.no.N.adapt[, paste0("pc.global.", v, "(",r,")")],   
            at=at[index], col=my.col.f[1], border=my.col.b[1], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.2,12), ylim=c(y.min[i.v],y.max[i.v]), 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=0.5*l.wd, staplelwd=0.5*l.wd, add=add)
    # % change in yield due to high yield trait for variables calculated as  national N rate 
    boxplot(pc.N.adapt[, paste0("pc.global.", v, "(",r,")")],  
            at=at[index]+1, col=my.col.f[2], border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=0.5*l.wd, staplelwd=0.5*l.wd, add=TRUE)
    # Variables calculated at the national N rate for high yielding cultivar
    boxplot(pc.N.adapt.gpc[, paste0("pc.global.", v, "(",r,")")],  
            at=at[index]+2, col=my.col.f[3], border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=0.5*l.wd, staplelwd=0.5*l.wd, add=TRUE)
  }
  abline(h=0)
  
  # Add letter
  legend(x=-0.5, y=1*y.max[i.v], legend=let[i.v+3], bty="n", cex=1)
  
  # Add Y axis
  if(i.v == 1) axis(2, cex.axis=1, las=1, mgp=c(0,0.5,0)) 
  if(i.v == 2) axis(2, cex.axis=1, las=1, mgp=c(0,0.5,0)) 
  if(i.v == 3) axis(2, cex.axis=1, las=1, mgp=c(0,0.5,0)) 
  
  # Add X axis titles
  axis(1, at=c(2,6,10), labels=c("Base-\nline", "RCP\n4.5", "RCP\n8.5"), cex.axis=1, las=0, mgp=c(0,1.1,0))

  # Add legend
  if(i.v == 2)
  {
    legend(x=0.3, y=135, pch=15, col=my.col.f[1], bty="n", cex=0.82, pt.cex=1.5, 
           legend="Current N application rate")
    legend(x=0.3, y=120, pch=15, col=my.col.f[2], bty="n", cex=0.82, pt.cex=1.5,
           legend="N application rate to maximize\ngrain yield")
    legend(x=0.3, y=95, pch=15, col=my.col.f[3], bty="n", cex=0.82, pt.cex=1.5,
           legend="N application rate to maximize grain\nyield with grain protein > 12%")
   }
}

# Add axis titles
mtext("Climate scenario",                                                         side=1, adj=0.55, cex=0.75,        line=-8.5,    outer = T)
mtext("    Relative change in global grain\n production from high-yield ideotype (%)", side=2, adj=0.27, cex=0.75, las=0, line=-2.3,  outer = T)
mtext("Relative change in global N uptake\n from high-yield ideotype (%)",        side=2, adj=0.27, cex=0.75, las=0, line=-20,   outer = T)
mtext("Relative change in global N fertilizer\n from high-yield ideotype (%)",    side=2, adj=0.27, cex=0.75, las=0, line=-37.7, outer = T)

dev.off()
