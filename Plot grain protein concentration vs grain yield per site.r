# get site information add scaled wheat area
site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")


# Read simulated yield interpolated at national fertilization rate 
res.opt.30y <- readRDS(paste0(DataAnalysis.wd,  "CropNtoMaximizeYield_30yrsAve.RDS"))

# Get simulated yield interpolated at national fertilization rate from list  
get.data <- function(data){ # data="current.dose"
  dt <- res.opt.30y[[data]]
  dt <- dt[which(dt$model%in% c("e.median","q.25","q.75") ), c("site","trait","rcp","gcm","model","Yield", "GPC", "NUtE")]
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

# Define plot layout
pl <- c(1:34,35,35)
plotmat <- matrix(pl, 6, 6, byrow=T)
# layout.show(layout(plotmat))

trait <- c("N",              "N",           "N",           "T",              "T",           "T")
rcp   <- c("00",             "45",          "45",          "00",             "45",          "85")
col   <- c("darkgoldenrod1", "darkorange",  "chocolate4",  "darkgoldenrod1", "darkorange",  "chocolate4")
col.b <- c("goldenrod4",     "darkmagenta", "blue2",       "goldenrod4",     "darkmagenta", "blue2")
col.f <- c("goldenrod1",     "magenta",     "deepskyblue", "goldenrod1",     "magenta",     "deepskyblue")


x.var   <- c("Yield", "NUtE", "Yield")
y.var   <- c("GPC",   "GPC",  "GrainN")
fac.xmax <- c(1.05,    1.20,    1.05)

for(i.var in 1:3) # i.var = 3
{

  pdf(paste0(main.wd , "Data_Analysis\\Figures for paper\\Fig S2. ", y.var[i.var], " vs ", x.var[i.var], ".pdf"))
  par(mar=c(0.3,0.3,0.3,0.3), oma=c(5,4,0,0), tck=0.02 , xaxs="i", yaxs="i",las=1)
  layout(plotmat , heights = c(1,1,1,1), widths = c(1,1,1))
  
  ylim <- c(0.95 * min(current.dose[which(current.dose$model == "e.median"), y.var[i.var]], opt[which(opt$model == "e.median"), y.var[i.var]]),
            1.05 * max(current.dose[which(current.dose$model == "e.median"), y.var[i.var]], opt[which(opt$model == "e.median"), y.var[i.var]]))

  xlim <- c(0.95 *            min(current.dose[which(current.dose$model == "e.median"), x.var[i.var]], opt[which(opt$model == "e.median"), x.var[i.var]]),
            fac.xmax[i.var] * max(current.dose[which(current.dose$model == "e.median"), x.var[i.var]], opt[which(opt$model == "e.median"), x.var[i.var]]))
  
  for(i in 1:34)   # i=1
  { 
    # Create empty plot without axis
    plot(0, 0,
         ylab="",
         xlab="",
         xaxt="n",
         yaxt="n",
         ylim = ylim,
         xlim = xlim,
         pch="")
    
    
    # Grain N isoplete lines
    if(i.var == 1)
    {
      for(i.grainN in seq(50,250,25)) # i.grainN = 50
      {
        yield    <- seq(0.1,100,0.1)
        isolpete <- ( i.grainN * 5.62 * 100 ) / ( 1000 * yield )
        # yield it corrected to 13% moisture content
        lines( (1/(1-gmc)) * yield[isolpete < 12], isolpete[isolpete < 12], lty = "dotted", col = "grey")
        if(i.grainN == 50)  legend(4.2,    7.2,  legend = paste0("   ", i.grainN, "\nkg N/ha"), bty="n", cex=0.5)
        if(i.grainN == 250) legend(12.9, 13.1, legend = paste0("   ", i.grainN, "\nkg N/ha"), bty="n", cex=0.5)
      }
    }
    
    # Add equation of GPC vs NUtE from Savin et al., (2019) - https://doi.org/10.1016/j.fcr.2019.107573 
    if(i.var == 2)
    {    
      NUtE <- seq(10,100, 1)
      GPC  <- 5.62 * ( 6.98 - 1.36 * log(NUtE))
      lines(NUtE[GPC < 12], GPC[GPC < 12], lwd=0.8, col = "grey")
    }
    
    # Add GPC isopletes= lines
    if(i.var == 3)
    {
      for(i.gpc in seq(6,13,1)) # i.gpc = 12
      {
        yield    <- seq(0.1,100,0.1)
        isolpete <- ( i.gpc * ( 1000 * yield ) ) / ( 5.62 * 100 )
        # yield it corrected to 13% moisture content
        lines( (1/(1-gmc)) * yield[isolpete < 300], isolpete[isolpete < 300], lty = "dotted", col = "grey")
        if(i.gpc == 9)  legend(12, 135,  legend = paste0("9%"), bty="n", cex=0.5)
        if(i.gpc == 13) legend(7,  240, legend = paste0("13%"), bty="n", cex=0.5)
      }
    }
    
    # Function to add data to plot
    add.data <- function(dt, j){  # dt = current.dose ; j = 1
      x    <- dt[which(dt$site==i & dt$trait==trait[j]  & dt$rcp==rcp[j]  &  dt$model=="e.median"), x.var[i.var]]
      y    <- dt[which(dt$site==i & dt$trait==trait[j]  & dt$rcp==rcp[j]  &  dt$model=="e.median"), y.var[i.var]]
      points(x, y, pch = pch[j], col = col.f[j], bg = col.b[j])
      # add error bars
      # x.25 <- dt[which(dt$site==i & dt$trait==trait[j]  & dt$rcp==rcp[j]  &  dt$model=="q.25"),     i.var]
      # x.75 <- dt[which(dt$site==i & dt$trait==trait[j]  & dt$rcp==rcp[j]  &  dt$model=="q.75"),     i.var]
      # y.25 <- dt[which(dt$site==i & dt$trait==trait[j]  & dt$rcp==rcp[j]  &  dt$model=="q.25"),     "GPC"]
      # y.75 <- dt[which(dt$site==i & dt$trait==trait[j]  & dt$rcp==rcp[j]  &  dt$model=="q.75"),     "GPC"]
      # arrows(y0 = y.25, y1 = y.75, x0 = x.25 , x1 = x.75, 
      #        col = col.b[j], angle = 90, length = 0.03, code = 3, lwd=1)
    }
    
    # Add data to plot     for(j in 1:6)
    for(j in c(1:6))  # j = 4
    {
      # @ current N dose
      pch <- c(rep(1,3), rep(19,3))
      add.data(current.dose, j)  
      # @ N dose to maximize yield
       pch <- c(rep(2,3), rep(17,3))
       add.data(opt, j)
      # # @ N dose to maximize yield with GPC > 12%
      # pch <- c(rep(0,3), rep(15,3))
      # add.data(opt.gpc, j)
    }
    
    # Add site and country
    if(i.var == 1)   legend(0.7*xlim[1], 1.02*ylim[2], legend=paste0(site[site$Site==i,"Site.name"], ", ", site[site$Site==i,"Country.code"]),bty="n", cex=0.8)
    if(i.var == 2)   legend(1.1*xlim[1], 1.02*ylim[2], legend=paste0(site[site$Site==i,"Site.name"], ", ", site[site$Site==i,"Country.code"]),bty="n", cex=0.8)
    if(i.var == 3)   legend(1.1*xlim[1], 1.02*ylim[2], legend=paste0(site[site$Site==i,"Site.name"], ", ", site[site$Site==i,"Country.code"]),bty="n", cex=0.8)
    # Add Y axis
    if(i%in%c(1,7,13,19,25,31))
    {
      axis(2, cex.axis=0.8, las=1)
    }
    
    # Add X axis
    if(i%in%c(31,32,33,34,29,30))
    {
      axis(1, cex.axis=0.8, las=0)
    }
  }
  # Add axis titles
  if(i.var %in% c(1,2))  mtext("Simulated grain protein concentration (%)",                 side=2, adj=0.5, cex=0.8, las=0, line =2.5, outer = T)
  if(i.var == 3)         mtext("Simulated grain nitrogen yield (kg N/ha)",                  side=2, adj=0.5, cex=0.8, las=0, line =2.5, outer = T)
  
  if(i.var %in% c(1,3)) mtext("Simulated grain yield (t/ha)",                              side=1, adj=0.5, cex=0.8, las=0, line =2.5, outer = T)
  if(i.var == 2)        mtext("Nitrogen utilisation efficiency (kg grain DM / kg crop N)", side=1, adj=0.5, cex=0.8, las=0, line =2.5, outer = T)
  
  # Add legend
  plot(0, 0,
       ylab="", 
       xlab="",
       axes=F,
       ylim = c(0, 15),
       xlim = c(0, 20),
       pch="")
  # Basline
  text(15.10, 8.9, "base-\nline", cex=0.6)
  legend(14.15,8.6, legend=rep("",4), pch=c(1, 2, 19, 17),  bty="n",cex=0.8, col=rep(col.f[1],6), bg=rep(col.b[1],6))
  # RCP45
  text(17.26, 8.9, "RCP\n4.5", cex=0.6)
  legend(16.4,8.6,  legend=rep("",4), pch=c(1, 2, 19, 17),  bty="n",cex=0.8, col=rep(col.f[2],6), bg=rep(col.b[2],6))
  # RCP8.5
  text(18.99, 8.9, "RCP\n8.5", cex=0.6)
  legend(18.4,8.6,  legend=rep("",4), pch=c(1, 2, 19, 17),  bty="n",cex=0.8, col=rep(col.f[3],6), bg=rep(col.b[3],6))
  x=14 ; y = 7.1 ; s = 1.7
  text(x, y - 0 * s, c("Current N application"),         adj=1, cex=0.6)
  text(x, y - 1 * s, c("N application to maximize grain yield"),                adj=1, cex=0.6)
  text(x, y - 2 * s, c("Current N application"),         adj=1, cex=0.6)
  text(x, y - 3 * s, c("N application to maximize grain yield"),                adj=1, cex=0.6)
  mtext("Standard\nelite\ncultivar", side=2, adj=0.50, cex=0.45, las=0, line =-1.75)
  mtext("  High\n  yield\nideotype",     side=2, adj=0.07, cex=0.45, las=0, line =-1.75)
  lines(c(2.7,2.7),c(5,9.5), lwd=0.8)
  lines(c(2.7,2.7),c(0.2,4.5), lwd=0.8)
  
  dev.off()
}

