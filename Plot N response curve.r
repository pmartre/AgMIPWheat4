#----------------------
# get site information
# add scaled wheat area
#----------------------

site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

#----------------
# Calculate mean
# over the 5 GCMs
#----------------

dt <-  as.data.frame(data.table(dt)[, lapply(.SD, mean), by=list(model, trait, ferti, site, rcp), .SDcols=var ])
dt$trait.rcp     <- paste(dt$trait,dt$rcp,sep=".")

#-----------
# N response
# curve per
# site
#-----------

site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

# Define plot layout
pl <- c(1:34,35,35)
plotmat <- matrix(pl, 6, 6, byrow=T)
# layout.show(layout(plotmat))

col.t <- c("black","red","blue","green4")
cropN.opt <- c(NA,NA)

for(m in "e.median") #unique(dt$model))  # m="e.median"
{
  pdf(paste0(main.wd , "Data_Analysis\\N response curve\\", m, "_", " response to N.pdf"), paper="a4", width=9, height=13)
  par(mar=c(0.3,0.3,0.3,0.3), oma=c(5,4,0,0), tck=0.02 , xaxs="i", yaxs="i",las=1)
  layout(plotmat , heights = c(1,1,1,1), widths = c(1,1,1))
  
  for(v in var) # v="Yield" 
  {
    for(i in 1:34)   # i=1
    { 
      index <- 0
      for(t in c("N.00", "T.00", "T.45", "T.85"))   # t = "N.00"
      { 
        y <- dt[,v][dt$model   == m & dt$site == i & dt$trait.rcp == t]
        if(all(is.na(y)) == T) next
        ferti   <- dt$ferti[dt$model   == m & dt$site == i & dt$trait.rcp == t]
        d.plot  <-data.frame(cbind(y, ferti))
        ave.y <- c(tapply(d.plot$y, d.plot$ferti,mean))
        dose <-c(seq(0,450,50))
        index   <- index + 1
        
        # Plot data
        y.max <- max(dt[,v][dt$model==m ], na.rm=T)
        if(v == "NUE") y.max = 42
        if(t == "N.00")  plot(ave.y ~ dose, 
                             ylab="", 
                             xlab="",
                             xaxt="n",
                             yaxt="n",
                             ylim = c(0, 1.05 * y.max),
                             cex=0.8,
                             col = col.t[index])
        if(t %in% c("T.00", "T.45", "T.85"))  points(ave.y ~ dose, cex=0.8,col=col.t[index])
        
        # plot a spline 
        if(!any(is.na(ave.y)))
        {
          p_y.VS.dose <- predict(smooth.spline(ave.y ~ dose, cv=FALSE, all.knots = FALSE), x= seq(0,450, by=1) )
          points(p_y.VS.dose, type = "l",col=col.t[index])
          if(v == "Yield") p_yield.VS.dose <- p_y.VS.dose
        
          # Estimated yield at optimum N dose
          if(v == "Yield")
          {
            yield.opt <- round(f.opti * min(max(y.max), p_y.VS.dose$y[which.max(p_y.VS.dose$y)]), 2)
            dt[dt$model   == m & dt$site == i & dt$trait.rcp == t, "dose.opt"]  <- p_yield.VS.dose$x[which.min(abs(p_yield.VS.dose$y - yield.opt))]
          }
          # Add verticale line for dose at maximum yield
          # points(rep(dt[dt$model   == m & dt$site == i & dt$trait.rcp == t, "dose.opt"][1], 2), c(0, 0.93*y.max), type="l", lty=2, col=col.t[index])
        }
      }
      if(index == 0) next
      
      # Add site and country
      legend(-65, 1.05 * y.max, legend=paste0(site[site$Site==i,"Site.name"], ", ", site[site$Site==i,"Country.code"]),bty="n")
      
       # Add Y axis
      if(i%in%c(1,7,13,19,25,31))
      {
        axis(2, cex.axis=1, las=1)
      }
      
      # Add X axis
      if(i%in%c(31,32,33,34,29,30))
      {
        axis(1, at=seq(0,450,100), labels=seq(0,450,100), cex.axis=1, las=0)
      }
      # Add a cross at reported national N yield and N fertilizer rate
      if(v == "Yield")
      {
        points(site[site$Site==i, "nat.n.fertilizer"], site[site$Site==i, "Country.yield"], pch = 4, col='red', cex = 2)
      }
      # if(v == "NUE")
      # {
      #   Global.NUE <- 0.49 * 1000 * site[site$Site==i, "Country.yield"] / site[site$Site==i, "nat.n.fertilizer"]  
      #   points(site[site$Site==i, "nat.n.fertilizer"], Global.NUE, pch = 4, col='red', cex = 2)
      # }  
    }
    if(index != 0)
    {
      # Add axis titles
      mtext("N fertilizer rate (kg N/ha)",  side=1, adj=0.5, cex=1, las=0, line =2.5, outer = T)
      mtext(y.lab[y.lab$var == v, "y.lab"], side=2, adj=0.5, cex=1, las=0, line =2.5, outer = T)
      
      # Add legend
      plot.new()
      legend("center", legend=c("Standard elite cultivar, Baseline",
                                 "High-yield ideotype, Baseline",
                                 "High-yield ideotype, RCP4.5",
                                 "High-yield ideotype, RCP8.5"),
             pch=1 ,bty="n",cex=1, col= col.t, text.col="black", inset = 0.1)
    }
  }
  dev.off()
}
rm(pl, col.t, m, v, t, i, yield.opt, cropN.opt, plotmat, site, y, ferti, ave.y, d.plot, dose, p_y.VS.dose, y.max, index, p_yield.VS.dose)
