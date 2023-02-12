#--------------------------
# Plot soil characteristics
# for the 34 Global sites
#--------------------------

# Define plot layout
pl <- c(1:34,35,35)
plotmat <- matrix(pl, 6, 6, byrow=T)
# layout.show(layout(plotmat))

index <- 0
for(v in var) # v="slll" 
{
  index   <- index + 1
  
  pdf(paste0(main.wd , "Data_Analysis\\Figures for paper\\", pdf.name[index]), paper="a4", width=9, height=13)
  par(mar=c(0.3,0.3,0.3,0.3), oma=c(5,4,0,0), tck=0.02 , xaxs="i", yaxs="i",las=1)
  layout(plotmat , heights = c(1,1,1,1), widths = c(1,1,1))
  
  for(i in 1:34)   # i=1
  { 
    x <- soil[soil$site == i, v]
    y <- soil[soil$site == i, "sllb"]
      
    # Plot data
    x.max <- max(soil[, v], na.rm=T)
    if(v == "slll") x.max <- max(soil[, "slsat"], na.rm=T)
    y.max <- max(soil[, "sllb"], na.rm=T)
    plot(x,y, 
         ylab="", 
         xlab="",
         xaxt="n",
         yaxt="n",
         ylim = rev(c(-20, 1.05 * y.max)),
         xlim = c(0, 1.05 * x.max),
         type="l",
         col="chocolate4")
      if(v ==  "slll")
      {
        points(soil[soil$site == i, "sldul"], y, type="l",col="darkorange")     
        points(soil[soil$site == i, "slsat"], y, type="l",col="darkgoldenrod1")
      }
      
    # Add site and country
    legend(0, -25, legend=paste0(site[site$Site==i,"Site.name"], ", ", site[site$Site==i,"Country.code"]),bty="n")
    
    # Add Y axis
    if(i%in%c(1,7,13,19,25,31))
    {
      axis(2, cex.axis=1, las=1)
    }
    
    # Add X axis
    if(i%in%c(31,32,33,34,29,30))
    {
      axis(1, cex.axis=1, las=0)
    }
  }
  if(index != 0)
  {
    # Add axis titles
    mtext("Soil depth (cm)",  side=2, adj=0.5, cex=1, las=0, line =2.5, outer = T)
    mtext(x.lab[index],       side=1, adj=0.5, cex=1, las=0, line =2.5, outer = T)
    
    # Add legend
    if(v ==  "slll")
    {
      plot.new()
      legend("center", legend=c("saturated water content", "drained upper limit", "drained lower limit"),
             lty=1,bty="n",cex=1, col= c("darkgoldenrod1", "darkorange",          "chocolate4"), text.col="black", inset = 0.1)   # c("chocolate4", "darkorange", "darkgoldenrod1")
    }
  }
  dev.off()
}

