# create a data frame for each mode of calculation of optimum N
no.N.adapt   <- as.data.frame(dt[["fao.dose"]])
no.N.adapt   <- no.N.adapt[-which(no.N.adapt$model %in% c("e.median","q.25","q.75")),]
N.adapt      <- as.data.frame(dt[["opt"]])
N.adapt      <- N.adapt[-which(N.adapt$model      %in% c("e.median","q.25","q.75")),]
N.adapt.gpc  <- as.data.frame(dt[["opt.gpc"]])
N.adapt.gpc  <- N.adapt.gpc[-which(N.adapt.gpc$model      %in% c("e.median","q.25","q.75")),]

site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

# Define plot layout
pl <- c(1:34,35,35)
plotmat <- matrix(pl, 6, 6, byrow=T)
# layout.show(layout(plotmat))

my.col.b <- c("goldenrod4","darkmagenta", "blue2")
my.col.f <- c("goldenrod1","magenta", "deepskyblue")
l.wd <- 0.6  # width of the boxplot lines
wd <- 1.1  # box width
at <- c(1, 5, 9)  


i.v <- 0
for(v in unique(var))  # v = "Yield"
{
  i.v <- i.v + 1
  pdf(paste0(main.wd , "Data_Analysis\\Boxplot percent change per site\\", v, ".pdf"), paper="a4", width=9, height=13)
  par(mar=c(0.3,0.3,0.3,0.3), oma=c(5,4,0,0), tck=0.02 , xaxs="i", yaxs="i",las=1)
  layout(plotmat , heights = c(1,1,1,1), widths = c(1,1,1))
 
  for(r in c("00","45","85"))   #  r ="00"
  {
    y <- c(no.N.adapt[, paste0("pc.", v, "(",r,")")], N.adapt[, paste0("pc.", v, "(",r,")")])
    if(!exists('Y')) { Y <- y } else { Y <- c(Y, y) }
  }
  
  y.lim <- c(quantile(y,probs=0.005,na.rm=T), quantile(y,probs=0.995,na.rm=T))
  
  for(s in 1:34)   #  s = 1
  {
    index <- 0
    for(r in c("00","45","85"))  #  r ="00"
    {
      index <-  index + 1
      ifelse(r == "00", add <- F, add <- T)
      # Variables calculated the national N rate for standard cultivar
      boxplot(no.N.adapt[no.N.adapt$site==s, paste0("pc.", v, "(",r,")")],   
              at=at[index], col=my.col.f[1], border=my.col.b[1], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0,12), ylim=y.lim, 
              width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=add)
      # Variables calculated the national N rate for high yielding cultivar
      boxplot(N.adapt[N.adapt$site==s, paste0("pc.", v, "(",r,")")],  
              at=at[index]+1, col=my.col.f[2], border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', 
              width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
      # Variables calculated the national N rate for high yielding cultivar
      boxplot(N.adapt.gpc[N.adapt.gpc$site==s, paste0("pc.", v, "(",r,")")],  
              at=at[index]+2, col=my.col.f[3], border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
              width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    }
    abline(h=0)
    
    # Add site and country
    legend(0, 1.05 * y.lim[2], legend=paste0(site[site$Site==s,"Site.name"], ", ", site[site$Site==s,"Country.code"]),bty="n")
    
    # Add Y axis
    if(s%in%c(1,7,13,19,25,31))
    {
      axis(2, cex.axis=1, las=1)    
    }
    
    # Add X axis
    if(s%in%c(31,32,33,34,29,30))
    {
      axis(1, at=c(1.5,6,10), labels=c("Base\nline", "RCP\n4.5", "RCP\n8.5"), cex.axis=1, las=0)
    }
  }
  # Add axis titles
  mtext(paste0("Relative change in ", var.name[i.v], " for high-yield ideotype (%)"),
        side=2, adj=0.5, cex=1, las=0, line =2.5, outer = T)
  
  # Add legend
  plot.new()
  legend("center", legend=c("Current N application rate",
                            "N application rate to maximize\ngrain yield\n",
                            "N application rate to maximize\ngrain yield with grain protein > 12%"),
         pch=22, col=my.col.b , pt.bg=my.col.f,  bty="n", cex=1.1, pt.cex=3)
  
  dev.off()
}

rm(v, s, site, pl, plotmat, my.col.b, my.col.f, l.wd, wd, at, index, y.lim, y)
