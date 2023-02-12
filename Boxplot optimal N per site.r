
# create a data frame for each mode of calculation of optimum N
fao.dose  <- as.data.frame(dt[["fao.dose"]])
opt       <- as.data.frame(dt[["opt"]])

# Remove MME stats
fao.dose  <- fao.dose[-which(fao.dose$model %in% c("e.median","q.25","q.75")),]
opt       <- opt[-which(opt$model      %in% c("e.median","q.25","q.75")),]

site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

# Define plot layout
pl <- c(1:34,35,35)
plotmat <- matrix(pl, 6, 6, byrow=T)
# layout.show(layout(plotmat))

my.col.b <- c("goldenrod4", "blue2","goldenrod4", "blue2")
my.col.f <- c("white","white","goldenrod1","deepskyblue")
l.wd <- 0.6  # width of the boxplot lines
wd <- 1.1  # box width
at <- c(1.5, 7, 12.5) 

i.v <- 0
for(v in unique(var))  # v = "Yield"
{
  i.v <- i.v + 1
  pdf(paste0(main.wd , "Data_Analysis\\Boxplot absolue value per site\\", v, ".pdf"), paper="a4", width=9, height=13)
  par(mar=c(0.3,0.3,0.3,0.3), oma=c(5,4,0,0), tck=0.02 , xaxs="i", yaxs="i",las=1)
  layout(plotmat , heights = c(1,1,1,1), widths = c(1,1,1))
  
  for(r in c("00","45","85"))   #  r ="00"
  {
    y <- c(fao.dose[, paste0(v, "(", r, "N)")] , opt[, paste0(v, "(", r, "N)")],
           fao.dose[, paste0(v, "(", r, "T)")],  opt[, paste0(v, "(", r, "T)")])
    if(!exists('Y')) { Y <- y } else { Y <- c(Y, y) }
  }
  IQR <- summary(Y)[5] -summary(Y)[2] 
  y.lim <- range(y,na.rm=T)
  # y.lim <- c(summary(Y)[2] - 1.5 * IQR, summary(Y)[5] + 1.5 * IQR)
  rm(Y,y)

  for(s in 1:34)   #  s=1
  {
    index <- 0
    for(r in c("00","45","85"))  #  r ="00"
    {
      index <-  index + 1
      ifelse(r == "00", add <- F, add <- T)
      # Variables calculated the national N rate for standard cultivar
      boxplot(fao.dose[fao.dose$site==s, paste0(v, "(", r, "N)")],   
              at=at[index], col=my.col.f[1], border=my.col.b[1], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.5,16.5), ylim=y.lim, 
              width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=add)
      # Variables calculated the national N rate for high yielding cultivar
      boxplot(fao.dose[fao.dose$site==s, paste0(v, "(", r, "T)")], 
              at=at[index]+1, col=my.col.f[2], border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', 
              width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
      # Variables calculated at N rate to maximize yield for standard cultivar
      boxplot(opt[opt$site==s, paste0(v, "(", r, "N)")], 
              at=at[index]+2, col=my.col.f[3], border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
              width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
      # Variables calculated at N rate to maximize yield for high yielding cultivar
      boxplot(opt[opt$site==s, paste0(v, "(", r, "T)")], 
              at=at[index]+3, col=my.col.f[4], border=my.col.b[4], outline=FALSE, xaxt='n', yaxt='n', 
              width=wd, medlwd=l.wd*2, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    }
    # Add site and country
    legend(0, y.lim[2], legend=paste0(site[site$Site==s,"Site.name"], ", ", site[site$Site==s,"Country.code"]),bty="n")
    # Add Y axis
    if(s%in%c(1,7,13,19,25,31))
    {
      axis(2, cex.axis=1, las=1)    
    }
    
    # Add X axis
    if(s%in%c(31,32,33,34,29,30))
    {
      axis(1, at=c(2,8,14), labels=c("Base\nline", "RCP\n4.5", "RCP\n8.5"), cex.axis=1, las=0)
    }
  }
  # Add axis titles
  mtext(var.name[i.v], side=2, adj=0.5, cex=1, las=0, line =2.5, outer = T)
  
  # Add legend
  plot.new()
  legend("center", legend=c("Standard elite cultivar with current N application",
                            "High-yield ideotype with\ncurrent N application",
                            "Standard elite cultivar with N adaptation",
                            "High-yield ideotype with\ncurrent N application"),
         pch=22 ,bty="n",pt.cex=2, cex=1, col=my.col.b , pt.bg=my.col.f, text.col="black", inset = 0.1)
  
  dev.off()
}

# rm(v, s, r, site, pl, plotmat, my.col.b, my.col.f, l.wd, wd, at, index, add, y.lim)
