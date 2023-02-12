# create a data frame for each mode of calculation of optimum N
dt  <- as.data.frame(dt[["opt"]])

#----------------------
# get site information
# add scaled wheat area
#----------------------

site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")
dt$area.weight <- site$area.weight[match(dt$site, site$Site)]

#-----------------------
# Create data frame
# with site lat and long
# and MME median and cv
# for plotting in a map
#-----------------------

# get e.median yield for std cultivar under baseline and calculate %CV
var.map     <- dt[which(dt$model=="e.median" & dt$gcm=="1"), c("site", paste0(var,"(00N)"))]
q25         <- dt[which(dt$model=="q.25"     & dt$gcm=="1"), c("site", paste0(var,"(00N)"))]
q75         <- dt[which(dt$model=="q.75"     & dt$gcm=="1"), c("site", paste0(var,"(00N)"))]
var.map$q25 <- q25[,paste0(var,"(00N)")][match(var.map$site, q25$site)]
var.map$q75 <- q75[,paste0(var,"(00N)")][match(var.map$site, q25$site)]
rm(q25, q75)

# Remove MME
dt <- dt[-which(dt$model%in%c("e.median","q.25","q.75")),]

site$Median <- var.map[,paste0(var,"(00N)")][match(site$Site, var.map$site)] 
# site$CV     <- var.map[,"cv"][match(site$Site,    var.map$site)] 
rm(var.map)

#------------------------
# Calculate area weighted
# global wheat yield
#------------------------

v <- c(paste0(var,"(00N)"), paste0(var,"(00T)"),
       paste0(var,"(45N)"), paste0(var,"(45T)"),
       paste0(var,"(85N)"), paste0(var,"(85T)"))
for( i in v)  #  i = "Yield(00N)"
{
  dt[paste0("wt.", i)] <- dt$area.weight * dt[,i]
}

global.whted.yield <-  as.data.frame(data.table(dt)[, lapply(.SD,  function(x){ sum(x, na.rm=T)}), by=list(model, gcm), .SDcols=paste0("wt.",v)])
rm(i,v)

# define symbol size inversely proportional to %CV
size <- rev(seq(0.50,1.2,0.10))
#  step.cv <- (max(site$CV) - min(site$CV)) / 8
# site$symbol.size <- max(size)
# for(i in 8:1)  # i=1
# {
#   site[which(site$CV < min(site$CV) + i * step.cv), "symbol.size"] <- size[i]
# }

# Shift site name lat and long to show them on the map
#                                1   2   3   4   5   6   7  8   9  10   11  12  13  14   15  16  17
site$Long.sift <- site$Long + c(-2, -2, -6,  6, -6, -6, -6, 2, 23,  17,  -6, -6, -6, -6, -12, 12, -6,
#                               18  19  20  21  22  23  24  25 26  27  28  29  30  31  32  33  34  
                                -6, -6, -6, -6, -6, -6, -6, -3, 6, -6, 15, -6,  5, -11, -15, 15, -6)
#                              1   2   3   4   5   6   7   8  9    10   11  12   13   14  15    16   17
site$Lat.sift <- site$Lat + c(-2, -2, -2, 3, -2, -2, -2, -2, 0.5, 1,  -2, -2, -1.5, -2, 2.5, -0.5, -2,
#                            18  19  20  21  22  23  24  25    26  27  28     29  30  31  32 33  34  
                              3, -2, -2,  3,  3,  3, -2,  2.5, -2, -2,  0.5,  -2, -2,  2, -2, 3, -2)
#    plot position  1   2   3   4   5   6   7   8  9  10  11  13  14  15 16  17
site$site.lay <- c(20, 22, 24, 29, 30, 31, 32, 23, 5, 27, 21, 25, 16, 18, 2, 4,
#                 18  19  20  21  22  23  24  25  26 27 28 29  30  31  32  33  34  35  
                   3,  6, 15, 13, 10, 33, 34, 17, 19, 9, 8, 7, 11, 14,  1, 26, 28, 35)

# Add an "*" after the name of the irrigated sites
site$site.name.irr <- site$Site.name
site[which(site$irrigation==1),"site.name.irr"] <- paste0(site[which(site$irrigation==1),"Site.name"], "*")

dt$site.lay <- site$site.lay[match(dt$site,site$Site)]

#-----------------------
# Map of yield potential
# (baseline line yield)
#-----------------------

sf::sf_use_s2(FALSE) # coord_sf() returns an error message if S2 is not desactivated
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
geom_sf(data = world, fill = "gray88", size=0.1, colour="gray69", stat="sf") +
theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        aspect.ratio = 1,
        legend.position = c(0.96, 0.55),
        legend.title=element_blank(),
        legend.background=element_blank()) +
  # Set the limits of the map
  coord_sf(xlim = c(-125, 180), ylim = c(-53, 61) , expand = FALSE,  default_crs = sf::st_crs(4326)) +
  # Add yield for baseline standard cultivar
  geom_point(data = site, aes(x = Long, y = Lat, colour=Median, size=4), alpha=1) +
  guides(size="none") +
  # Add legeng
  scale_color_gradient2(low = 'darkblue', mid = "yellow", high = 'red',midpoint = 8.5) +
  # Add site names
  annotate(geom="text", x=site$Long.sift, y=site$Lat.sift, label=site$site.name.irr, 
           color="darkblue", size=2.5, fontface="bold" ) +
  # Add Legend names  
  annotate(geom="text", x=162 , y=25 ,   label="Nitrogen\nunlimited grain\nyield (t/ha)", size=2.5, fontface="bold") 
  # annotate(geom="text", x=-112 , y=-28.5 , label="%CV",     size=3.5, fontface="bold")

ggsave(paste0(DataAnalysis.wd, "Maps\\map.jpeg"))
map <- readJPEG(paste0(DataAnalysis.wd, "Maps\\map.jpeg"),native = TRUE)
rm(world)
  
#-------------------------------
# add boxplots around the map of
# % change in response to traits
#  under baseline and CC
#-------------------------------

my.col.b <- c("goldenrod4","darkmagenta", "blue2")
my.col.f <- c("goldenrod1","magenta", "deepskyblue")
l.wd <- 0.6  # width of the boxplot lines
wd <- 1.1  # box width

v <- c(1:11,
       rep(12,8),13,14,
       rep(12,8),15,16,
       rep(12,8),17,18,
       rep(12,8),19,20,
       rep(12,8),21,22,
       rep(12,8),23,24,
       rep(12,8),25,26,
       27:35)
plotmat <- matrix(v, 9, 10, byrow=T)
layout.show(layout(plotmat))

#----------
# Final map
#----------

pdf(paste0(DataAnalysis.wd, "Figures for paper/", title, ".pdf"))
par(mar=c(0.2,0.2,0.2,0.2), oma=c(0,0,0,0), tck=0.02 ,las=1, yaxs="i", xaxs="i")
layout(plotmat)
for(i in seq(1:35)) # i=26
{
  if(!(i%in%12))
  {
    # if(i < 12)  { s <- i } else { s <- i - 1 }
    boxplot(dt[dt$site.lay==i & dt$gcm==1, paste0(var, "(00N)")],   
            at=1, col="white",   border=my.col.b[1], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.25,8.75), ylim=y.lim, xlab="", ylab="",
            width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd)
    boxplot(dt[dt$site.lay==i & dt$gcm==1, paste0(var, "(00T)")], 
            at=2, col=my.col.f[1], border=my.col.b[1], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    
    boxplot(dt[dt$site.lay==i, paste0(var, "(45N)")],   
            at=4, col="white",   border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    boxplot(dt[dt$site.lay==i, paste0(var, "(45T)")], 
            at=5, col=my.col.f[2], border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
     
    boxplot(dt[dt$site.lay==i, paste0(var, "(85N)")],   
            at=7, col="white",   border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    boxplot(dt[dt$site.lay==i, paste0(var, "(85T)")], 
            at=8, col=my.col.f[3], border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
            width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
    
    abline(h=0)
    axis(1, at=c(1.5,4.5,7.5), cex.axis=0.7,  labels=F)
    axis(2, cex.axis=1,  labels=F)
    legend(0, 1.05*y.lim[2], legend= site[which(site$site.lay==i),"site.name.irr"],bty="n", cex=0.8)
  } else
  {
    plot(0:1,0:1,pch="", xaxt='n', yaxt='n', ann=FALSE, frame.plot=F)
    # Insert map
    addImg(map, x = 0.5, y = 0.5, width = 1)
    if(grepl("Yield", var) == T)
    {
      # Line for Obregon, Mexico
      arrows(x0=0.095,  y0=0.702,  x1=0.144, y1=0.732, col="darkblue", angle=0,lwd=1)
      # Line for Rots, France
      arrows(x0=0.35,  y0=0.85,   x1=0.375, y1=0.90, col="darkblue", angle=0,lwd=1)
      # Line for Leeston, NZ
      arrows(x0=0.90981, y0=0.058,   x1=0.91, y1=0.058, col="darkblue", angle=0,lwd=1)
      # Line for Valdivia, Chile
      arrows(x0=0.160,  y0=0.130,   x1=0.13, y1=0.165, col="darkblue", angle=0,lwd=1)
      # Line for Buenos Aires, Argentina
      arrows(x0=0.31,  y0=0.175,  x1=0.36, y1=0.08, col="darkblue", angle=0,lwd=1)
    }
  }
}

# Add inset boxplot in the map
par(fig=c(0.54, 0.67, 0.145, 0.245), new=TRUE, mar=c(0,0,0,0), las=1, tck=0.02)
boxplot(global.whted.yield[global.whted.yield$gcm==1, paste0("wt.", var, "(00N)")],
        at=1, col="white", border="white", outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.25,8.75), ylim=y.lim,
        width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd)
# boxplot(global.whted.yield[global.whted.yield$gcm==1, paste0("wt.", var, "(00N)")],
#         at=1, col="white", border=my.col.b[1], outline=FALSE, xaxt='n', yaxt='n', xlim=c(0.25,8.75), ylim=y.lim, 
#         width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd)
# boxplot(global.whted.yield[global.whted.yield$gcm==1, paste0("wt.", var, "(00T)")],
#         at=2, col=my.col.f[1], border=my.col.b[1], outline=FALSE, xaxt='n', yaxt='n',
#         width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
# 
# boxplot(global.whted.yield[, paste0("wt.", var, "(45N)")],
#         at=4, col="white", border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n', 
#         width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
# boxplot(global.whted.yield[, paste0("wt.", var, "(45T)")],
#         at=5, col=my.col.f[2], border=my.col.b[2], outline=FALSE, xaxt='n', yaxt='n',
#         width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
# 
# boxplot(global.whted.yield[, paste0("wt.", var, "(85N)")],
#         at=7, col="white", border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
#         width=wd, medlwd=l.wd, boxlwd=l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
# boxplot(global.whted.yield[, paste0("wt.", var, "(85T)")],
#         at=8, col=my.col.f[3], border=my.col.b[3], outline=FALSE, xaxt='n', yaxt='n', 
#         width=wd, medlwd = l.wd, boxlwd = l.wd, whisklwd=l.wd, staplelwd=l.wd, add=TRUE)
abline(h=0)
axis(1, at=c(1.5,4.5,7.5), labels=c("Base-\nline", "RCP\n4.5", "RCP\n8.5"), cex.axis=0.7, mgp=c(3,0.5,0))
axis(2, cex.axis=0.7, mgp=c(3,0.3,0))
mtext("Nitrogen unlimited\ngrain yield\n(t /ha)", side=2, adj=0.5, cex = 0.60, las=0, line =1)
legend(x=0.5, y=1.05 * max(y.lim), legend=c("Standard\nelite cultivar"), pch=c(0), bty="n", cex=0.8, pt.cex=1.7)
legend(x=0.5, y=0.7 * max(y.lim), legend=c("High-yield\nideotype"), pch=c(15), bty="n", cex=0.8, pt.cex=1.7)

# Add inset barplot of experimental datasets
if(grepl("Yield", var) == T)
{
  # Function for yield barplot
  do.barplot.yield <- function(df){ # df = mme$nz
    if("DH"%in%df$genotype == T) 
    {
      par(lwd=0.75)
      a <- barplot(df[,3], xaxt='n', yaxt='n', col=c("white","darkorange","white","olivedrab1"), border=rep(c("darkorange4","olivedrab"), each=2), lwd= 2,ylim=c(0,20))
      # Interquantile of measurments for Bacanora
      arrows(x0=a[1], y0=df[1,4], x1=a[1], y1=df[1,5], col=c("darkorange4"), angle=90, length=0.02, code=3)
      # Interquantile of simulations for DH
      arrows(x0=a[2], y0=df[2,4], x1=a[2], y1=df[2,5], col=c("darkorange4"), angle=90, length=0.02, code=3)
      # Interquantile of measurments for Bacanora
      if(nrow(df)==4)  arrows(x0=a[3], y0=df[3,4], x1=a[3], y1=df[3,5], col=c("olivedrab"),   angle=90, length=0.02, code=3)
      # Interquantile of simulations for DH
      if(nrow(df)==4)  arrows(x0=a[4], y0=df[4,4], x1=a[4], y1=df[4,5], col=c("olivedrab"),   angle=90, length=0.02, code=3)
    } else
    {
      a <- barplot(df[,3], xaxt='n', yaxt='n', col=c("white","darkorange"), border=c("darkorange4","darkorange4"), ylim=c(0,16), lwd= 2)
      # Interquantile of measurments for Bacanora
      if(c(df[1,4]-df[1,5]) != 0)  arrows(x0=a[1], y0=df[1,4], x1=a[1], y1=df[1,5], col=c("darkorange4"), angle=90, length=0.02, code=3)
      # Interquantile of simulations for DH
      if(c(df[2,4]-df[2,5]) != 0)  arrows(x0=a[2], y0=df[2,4], x1=a[2], y1=df[2,5], col=c("darkorange4"),   angle=90, length=0.02, code=3)
    }
    par(lwd=1)
    # Add axis
    axis(2, cex.axis=0.8, las=1 , lin=-0.9, lty=0)
    mtext("Grain yield\n(t/ha)", side=2, adj=0.5, cex = 0.6  , las=0, line =0.7)
  }

  # NZ  
  par(fig=c(0.74, 0.80, 0.12, 0.21), new=TRUE, mar=c(0,0,0,0), tck=0.02)
  do.barplot.yield(mme$nz)
  # Rots
  par(fig=c(0.34, 0.40, 0.70, 0.79), new=TRUE, mar=c(0,0,0,0), tck=0.02)
  do.barplot.yield(mme$fr)
  # Valdivia, Chile
  par(fig=c(0.15, 0.27, 0.25, 0.34), new=TRUE, mar=c(0,0,0,0), tck=0.02)
  do.barplot.yield(mme$va)
  # Balcarce, Argentina
  par(fig=c(0.34, 0.46, 0.12, 0.21), new=TRUE, mar=c(0,0,0,0), tck=0.02)
  do.barplot.yield(mme$ba)
  # Obregon, Mexico
  par(fig=c(0.27, 0.38, 0.61, 0.70), new=TRUE, mar=c(0,0,0,0), tck=0.02)
  do.barplot.yield(mme$co)
  
  # Add legend    c(0.12, 0.35, 0.46, 0.53)
  par( fig=c(0.12, 0.7, 0.46, 0.8), new=TRUE, mar=c(0,0,0,0), tck=0.02)
  plot.new()
  legend("bottomleft", 
         legend=c("Measured standard elite cultivar",
                  "Simulated standard elite cultivar",
                  "Measured high-yield ideotype",
                  "Simulated high-yield ideotype"),
         col=rep(c("darkorange4","olivedrab"),each=2), 
         pt.bg = c("white","darkorange","white","olivedrab1"), pch=22, cex=1, pt.cex=2, bty="n")
}
dev.off()
# rm(i, v, s, size, step.cv, plotmat, map, my.col.b, my.col.f, l.wd, wd, y.lim, global.whted.yield, site)
