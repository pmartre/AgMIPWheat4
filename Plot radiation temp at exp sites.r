library(data.table)

setwd("C:/Users/martrepi/OneDrive/1-Pierre/3-Projets/En cours/AgMIP/AgMIP_4/agmip_phase4_globalsites/ExpDatasets/AgMERRA baseline weather")


#-------------------
# Define site names
# sowing, anthesis, 
# and maturity date
# and DOY
#-------------------

site            <-  c("Obregon, MX",        "Rots, FR",           "Valdivia, CL",         "Buenos Aires, AR",   "Leeston, NZ")
file            <- c("MXOB0QXX.AgMIP",      "FRRO0QXX.AgMIP",     "CLVA0QXX.AgMIP",       "ARBA0QXX.AgMIP",     "NZLE0QXX.AgMIP")
col             <- c("#E69F00",             "#56B4E9",            "#009E73",              "#0072B2",            "#D55E00")
sowing.date     <- c(as.Date("2022-12-01"), as.Date("2022-10-25"), as.Date("2022-08-25"), as.Date("2022-07-05"), as.Date("2012-03-25"))
anthesis.date   <- c(as.Date("2023-02-27"), as.Date("2023-05-30"), as.Date("2022-11-30"), as.Date("2022-10-15"), as.Date("2012-11-26"))
maturity.date   <- c(as.Date("2023-04-08"), as.Date("2023-07-11"), as.Date("2023-01-15"), as.Date("2022-11-18"), as.Date("2013-01-10"))
sowing.doy      <- as.numeric(strftime(sowing.date, format="%j"))
anthesis.doy    <- as.numeric(strftime(anthesis.date, format="%j"))
maturity.doy    <- as.numeric(strftime(maturity.date, format="%j"))
sowing2anthesis <- as.numeric(anthesis.date - sowing.date) 

#-------------------
# Read and format
# daily weather data
#-------------------

if(exists('wht')) rm(wht)
for(i in 1:5) # i = 2
{
  new        <- read.table(file[i],skip=5)
  names(new) <- c("DATE","YYYY","MM","DD","SRAD","TMAX","TMIN","RAIN","WIND","DEWP","VPRS","RHUM")
  # Add date and day of the year
  new$date <- as.Date(format(ISOdate(new$YYYY,new$MM,new$DD)))
  new$doy <- as.numeric(strftime(format(ISOdate(new$YYYY,new$MM,new[,4])), format="%j"))
  # Remove 29 feb. 
  new <- new[-c(which(new$MM==2 & new$DD==29)), ]
  # Calculate daily average temperature
  new$TMEAN <-( new$TMIN + new$TMAX ) / 2
    # Calculate 30 years average
  new <- as.data.frame(data.table(new)[, lapply(.SD,  median), by=list(doy), .SDcols=c("SRAD","TMAX","TMIN","RAIN","WIND","DEWP","VPRS","RHUM", "TMEAN")])
  # Keep only data from sowing to maturity
  if(i != 4)
  {
    # get sowing to last day of the year
    new1 <- new[which(new$doy %in% c(sowing.doy[i]:366)), ]
    # get first day of the year to maturity
    new2 <- new[which(new$doy %in% c(1:maturity.doy[i])), ]
    new <- rbind(new1,new2)
  } else
  {
    new <- new[which(new$doy %in% c(sowing.doy[i] : maturity.doy[i])), ]
  }
  # add days after sowing
  new$das <- c(1:nrow(new))
  # Calculate cumulative radiation and temperature since sowing
  if(length(which(new$TMEAN < 0))!=0) new[which(new$TMEAN < 0), "TMEAN"] <- 0 
  new$thermaltime <- cumsum(new$TMEAN)
  new$cumSRAD <- cumsum(new$SRAD)
  # add site name
  new$site <- site[i]
  ifelse(exists('wht') == F, wht <- new , wht <- rbind(wht, new))
  rm(new)
}

# Calculated photothermal quotient
wht$PTQ <- wht$SRAD / wht$TMEAN

#--------------
# Plot function
#--------------

wht.plot <- function(var, ymax){ # var = "TMEAN", ymax=1
  plot(wht$das, wht[,var], ylim = c(0,ymax), xlim = c(0,300), pch="", xlab="", ylab="", xaxt="n", yaxt="n")
  for(i in 1:5) # i= 5
  {
    dt <- wht[wht$site==site[i], ]
    fit <- smooth.spline(dt[, var]  ~ dt[, "das"], spar=0.75)
    # lines(dt$das, dt$SRAD, col=col[i])
    lines(fit$x, fit$y,col=col[i])
    arrows.y <- fit$y[which(dt$doy == anthesis.doy[i])]
    arrows.x <- sowing2anthesis[i]
    if(var %in% c("SRAD", "TMEAN"))            arrows(y0=arrows.y + 1, y1=arrows.y , x0=arrows.x, x1=arrows.x, length=0.05, col=col[i])
    if(var == "PTQ")                           arrows(y0=arrows.y + 0.07, y1=arrows.y , x0=arrows.x, x1=arrows.x, length=0.05, col=col[i])
    if(var %in% c("cumSRAD", "thermaltime"))   arrows(y0=arrows.y + 133, y1=arrows.y , x0=arrows.x, x1=arrows.x, length=0.05, col=col[i])
    rm(fit)
  }
  # Add Y axis
  axis(2, cex.axis=1, mgp=c(0,0.5,0), tck=0.02)
}

pdf(paste0(main.wd , "Data_Analysis\\Figures for paper\\Fig S1. radiation and temp exp locations.pdf"))
par(mar=c(0.4,6, 0.4, 1), oma=c(3,6,0,0), tck=0.02 , xaxs="i", yaxs="i",las=1)
layout(matrix(c(1:6), 3, 2, byrow=T) , heights = c(1,1,1,1), widths = c(1,1,1))

wht.plot("SRAD", ymax=30)
legend("topleft", legend=("(A)"), bty="n", cex=1)

wht.plot("cumSRAD", ymax=4000)
legend("topleft", legend=("(B)"), bty="n", cex=1)

wht.plot("TMEAN", ymax=25)
legend("topleft", legend=("(C)"), bty="n", cex=1)

wht.plot("thermaltime", ymax=4000)
legend("topleft", legend=("(D)"), bty="n", cex=1)

# Add X axis
axis(1 ,cex.axis=1, mgp=c(0,0.5,0), tck=0.02)

wht.plot("PTQ", ymax=2.2)
legend("topleft", legend=("(E)"), bty="n", cex=1)

# Add X axis
axis(1 ,cex.axis=1, mgp=c(0,0.5,0), tck=0.02)

plot.new()
legend("center", legend=site, col=col, lty=1, bty="n", cex=1, lwd=1.5)

# Add axis titles
mtext("Days after sowing", side=1,  cex=0.8, line=1.5, outer=T, adj=0.5)

mtext("Global solar radiation (MJ)",    side=2, cex=0.8, line=-4,  outer=T, adj=0.95, las=0)
mtext("Mean temperature (°C)",          side=2, cex=0.8, line=-4,  outer=T, adj=0.50, las=0)
mtext("Photothermal quotient (MJ/°C)", side=2, cex=0.8, line=-4,  outer=T, adj=0.02, las=0)

mtext("Cumulative global solar    \nradiation since sowing (MJ)",    side=2, cex=0.8, line=-26.5,  outer=T, adj=0.95, las=0)
mtext("Thermal time\nsince sowing (°C)",                          side=2, cex=0.8, line=-26.5,  outer=T, adj=0.50, las=0)


dev.off()
    