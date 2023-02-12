# Read site information
site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

# Get simulated yield interpolated at national fertilization rate from list  
current.dose <- res.opt.30y[["current.dose"]]
current.dose <- current.dose[which(current.dose$model%in% c("e.median","q.25","q.75") & current.dose$rcp=="00" & current.dose$trait=="N"), c("model","site","dose","Yield")]
current.dose$country.code <- site$Country.code[match(current.dose$site, site$Site)] 

# Get N fertilization rate (dose) at FAO national yield from list 
fao.yield <- res.opt.30y[["fao.yield"]]
fao.yield <- fao.yield[which(fao.yield$model%in% c("e.median","q.25","q.75") & fao.yield$rcp=="00" & fao.yield$trait=="N"), c("model","site","dose","Yield")]
fao.yield$country.code <- site$Country.code[match(fao.yield$site, site$Site)] 

# Calculate means per country
current.dose <- as.data.frame(data.table(current.dose)[, lapply(.SD,  mean), by=list(model, country.code), .SDcols=c("Yield","dose")])
current.dose <- current.dose[order(current.dose$country.code),]

fao.yield <- as.data.frame(data.table(fao.yield)[, lapply(.SD,  mean), by=list(model, country.code), .SDcols=c("Yield","dose")])
fao.yield <- fao.yield[order(fao.yield$country.code),]

site <- as.data.frame(data.table(site)[, lapply(.SD,  mean), by=list(Country.code), .SDcols=c("Country.yield","nat.n.fertilizer")])
site <- site[order(site$Country.code),]

#-----------------
# Function to plot
#-----------------

do.plot <- function(sim, x.var, y.var, gap, y.lab, x.lab, letter){
  obs <- site[,x.var] 
  mme <- sim[which(sim$model=="e.median"), y.var] 
  q25 <- sim[which(sim$model=="q.25"),     y.var] 
  q75 <- sim[which(sim$model=="q.75"),     y.var] 
  xy.max <- 1.1 * max(q75)
  plot(mme, obs, pch = "", xlim = c(0,xy.max), ylim = c(0,xy.max), xlab = "", ylab = "", 
       cex=2.5, cex.axis=1.5, cex.lab=1.5)
  abline(0,1, lty=2)
  abline(lm(obs ~ mme))
  arrows(y0 = obs, y1 = obs, x0 = q25 ,      x1 = mme - gap, col="grey", angle = 90, length = 0.03, code = 3, lwd=1)
  arrows(y0 = obs, y1 = obs, x0 = mme + gap, x1 = q75,       col="grey", angle = 90, length = 0.03, code = 3, lwd=1)
  text(mme, obs, labels = site$Country.code)
  
  mtext(x.lab, side=1,  cex=1.5, line=-11,    outer=T, adj=0.5)
  mtext(y.lab, side=2,  cex=1.5, line=-3.5,  outer=T, adj=0.5, las=0)
  
  RRMSE <- round(100 * sqrt(mean( (obs - mme)^2))/mean(obs), 1)
 
  MBE <- mean(c(obs-mme)) / mean (obs) 
  
  r2    <- cor(obs,mme)^2
  slope <- summary(lm(obs ~ mme))$coeff[2,1]
  
  SB <- (mean(mme) - mean(obs))^2
  NU <- (1 - slope)^2 * mean((mme - mean(mme))^2) 
  LC <- (1 - r2) * mean((obs - mean(obs))^2)
  
  # legend("topleft", legend = letter, bty="n")
  legend("topleft", legend = c(paste0("r² = ",    round(r2,    2)), 
                               paste0("RRMSE = ", round(RRMSE, 2) ," %"),
                               paste0("NU = ",    round(NU,    2) ," (t/ha)^2"),
                               paste0("LC = ",    round(LC,    2) ," (t/ha)^2"),
                               paste0("SB = ",    round(SB,    2) ," (t/ha)^2")),bty="n")
}
  
# Do the plot
#-------------

pdf(paste0(main.wd , "Data_Analysis\\Figures for paper\\", "Fig S1_Simulated vs FAO national yield.pdf"), paper="a4", width=9, height=13)
par(mar=c(6,6,4,4), oma=c(6,6,2,2), mgp=c(3,0.5,0), tck=0.02 , xaxs="i", yaxs="i", las=1, pty= "s", tck=0.02)
# layout(matrix(c(1,2), 2,1,byrow=F))

do.plot(sim = current.dose, x.var = "Country.yield", y.var = "Yield", gap = 0.45,
        x.lab = "Simulated national\naverage grain yield (t/ha)", 
        y.lab = "Reported national\naverage grain yield (t/ha)",
        letter = "(A)")

# do.plot(sim = fao.yield, x.var = "nat.n.fertilizer", y.var = "dose", gap = 15,
#         y.lab = "Average N fertilizer application rate estimated\nfrom simulated grain yield (kg N/ha)" , x.lab = "Historical national average\nN fertilizer application rate (kg N/ha)",
#         letter = "(B)")

dev.off()
