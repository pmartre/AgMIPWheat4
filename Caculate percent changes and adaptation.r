# create a data frame for each mode of calculation of optimum N
N450         <- as.data.frame(res.opt.30y[["N450"]])
UL           <- as.data.frame(res.opt.30y[["UL"]])
fao.dose     <- as.data.frame(res.opt.30y[["fao.dose"]])
fao.yield    <- as.data.frame(res.opt.30y[["fao.yield"]])
current.dose <- as.data.frame(res.opt.30y[["current.dose"]])
opt          <- as.data.frame(res.opt.30y[["opt"]])
opt.N        <- as.data.frame(res.opt.30y[["opt.N"]])
opt.gpc      <- as.data.frame(res.opt.30y[["opt.gpc"]])
opt.gpc.N    <- as.data.frame(res.opt.30y[["opt.gpc.N"]])


do.caculate.pc.adaptation <- function(data){ # data = current.dose

  #--------------------------------------
  # Rearrange the data frame with Yield
  # for the RCPs and traits in columns
  #--------------------------------------
  
  dt <- data[which(data$trait=="N" & data$rcp=="45"), c("site", "model", "gcm")]
  dt$site.model.gcm <- paste(dt$site, dt$model, dt$gcm, sep=".")
  dt$site.model <- paste(dt$site, dt$model, sep=".")
  
  # Add baseline data
  #------------------
  for(v in names(data)[8:ncol(data)]) # v = "Yield"
  {
    for(t in c("N","T"))
    {
      baseline <- data[which(data$rcp=="00" & data$gcm=="_" & data$trait==t), c("site", "model", v)]
      baseline$site.model <- paste(baseline$site, baseline$model, sep=".")
      dt[, paste0(v,"(00",t,")")] <- baseline[,v][match(dt$site.model, baseline$site.model)]
    }
  }
  rm(v, t, baseline)
  
  # Add future scenarios data
  #--------------------------
  for(r in c("45", "85"))
  {
    for(v in names(data)[8:ncol(data)]) # v = "Yield"
    {
      for(t in c("N","T"))
      {
        rcp <- data[which(data$rcp==r & data$trait==t), c("site", "model", "gcm", "trait", v)]
        rcp$site.model.gcm <- paste(rcp$site, rcp$model , rcp$gcm, sep=".")
        dt[, paste0(v,"(",r,t,")")] <- rcp[,v][match(dt$site.model.gcm, rcp$site.model.gcm)]
      }
    }
  }
  rm(r, v, t, rcp)
  
  #--------------------------------
  # Calculate % change in yield in 
  # response to traits under
  # baseline and CC and adaptation 
  # to CC with improved traits or N
  #--------------------------------
  
  for(v in names(data)[8:ncol(data)])  # v = "dose"
  { 
    for(r in c("00","45","85"))  # r = "00"
    {
      # Calculate % Change in response to trait for baseline and future scenarios
      dt[,paste0("pc.", v, "(",r,")")]   <- 100 * ( dt[, paste0(v,"(",r,"T)")]     - dt[,paste0(v,"(00N)")] )   / dt[, paste0(v,"(00N)")]
      if(length(which(is.infinite(dt[,paste0("pc.", v, "(",r,")")]))) != 0)  dt[which(is.infinite(dt[,paste0("pc.", v, "(",r,")")])), paste0("pc.", v, "(",r,")")] <- NA
      # Fine rows with response equal to 0 for N and T trait
      is.0 <- which(c(dt[, paste0(v,"(",r,"T)")] + dt[,paste0(v,"(00N)")])%in%0)
      dt[is.0, paste0("pc.", v, "(",r,")")] <- 0
      if(v=="GPC")
      {
        dt[,paste0("pc.", v, "(",r,")")]   <- dt[, paste0(v,"(",r,"T)")]     - dt[,paste0(v,"(00N)")]
        if(length(which(is.infinite(dt[,paste0("pc.", v, "(",r,")")]))) != 0)  dt[which(is.infinite(dt[,paste0("pc.", v, "(",r,")")])), paste0("pc.", v, "(",r,")")] <- NA
      }
      # Calculate adaptation to climate change due to trait or N
      if(r != "00")
      {
        # Absolute adaptation
        dt[,paste0("adapt.", v, "(",r,")")]     <-   ( dt[, paste0(v,"(",r,"T)")]     - dt[, paste0(v,"(",r,"N)")] ) - (  dt[,paste0(v,"(00T)")]      - dt[,paste0(v,"(00N)")] )
        if(length(which(is.infinite(dt[,paste0("adapt.", v, "(",r,")")]))) != 0)   dt[which(is.infinite(dt[,paste0("adapt.", v, "(",r,")")])), paste0("adapt.", v, "(",r,")")] <- NA
        
        # Relative absolute adaptation
        dt[,paste0("r.adapt.", v, "(",r,")")]   <- ( ( dt[, paste0(v,"(",r,"T)")]     - dt[, paste0(v,"(",r,"N)")] ) - (  dt[,paste0(v,"(00T)")]      - dt[,paste0(v,"(00N)")] ) ) / dt[,paste0(v,"(00N)")]
        if(length(which(is.infinite(dt[,paste0("r.adapt.", v, "(",r,")")]))) != 0)   dt[which(is.infinite(dt[,paste0("r.adapt.", v, "(",r,")")])),paste0("r.adapt.", v, "(",r,")")] <- NA
      }
    }
  }
  rm(v, r)
  return(dt)
}

#-------------
# save results
#-------------

N450.pc.adpat         <- do.caculate.pc.adaptation(N450) 
UL.pc.adpat           <- do.caculate.pc.adaptation(UL) 
fao.dose.pc.adpat     <- do.caculate.pc.adaptation(fao.dose) 
fao.yield.pc.adpat    <- do.caculate.pc.adaptation(fao.yield) 
current.dose.pc.adpat <- do.caculate.pc.adaptation(current.dose)
opt.pc.adpat          <- do.caculate.pc.adaptation(opt) 
opt.N.pc.adpat        <- do.caculate.pc.adaptation(opt.N) 
opt.gpc.pc.adpat      <- do.caculate.pc.adaptation(opt.gpc) 
opt.gpc.N.pc.adpat    <- do.caculate.pc.adaptation(opt.gpc.N) 

results <- list(N450=N450.pc.adpat, UL=UL.pc.adpat, fao.dose=fao.dose.pc.adpat, fao.yield=fao.yield.pc.adpat, 
                current.dose = current.dose.pc.adpat,  opt=opt.pc.adpat, opt.gpc=opt.gpc.pc.adpat, 
                opt.N=opt.N.pc.adpat, opt.gpc.N=opt.gpc.N.pc.adpat)

saveRDS(results, paste0(DataAnalysis.wd, "30y.mean_PercentageChange_Adaptation.RDS"))
  