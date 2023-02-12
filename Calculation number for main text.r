res.opt.30y <- readRDS(paste0(DataAnalysis.wd, "CropNtoMaximizeYield_30yrsAve.RDS"))

current.dose <- as.data.frame(res.opt.30y[["current.dose"]])
current.dose <- current.dose[-which(current.dose$model %in% c("e.median","q.25","q.75")),]
opt.gpc <- as.data.frame(res.opt.30y[["opt.gpc"]])
opt.gpc  <- opt.gpc[-which(opt.gpc$model %in% c("e.median","q.25","q.75")),]

site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

scale.to.globe <- function(data){   # data = current.dose 
  # Create an empty data frame with model and GCM columns
  global       <- data.frame(model.gcm = paste0(rep(unique(data$model),each=length(unique(data$gcm))), ".", rep(unique(data$gcm),length(unique(data$model)))), stringsAsFactors = F)
  global$model <- substr(global$model.gcm, 1,2)
  global$gcm   <- substr(global$model.gcm, 4,4)
  
  # Scale to yield and NUE to the  globe based on production
  for(v in c("Yield", "CroN.ma", "NUE", "GPC", "dose"))   # v = "Yield(00N)"
  {
    # Scale % change in yield and NUE to the globe based on production
    data$area.weight <- site$area.weight[match(data$site, site$Site)]
    wht.v <- paste0("wht.", v)
    data[, wht.v] <- data$area.weight * data[, v]
    # Yield and NUE to the globe based on production area (for each model and gcm)
    global.v <- paste0("global.", v)
    global[, global.v]  <- round(as.data.frame(data.table(data)[, lapply(.SD,  sum), by=list(model, gcm), .SDcols=wht.v])[,3] , 2)
  }
  return(global)
}

OON <- current.dose[which(current.dose$trait=="N" & current.dose$gcm=="_"),]
global.current.dose.00N <- scale.to.globe(OON)
OOT <- current.dose[which(current.dose$trait=="T" & current.dose$gcm=="_"),]
global.current.dose.00T <- scale.to.globe(OOT)

N45 <- current.dose[which(current.dose$trait=="N" & current.dose$rcp=="45"),]
global.current.dose.45N <- scale.to.globe(N45)
T45 <- current.dose[which(current.dose$trait=="T" & current.dose$rcp=="45"),]
global.current.dose.45T <- scale.to.globe(T45)

N85 <- current.dose[which(current.dose$trait=="N" & current.dose$rcp=="85"),]
global.current.dose.85N <- scale.to.globe(N85)
T85 <- current.dose[which(current.dose$trait=="T" & current.dose$rcp=="85"),]
global.current.dose.85T <- scale.to.globe(T85)



opt.00N <- opt.gpc[which(opt.gpc$trait=="N" & opt.gpc$rcp=="00"),]
global.opt.gpc.00N <- scale.to.globe(opt.00N)
opt.00T <- opt.gpc[which(opt.gpc$trait=="T" & opt.gpc$rcp=="00"),]
global.opt.gpc.00T <- scale.to.globe(opt.00T)

opt.45N <- opt.gpc[which(opt.gpc$trait=="N" & opt.gpc$rcp=="45"),]
global.opt.gpc.45N <- scale.to.globe(opt.45N)
opt.45T <- opt.gpc[which(opt.gpc$trait=="T" & opt.gpc$rcp=="45"),]
global.opt.gpc.45T <- scale.to.globe(opt.45T)

opt.85N <- opt.gpc[which(opt.gpc$trait=="N" & opt.gpc$rcp=="85"),]
global.opt.gpc.85N <- scale.to.globe(opt.85N)
opt.85T <- opt.gpc[which(opt.gpc$trait=="T" & opt.gpc$rcp=="85"),]
global.opt.gpc.85T <- scale.to.globe(opt.85N)


mean(global.opt.gpc.00T$global.dose ,na.rm=T)
mean(global.current.dose.00N$global.dose , na.rm=T)

mean( 100 * ( global.opt.gpc.00T$global.dose - global.current.dose.00N$global.dose) / global.current.dose.00N$global.dose, na.rm=T )

mean( 100 * ( global.opt.gpc.45T$global.dose - global.opt.gpc.85N$global.dose) / global.opt.gpc.85N$global.dose, na.rm=T )


# Difference of GPC due to trait at optimum N with GPC > 12%
mean(global.current.dose.00T$global.GPC,na.rm=T) - mean(global.current.dose.00N$global.GPC,na.rm=T)
mean(global.current.dose.45T$global.GPC,na.rm=T) - mean(global.current.dose.45N$global.GPC,na.rm=T)
mean(global.current.dose.85T$global.GPC,na.rm=T) - mean(global.current.dose.85N$global.GPC,na.rm=T)

# % Change in N fertilizer use due to trait at optimmum GPC > 12%
100 * ( mean(global.opt.gpc.00T$global.CroN.ma,na.rm=T) - mean(global.opt.gpc.00N$global.CroN.ma,na.rm=T) ) / mean(global.opt.gpc.00N$global.CroN.ma,na.rm=T)
