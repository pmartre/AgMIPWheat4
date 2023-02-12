# Read site information
Site <- read.table(paste0(main.wd,"Site_codes__coordinates.txt"),header=TRUE,stringsAsFactors=FALSE, sep="\t")

dt <- dt[order(dt$model, dt$site, dt$rcp, dt$gcm, dt$trait),]

dt$site.rcp.gcm.trait <- paste(dt$site, dt$rcp, dt$gcm, dt$trait, sep=".")
dt$model.site.rcp.gcm <- paste(dt$model, dt$site, dt$rcp, dt$gcm, sep=".")

#----------------------
# Create a data frame
# to store the results
#----------------------

n <- data.frame(model.site.rcp.gcm = unique(dt$model.site.rcp.gcm), stringsAsFactors = FALSE)

# Add model code
n$model <- substring(n$model.site.rcp.gcm, 1, 2)

# Add sites #
n[which(nchar(n$model.site.rcp.gcm)==9),  "site"] <- as.numeric(substring(n[which(nchar(n$model.site.rcp.gcm)==9),  "model.site.rcp.gcm"], 4, 4))
n[which(nchar(n$model.site.rcp.gcm)==10), "site"] <- as.numeric(substring(n[which(nchar(n$model.site.rcp.gcm)==10), "model.site.rcp.gcm"], 4, 5))

# Add RCP codes
n[which(nchar(n$model.site.rcp.gcm)==9),  "rcp"] <- substring(n[which(nchar(n$model.site.rcp.gcm)==9),  "model.site.rcp.gcm"], 6, 7)
n[which(nchar(n$model.site.rcp.gcm)==10), "rcp"] <- substring(n[which(nchar(n$model.site.rcp.gcm)==10), "model.site.rcp.gcm"], 7, 8)

# Add gcm codes
n[which(nchar(n$model.site.rcp.gcm)==9), "gcm"]  <- substring(n[which(nchar(n$model.site.rcp.gcm)==9),  "model.site.rcp.gcm"], 9, 9)
n[which(nchar(n$model.site.rcp.gcm)==10), "gcm"] <- substring(n[which(nchar(n$model.site.rcp.gcm)==10), "model.site.rcp.gcm"], 10, 10)

# Add a column for traits (N = standard cultivar, T = high yielding traits)
n$trait <- "N"
t       <- n
t$trait <- "T"
n       <- rbind(n,t)
rm(t)

# Add treatment #
n$site.rcp.gcm.trait <- paste(n$site, n$rcp, n$gcm, n$trait, sep=".")
n$TRNO <- dt$TRNO[match(n$site.rcp.gcm.trait, dt$site.rcp.gcm.trait)]
n <- n[,-which(names(n)=="site.rcp.gcm.trait")]

# Move columns
n <- n[moveme(names(n), "gcm after model.site.rcp.gcm")]
n <- n[moveme(names(n), "rcp after model.site.rcp.gcm")]
n <- n[moveme(names(n), "trait after model.site.rcp.gcm")]
n <- n[moveme(names(n), "site after model.site.rcp.gcm")]
n <- n[moveme(names(n), "model after model.site.rcp.gcm")]
n <- n[moveme(names(n), "TRNO after model")]

# Add columns for the respnse variables
for(v in 1:length(VAR))
{
  n$x1 <- NA
  names(n)[dim(n)[2]] <- VAR[v]
}

results <- n

#---------------------
# Functions to fit 
# responses to N dose
#---------------------

do.spline <- function(y){
  if(!any(is.na(y)))  return(predict(smooth.spline(y  ~ dose, cv=FALSE), x=seq(0,450))$y)
}

#------------------------------------
#  Function to calculate N fertilizer
# efficiency as the average of the
# rolling slope of yield vs. ferti
#------------------------------------

do.ferti.eff <- function(x,y) { 
  df <- data.frame(list(x=x, y=y))
  Coef <- function(Z) coef(lm(1000*y ~ x, as.data.frame(Z)))[2]    
  return(round(mean(rollapplyr(zoo(df), 20 , Coef, by.column = FALSE)),2))
}

#------------------------------
# Function to get responses 
# interpolated at target N dose
#------------------------------

get.interpolation <- function(target.yield, calculate.target.dose, target.dose){
  
  res <- results.i
  # 2022-05-24: target.yield multiplied by 0.95 to estimate N dose at the beginning of the asymptotic value of yield vs. N 
  #             dose when the target yield is close or maximum value of yield 
  if(calculate.target.dose == "T")
  {
    target.dose <- x.fits[which.min(abs(y.fits$Yield - 0.95 * target.yield))]
  }
  res[which(res$model.site.rcp.gcm==i & res$trait==t), "dose"]  <- round(target.dose,  0)
  for(v in unique(VAR))
  {
    if(!any(is.na(var[v]))) res[which(res$model.site.rcp.gcm==i & res$trait==t), v] <- round(y.fits[,v][which(x.fits == target.dose)], 4)
  }
  res[which(res$model.site.rcp.gcm==i & res$trait==t), "Yield"] <- round(target.yield, 4)
  # Calculate N fertilizer efficiency
  ifelse(target.dose == 0, n <- 0, n <- max(which(x.fits < target.dose)))
  res[which(res$model.site.rcp.gcm==i & res$trait==t), "ferti.eff"] <- round(do.ferti.eff(x.fits[1:n], y.fits$Yield[1:n]), 2)
  
  # Replace "Inf" values by NA
  res[sapply(res, is.infinite)] <- NA
  
  return(res)
}

#------------------------
# Calculate optimal N and
# inefficiencies for each
# year / site combination
#------------------------

# Create a data frame to store the predicted values
y.fits <- data.frame(Yield =rep(NA,451))
for(v in 2:length(VAR)) # v=2
{
  y.fits$x1 <- NA
  names(y.fits)[v] <- VAR[v]
}
x.fits <- seq(0,450)

gc.index <- 0
index <- 0
for(i in unique(results$model.site.rcp.gcm))  #  i = "AW.21.00._"   
{ 
  for(t in c("N","T"))    # t = "N"
  { 
    if(t=="T") 
    {
      index    <- index    + 1
      gc.index <- gc.index + 1
      progress <- 2 * round(100 * index / length(results$model.site.rcp.gcm), 2)
      print(progress)
    }
    model <- substring(i, 1, 2)
    if(nchar(i)==9)   { site <- substring(i, 4, 4) ; rcp <- substring(i, 6, 7) }
    if(nchar(i)==10)  { site <- substring(i, 4, 5) ; rcp <- substring(i, 7, 8) }
    
    results.i <- results[which(results$model.site.rcp.gcm==i & results$trait==t),]
    
    # Values of selected variables for 450 kg N/ha fertilizer and unlimited N (Step 1)
    #---------------------------------------------------------------------------------
    N450.i <- results.i ; UL.i <- results.i 
    for(v in 1:length(VAR))
    {
      N450.i[which(N450.i$model.site.rcp.gcm==i & N450.i$trait==t), VAR[v]] <- round(dt[which(dt$model.site.rcp.gcm==i & dt$trait==t & dt$ferti==450), VAR[v]], 2)
      UL.i[which(UL.i$model.site.rcp.gcm==i & UL.i$trait==t),       VAR[v]] <- round(dt[which(dt$model.site.rcp.gcm==i & dt$trait==t & dt$step==1),  VAR[v]],   2)
    }
    
    # Get the simulated values for the selected variables
    var  <- dt[dt$model.site.rcp.gcm  == i & dt$trait == t & dt$step==2, VAR]
    
    # Reduce target GPC if all values are lower then the defined minimum target GPC
    if(!all(is.na(var$GPC)))
    {
      if(max(var$GPC) < target.gpc) { adj.target.gpc <- max(var$GPC) } else { adj.target.gpc <- target.gpc }
    }
    
    #---------------------
    # Fit spline functions to 
    # simulated responses vs
    # N dose
    #---------------------
    dose <- dt[dt$model.site.rcp.gcm  == i & dt$trait == t & dt$step==2, "ferti"]
    for(v in unique(VAR)) # i = "Yield"
    {
      y.fits[v] <- do.spline(y=var[,v])
    }
    
    # @ FAO yield aggregated by location 
    #-----------------------------------
    if(between(Site[Site$Site==site, "Country.yield"], min(var$Yield), max(var$Yield)))
    {
      target.yield <- Site[Site$Site==site, "Country.yield"]  
    } else
    {
      if(min(var$Yield) > Site[Site$Site==site, "Country.yield"]) target.yield <- min(var$Yield)
      if(max(var$Yield) < Site[Site$Site==site, "Country.yield"]) target.yield <- max(var$Yield)
    }
    
    fao.yield.i <- get.interpolation(target.yield = target.yield, calculate.target.dose = "T", target.dose = NA)
    
    # @ N dose for FAO yield for std cultivar for the baseline period
    #----------------------------------------------------------------
    
    if(rcp == "00" & t == "N")
    {
      dose.fao.N <- fao.yield.i[which(fao.yield.i$model== model & fao.yield.i$site == site & fao.yield.i$rcp == "00" & fao.yield.i$trait == "N"), "dose"]
    }
     
    fao.dose.i <- get.interpolation(target.yield = y.fits$Yield[which(x.fits == dose.fao.N)], calculate.target.dose = "F", target.dose = dose.fao.N)
    
    
    # @ current national N dose 
    #--------------------------
    
    national.dose <- Site[Site$Site==site, "nat.n.fertilizer"]  
    
    current.dose.i <- get.interpolation(target.yield = y.fits$Yield[which(x.fits == national.dose)], calculate.target.dose = "F", target.dose = national.dose)
    
    # @ N dose that maximize yield
    #-----------------------------
    
    opt.i <- get.interpolation(target.yield = round(f.opti * min(max(var$Yield), y.fits$Yield[which.max(y.fits$Yield)]), 2),
                               calculate.target.dose = "T", target.dose = NA)
    
    # @ N dose that maximize with a minimum target GPC (target.gpc)
    #-----------------------------------------------------------
    
    if(!all(is.na(var$GPC)))
    {
      # GPC at optimum yield
      gpc.at.opt.yield <- opt.i[opt.i$model.site.rcp.gcm==i & opt.i$trait==t, "GPC"]
   
      if(gpc.at.opt.yield >= adj.target.gpc)
      {
        dose.opt.gpc <- opt.i[opt.i$model.site.rcp.gcm==i & opt.i$trait==t, "dose"]
      } else
      {
        if(max(y.fits$GPC) > adj.target.gpc)
        {
          dose.opt.gpc <- x.fits[which.max(y.fits$GPC > adj.target.gpc)]
        } else
        {
          dose.opt.gpc <- 450
        }
      }
      opt.gpc.i <- get.interpolation(target.yield = y.fits$Yield[which(x.fits==dose.opt.gpc)],
                                     calculate.target.dose = "F", target.dose = dose.opt.gpc)
    } else
    {
      tmp <- opt.i
      tmp[8:ncol(opt.i)] <- NA
      opt.gpc.i <- data.frame(opt.i[1:7], tmp[8:ncol(opt.i)])
      rm(tmp)
    }
    
    # @ N dose that maximize yield for std cultivar for the baseline period
    #--------------------------------------------------------------------
    
    if(rcp == "00" & t == "N")
    {
      dose.opt.N     <- opt.i[which(opt.i$model         == model & opt.i$site     == site & opt.i$rcp     == "00" & opt.i$trait     == "N"), "dose"]
      dose.opt.gpc.N <- opt.gpc.i[which(opt.gpc.i$model == model & opt.gpc.i$site == site & opt.gpc.i$rcp == "00" & opt.gpc.i$trait == "N"), "dose"]
    } else
    {
      dose.opt.N     <- opt[which(opt$model         == model & opt$site     == site & opt$rcp     == "00" & opt$trait     == "N"), "dose"]
      dose.opt.gpc.N <- opt.gpc[which(opt.gpc$model == model & opt.gpc$site == site & opt.gpc$rcp == "00" & opt.gpc$trait == "N"), "dose"]
    }
    
    opt.N.i <- get.interpolation(target.yield = y.fits$Yield[which(x.fits==dose.opt.N)],
                                 calculate.target.dose = "F", target.dose = dose.opt.N)
 
    # @ N dose that maximize yield with a minimum GPC (target GPC) for std cultivar for the baseline period
    #-------------------------------------------------------------------------------------------------
    
    if(!all(is.na(var$GPC)))
    {
      opt.gpc.N.i <- get.interpolation(target.yield = y.fits$Yield[which(x.fits == dose.opt.gpc.N)],
                                       calculate.target.dose = "F", target.dose = dose.opt.gpc.N)
    } else
    {
      tmp <- opt.i
      tmp[8:ncol(opt.i)] <- NA
      opt.gpc.N.i <- data.frame(opt.i[1:7], tmp[8:ncol(opt.i)])
      rm(tmp)
    }
   
     # Merge data
    if(exists('N450'))
    {
      N450 <- rbind(N450, N450.i) ; UL      <- rbind(UL,      UL.i)      ; fao.dose <- rbind(fao.dose, fao.dose.i) ; fao.yield <- rbind(fao.yield, fao.yield.i)
      opt  <- rbind(opt,  opt.i)  ; opt.gpc <- rbind(opt.gpc, opt.gpc.i) ; opt.N    <- rbind(opt.N,    opt.N.i)    ; opt.gpc.N <- rbind(opt.gpc.N, opt.gpc.N.i)
      current.dose <- rbind(current.dose, current.dose.i) 
    } else
    {
      N450         <- N450.i         ; UL      <- UL.i      ; fao.dose <- fao.dose.i ; fao.yield <- fao.yield.i
      opt          <- opt.i          ; opt.gpc <- opt.gpc.i ; opt.N    <- opt.N.i    ; opt.gpc.N <- opt.gpc.N.i
      current.dose <- current.dose.i
    }
    if(gc.index == 100)  { gc() ; gc.index <- 1 }
  }
}

remove.infinite <- function(data){
  for(i in 8:ncol(data)) # i = 8
  {
    if(length(which(is.infinite(data[,i]))) != 0)  data[which(is.infinite(data[,i])),i] <- NA
  }
  return(data)
}
N450         <- remove.infinite(N450)
UL           <- remove.infinite(UL)
fao.dose     <- remove.infinite(fao.dose)
fao.yield    <- remove.infinite(fao.yield)
current.dose <- remove.infinite(current.dose) 
opt          <- remove.infinite(opt)
opt.gpc      <- remove.infinite(opt.gpc)
opt.N        <- remove.infinite(opt.N)
opt.gpc.N    <- remove.infinite(opt.gpc.N)
#----------------------
# Calculate multimodel
# ensemble median and
# 25% and 75% quantiles
#----------------------

cal.mme <- function(data, stat, stat2let, myfunc) {
    for(i in 8:ncol(data))    # i = 8
    {
      if(i == 8) results                  <-  as.data.frame(data.table(data)[, lapply(.SD,  myfunc), by=list(TRNO, site, trait, rcp, gcm), .SDcols=names(data)[i]])
      if(i != 8) results[,names(data)[i]] <-  as.data.frame(data.table(data)[, lapply(.SD,  myfunc), by=list(TRNO, site, trait, rcp, gcm), .SDcols=names(data)[i]])[,6]
    } 
  results$model.site.rcp.gcm <- paste(rep(stat2let, nrow(results)), results$site, results$rcp, results$gcm, sep=".")
  results <- data.frame("model" = rep(stat, nrow(results)), results)
  results <- results[,names(data)]
  return(results)
}

call.cal.mme <- function(data){
  e.median <- cal.mme(data, stat = "e.median", stat2let <- "em", myfunc = function(x){ median(x,na.rm=T)})
  q.25     <- cal.mme(data, stat = "q.25",     stat2let <- "q2", myfunc = function(x){quantile(x,probs=0.25,na.rm=T)})
  q.75     <- cal.mme(data, stat = "q.75",     stat2let <- "q7", myfunc = function(x){quantile(x,probs=0.75,na.rm=T)})
  data <- rbind(data[,c("model.site.rcp.gcm", "model", "TRNO", "site", "trait", "rcp", "gcm", names(data[8:ncol(data)]))], e.median, q.25, q.75)
  gc()
  return(data)
}

N450         <- call.cal.mme(N450)
UL           <- call.cal.mme(UL)
fao.dose     <- call.cal.mme(fao.dose)
fao.yield    <- call.cal.mme(fao.yield)
current.dose <- call.cal.mme(current.dose)
opt          <- call.cal.mme(opt)
opt.gpc      <- call.cal.mme(opt.gpc)
opt.N        <- call.cal.mme(opt.N)
opt.gpc.N    <- call.cal.mme(opt.gpc.N)

#-----------------
# save the results
# as a list
#-----------------

# Create a list
results <- list(N450=N450, UL=UL, fao.dose=fao.dose, fao.yield=fao.yield,
                current.dose=current.dose, opt=opt, opt.gpc=opt.gpc,
                opt.N=opt.N, opt.gpc.N=opt.gpc.N)

if(ave.30 == "T")    saveRDS(results, paste0(DataAnalysis.wd, "CropNtoMaximizeYield_30yrsAve.RDS"))
if(ave.30 == "F")    saveRDS(results, paste0(DataAnalysis.wd, "CropNtoMaximizeYield.RDS"))

rm(results, site,   var,       dose,       gc.index, 
   index,   model,  rcp,     progress,  t,           v,          x.fits,   y.fits, 
   dose.opt.gpc,    dose.opt.gpc.N,     i,
   N450,    N450.i, UL,      UL.i,      fao.dose,    fao.dose.i, fao.yield, fao.yield.i, 
   opt,     opt.i,  opt.gpc, opt.gpc.i, opt.N,       opt.N.i,    opt.gpc.N, opt.gpc.N.i)
