#-------------------
# read merged
# simulation results
#-------------------

data <- readRDS(paste0(DataAnalysis.wd, "merged summary_", Step, ".RDS"))

#-------------------
# Convert columns to
# the correct type
#-------------------

num.var <- c("Yield", "FLN", "GNumber", "Biom.an", "Biom.ma", "MaxLAI", "WDrain","CumET", "SoilAvW", "Runoff", "Transp", "CroN.an", "CroN.ma", "Nleac","GrainN", "Nmin", "Nvol", "Nimmo", "SoilN", "Nden", "cumPARi")
data <- as.data.frame(data %>% mutate_at(num.var[which(num.var %in% var.rearrange)], as.numeric))
rm(num.var)

data <- as.data.frame(data %>% mutate_at(c("Planting.date", "Emergence", "Ant", "Mat"), as.Date))

#---------------------
# Caculate duration of
# phenological phases
#---------------------

if("se"   %in% var.rearrange) data <- data %>% mutate(se   = as.numeric(Emergence - Planting.date))
if("sa"   %in% var.rearrange) data <- data %>% mutate(sa   = as.numeric(Ant       - Planting.date))
if("ea"   %in% var.rearrange) data <- data %>% mutate(ea   = as.numeric(Ant       - Emergence))
if("segf" %in% var.rearrange) data <- data %>% mutate(segf = as.numeric(Mat       - Planting.date))
if("eegf" %in% var.rearrange) data <- data %>% mutate(eegf = as.numeric(Mat       - Emergence))
if("aegf" %in% var.rearrange) data <- data %>% mutate(aegf = as.numeric(Mat       - Ant))

# -------------------------
# Remove crop and grain N 
# data for the models that
# do not consider crop or
# grain N
# ------------------------

if(exists('rm.gpc')==T)
{
  # data[data$model%in%rm.gpc, "CroN.an"] <- NA
  # data[data$model%in%rm.gpc, "CroN.ma"] <- NA
  data[data$model%in%rm.gpc, "GrainN"]  <- NA
}


#-------------------
# calculate GDW, GNC
# GPC, nhi, HI
#-------------------

calculate.ratio <- function(num,denum,factor){
  res <- round(factor * ( data[,num] / data[,denum] ) , 2)
  if(length(which(data[,denum] %in% c(0,NA))) != 0)
  {
    res[which(data[,denum] %in% c(0,NA))] <- NA
  }
  if(length(which(data[,num] %in% c(0,NA))) != 0)
  {
    res[which(data[,num] %in% c(0,NA))] <- NA
  }
  res[is.infinite(res)] <- NA
  return(res)
}
if("GDW" %in% var.rearrange) data$GDW <- calculate.ratio(num="Yield"  , denum="GNumber" , factor=10^5)    
if("GNC" %in% var.rearrange) data$GNC <- calculate.ratio(num="GrainN" , denum="GNumber" , factor=100)    
if("GPC" %in% var.rearrange) data$GPC <- calculate.ratio(num="GrainN" , denum="Yield"   , factor=.57)
if("nhi" %in% var.rearrange) data$nhi <- calculate.ratio(num="GrainN" , denum="CroN.ma" , factor=100)
if("HI"  %in% var.rearrange) data$HI  <- calculate.ratio(num="Yield"  , denum="Biom.ma" , factor=100)

#--------------
# Calculate NUE
#--------------

if("NUE" %in% var.rearrange) 
{
  if(Step == "Step02") { data$Nav  <-        data$ferti   + data$Nmin + 38.8 } else { data$Nav  <- as.numeric(NA)} # initial soil N = 38.8 kg N/ha
  if(Step == "Step02") { data$NUE  <- 1000 * data$Yield   / data$Nav         } else { data$NUE  <- as.numeric(NA)}
  if(Step == "Step02") { data$NUpE <-        data$CroN.ma / data$Nav         } else { data$NUpE <- as.numeric(NA)}
  data$NUtE <- 1000 * data$Yield / data$CroN.ma
  for(i in c("Nav", "NUE","NUtE","NUpE"))
  { 
    if(length(which(is.infinite(data[,i]))) != 0) data[which(is.infinite(data[,i])), i] <- NA
  }
}

#--------------
# Calculate NNI
# and anthesis
#--------------
# Uses Juste et al. (1994, Ann. Bot. 74:397-407) dilution curve for wheat

if("NNI.an" %in% var.rearrange) 
{
  data$NNI.an <- NA
  data[which(data$Biom.an < 1.55),  "NNI.an"] <- round( (100  * data[which(data$Biom.an < 1.55),  "CroN.an"] / (10^3 * data[which(data$Biom.an < 1.55) , "Biom.an"])) / 4.4 , 2) 
  data[which(data$Biom.an >= 1.55), "NNI.an"] <- round( (100  * data[which(data$Biom.an >= 1.55), "CroN.an"] / (10^3 * data[which(data$Biom.an >= 1.55) , "Biom.an"])) / 
                                                        (5.35 * data[which(data$Biom.an >= 1.55) , "Biom.an"] ^-0.442) , 2)     
  if(length(which(data$Biom.an == 0 | data$CroN.an == 0)) != 0)   data[which(data$Biom.an == 0 | data$CroN.an == 0) , "NNI.an"] <- NA
  if(length(which(is.infinite(data$NNI))) != 0)                   data[which(is.infinite(data$NNI.an)), "NNI.an"] <- NA
}

#---------------------
# Calculate yield at
# 13% moisture content
#---------------------

data$Yield <- (1/(1-gmc) * data$Yield)

#-----------------
# Add a column 
# with year number
#-----------------

if(Step == "Step01")   data <- as.data.frame(group_by(data, model, site, rcp, gcm, trait)        %>% mutate(year.nb=1:n()))
if(Step == "Step02")   data <- as.data.frame(group_by(data, model, site, rcp, gcm, trait, ferti) %>% mutate(year.nb=1:n())) 

#----------------------
# Calculate multimodel
# ensemble median and
# 25% and 75% quantiles
#----------------------

if(Step == "Step01")
{
  cal.mme <- function(myfunc, stat) {
    for(i in 1:length(var.rearrange))    # i = 1
    {
      if(i == 1) results                    <-  as.data.frame(data.table(data)[, lapply(.SD,  myfunc), by=list(trait, site, gcm, rcp, year.nb), .SDcols=var.rearrange[i]])
      if(i != 1) results[,var.rearrange[i]] <-  as.data.frame(data.table(data)[, lapply(.SD,  myfunc), by=list(trait, site, gcm, rcp, year.nb), .SDcols=var.rearrange[i]])[,6]
    }
    results <- data.frame("model" = rep(stat, nrow(results)), results)
    return(results)
  }
  
  e.median <- cal.mme(stat = "e.median", myfunc = function(x){ median(x,na.rm=T)})
  gc()
  q.25     <- cal.mme(stat = "q.25",     myfunc = function(x){quantile(x,probs=0.25,na.rm=T)})
  gc()
  q.75     <- cal.mme(stat = "q.75",     myfunc = function(x){quantile(x,probs=0.75,na.rm=T)})
  gc()
  data <- rbind(data[,c("model", "trait", "site", "gcm", "rcp", "year.nb", var.rearrange)], e.median, q.25, q.75)
}

if(Step == "Step02")
{
  cal.mme <- function(myfunc, stat) {
    for(i in 1:length(var.rearrange))    # i = 1
    {
      if(i == 1) results                    <-  as.data.frame(data.table(data)[, lapply(.SD,  myfunc), by=list(trait, site, gcm, rcp, ferti, year.nb), .SDcols=var.rearrange[i]])
      if(i != 1) results[,var.rearrange[i]] <-  as.data.frame(data.table(data)[, lapply(.SD,  myfunc), by=list(trait, site, gcm, rcp, ferti, year.nb), .SDcols=var.rearrange[i]])[,7]
    }
    results <- data.frame("model" = rep(stat, nrow(results)), results)
    return(results)
  }

  e.median <- cal.mme(stat = "e.median", myfunc = function(x){ median(x,na.rm=T)})
  gc()
  q.25     <- cal.mme(stat = "q.25",     myfunc = function(x){quantile(x,probs=0.25,na.rm=T)})
  gc()
  q.75     <- cal.mme(stat = "q.75",     myfunc = function(x){quantile(x,probs=0.75,na.rm=T)})
  gc()
  data <- rbind(data[,c("model", "trait", "site", "gcm", "rcp", "ferti","year.nb", var.rearrange)], e.median, q.25, q.75)
}

rm(e.median, q.25, q.75)

# mme <- group_by(data, trait, site, gcm, rcp, year.nb) %>% summarise_at(var.rearrange, list(e.median = function(x){median(x,na.rm=T)},
#                                                                                  q.25     = function(x){quantile(x,probs=0.25,na.rm=T)},
#                                                                                  q.75     = function(x){quantile(x,probs=0.75,na.rm=T)}))

#------------------
# Calculate mean
# over the 30 years
#------------------

if(Step == "Step01")
{
  for(i in 1:length(var.rearrange))    # i = 1
  {
    if(i == 1) data.30y                    <-  as.data.frame(data.table(data)[, lapply(.SD,  function(x){ mean(x,na.rm=T)}), by=list(model, trait, site, gcm, rcp), .SDcols=var.rearrange[i]])
    if(i != 1) data.30y[,var.rearrange[i]] <-  as.data.frame(data.table(data)[, lapply(.SD,  function(x){ mean(x,na.rm=T)}), by=list(model, trait, site, gcm, rcp), .SDcols=var.rearrange[i]])[,6]
  }
}

if(Step == "Step02")
{
  for(i in 1:length(var.rearrange))    # i = 1
  {
    if(i == 1) data.30y                    <-  as.data.frame(data.table(data)[, lapply(.SD,  function(x){ mean(x,na.rm=T)}), by=list(model, trait, site, gcm, rcp, ferti), .SDcols=var.rearrange[i]])
    if(i != 1) data.30y[,var.rearrange[i]] <-  as.data.frame(data.table(data)[, lapply(.SD,  function(x){ mean(x,na.rm=T)}), by=list(model, trait, site, gcm, rcp, ferti), .SDcols=var.rearrange[i]])[,7]
  }
}

#----------------
# Add traitment #
#----------------

keys <- read.table(paste0(main.wd, "//AgMIPWheat4_GLOBAL_Step1_traitment_code.txt") , header = T  , stringsAsFactors = F)
keys$rcp.code <- as.character(keys$rcp.code)
keys[which(keys$rcp.code=="0"),"rcp.code"] <- "00"
keys$rcp.gcm.trait     <- paste(keys$rcp.code, keys$gcm.code, keys$trait.code, sep=".")
data$rcp.gcm.trait     <- paste(data$rcp,      data$gcm,      data$trait,      sep=".")
data.30y$rcp.gcm.trait <- paste(data.30y$rcp,  data.30y$gcm,  data.30y$trait,  sep=".")
data$TRNO <- keys$TRNO[match(data$rcp.gcm.trait, keys$rcp.gcm.trait)]
data.30y$TRNO <- keys$TRNO[match(data.30y$rcp.gcm.trait, keys$rcp.gcm.trait)]
data     <- data[moveme(names(data),         "TRNO after model")]
data.30y <- data.30y[moveme(names(data.30y), "TRNO after model")]
data     <- data[,    -which(names(data)     == "rcp.gcm.trait")]
data.30y <- data.30y[,-which(names(data.30y) == "rcp.gcm.trait")]
rm(keys)

#--------------------
# save rearanged data
#--------------------

setwd(DataAnalysis.wd)
saveRDS(data,     file=paste0("merged summary_", Step, "(formated).RDS"))
saveRDS(data.30y, file=paste0("30y.mean_", Step, ".RDS"))
rm(data, data.30y)
gc()
