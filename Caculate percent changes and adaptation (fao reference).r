#--------------------------------------
# Rearrange the data frame with Yield
# for the RCPs and traits in columns
#--------------------------------------

dt <- OptiN[which(OptiN$trait=="N" & OptiN$rcp=="45"), c("site", "model", "gcm")]
dt$site.model.gcm <- paste(dt$site, dt$model, dt$gcm, sep=".")
dt$site.model <- paste(dt$site, dt$model, sep=".")

# var2 <- c(var, paste0(var[6:length(var)], ".N"))
var2 <- c(var, paste0(var[11:length(var)], ".N"))

# Add baseline data
#------------------
for(v in var2)
{
  for(t in c("N","T"))
  {
    baseline <- OptiN[which(OptiN$rcp=="00" & OptiN$gcm=="_" & OptiN$trait==t), c("site", "model", v)]
    baseline$site.model <- paste(baseline$site, baseline$model, sep=".")
    dt[, paste0(v,"(00",t,")")] <- baseline[,v][match(dt$site.model, baseline$site.model)]
  }
}
rm(v, t, baseline)

# Add future scenarios data
#--------------------------
for(r in c("45", "85"))
{
  for(v in var2)
  {
    for(t in c("N","T"))
    {
      rcp <- OptiN[which(OptiN$rcp==r & OptiN$trait==t), c("site", "model", "gcm", "trait", v)]
      rcp$site.model.gcm <- paste(rcp$site, rcp$model , rcp$gcm, sep=".")
      dt[, paste0(v,"(",r,t,")")] <- rcp[,v][match(dt$site.model.gcm, rcp$site.model.gcm)]
    }
  }
}
rm(r, v, t, rcp, var2)

#--------------------------------
# Calculate % change in yield in 
# response to traits under
# baseline and CC and adaptation 
# to CC with improved traits or
# N
#--------------------------------
i.v <- 0
for(v in var[11:length(var)])  # v = "yield.opt"
{ 
  i.v <- i.v + 1 
  for(r in c("00","45","85"))  # r = "45"
  {
    # Calculate % Change in response to trait
    dt[,paste0("pc.", v, "(",r,")")]   <- 100 * ( dt[, paste0(v,  "(",r,"T)")] - dt[,paste0(var[i.v],"(00N)")] )   / dt[, paste0(var[i.v],"(00N)")]
    dt[,paste0("pc.", v, ".N(",r,")")] <- 100 * ( dt[, paste0(v,".N(",r,"T)")] - dt[,paste0(var[i.v],"(00N)")] )   / dt[, paste0(var[i.v],"(00N)")]
    if(grepl("gpc.",v))
    {
      dt[,paste0("pc.", v, "(",r,")")]   <- dt[, paste0(v,"(",r,"T)")]     - dt[,paste0(v,"(00N)")]
      dt[,paste0("pc.", v, ".N(",r,")")] <- dt[, paste0(v,".N(",r,"T)")]   - dt[,paste0(v,"(00N)")]
    }
    # Calculate adaptation to climate change due to trait or N
    if(r != "00")
    {
      dt[,paste0("adapt.", v, "(",r,")")]     <-   ( dt[, paste0(v,  "(",r,"T)")] - dt[, paste0(v,"(",r,"N)")] ) - (  dt[,paste0(v,  "(00T)")] - dt[,paste0(v,"(00N)")] )
      dt[,paste0("adapt.", v, ".N(",r,")")]   <-   ( dt[, paste0(v,".N(",r,"T)")] - dt[, paste0(v,"(",r,"N)")] ) - (  dt[,paste0(v,".N(00T)")] - dt[,paste0(v,"(00N)")] )
      dt[,paste0("r.adapt.", v, "(",r,")")]   <- ( ( dt[, paste0(v,  "(",r,"T)")] - dt[, paste0(v,"(",r,"N)")] ) - (  dt[,paste0(v,  "(00T)")] - dt[,paste0(v,"(00N)")] ) ) / dt[,paste0(v,"(00N)")]
      dt[,paste0("r.adapt.", v, ".N(",r,")")] <- ( ( dt[, paste0(v,".N(",r,"T)")] - dt[, paste0(v,"(",r,"N)")] ) - (  dt[,paste0(v,".N(00T)")] - dt[,paste0(v,"(00N)")] ) ) / dt[,paste0(v,"(00N)")]
    }
  }
}
rm(v, r)

#-------------
# save results
#-------------

saveRDS(dt, paste0(DataAnalysis.wd, "30y.mean_PercentageChange_Adaptation.RDS"))
  