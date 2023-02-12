#---------
# Boxplot 
# function
#---------

do.boxplot <- function(pc, var, ylab){     # pc = "T" ; var=var.to.plot[v] ; ylab=y.lab.pc[v]
  y.limit <- range(c(df[df$TRNO==t, var], q.25[q.25$TRNO==t, var], q.75[q.75$TRNO==t, var]),  na.rm=T)
  # empty boxplot
  boxplot(df[df$TRNO==t,var] ~ df[df$TRNO==t,"site"], 
          boxwex=0.5,
          las=1,
          ylab=ylab,
          xlab="Sites",
          ylim=y.limit,
          outline = F,
          medlwd=1,
          horizontal=F,
          border="white")
  abline(h = 0)
  # Add data (30 years per site)
  points(df[df$TRNO==t,"site"],
         df[df$TRNO==t,var],
         pch=1,
         cex=cex.m,
         col="black")
  # Add boxplot of data
  boxplot(df[df$TRNO==t,var] ~ df[df$TRNO==t,"site"], 
       boxwex=0.5,
       las=1,
       ylab="",
       xlab="",
       outline = F,
       medlwd=1,
       horizontal=F,
       add=TRUE,
       border="red",
       ylim=y.limit)
  if(pc != "T")
  {
    if(m != "e.median")
    {
      if(var != "pc")
      {
        # Add e.median 30 years mean on the side of the boxplot
        points(e.median[e.median$TRNO==t, "site"] - 0.25,
               e.median[e.median$TRNO==t, var],
               pch=16,
               cex= 1,
               col="green")
        # Add a vertical bar of MME 25% - 75%-til eon the side of the boxplot
        arrows(x0 = q.25[q.25$TRNO==t, "site"] - 0.25,
               y0 = q.25[q.25$TRNO==t, var],
               x1 = q.75[q.75$TRNO==t, "site"] - 0.25,
               y1 = q.75[q.75$TRNO==t, var],
               length=0,
               col="green")
      }
    }
  }
  if("00" %in% unique(df[,"rcp"]) & pc != "T")
  {
    title(main=paste("Step 1 - Model ",   m,
                   " - Baseline", 
                   " - Trait = ", unique(df[df$TRNO==t, "trait"]),
                   "\nGreen circles are e.median ; vertical green lines are MME 25%-75% interquantile"))
  }
  if(!"00" %in% unique(df[,"rcp"]) & pc != "T")
  {
    title(main=paste("Step 1 - Model ",      m,
                     " - GCM = ",   unique(df[df$TRNO==t, "gcm"]), 
                     " - RCP = ",   unique(df[df$TRNO==t, "rcp"]), 
                     " - Trait = ", unique(df[df$TRNO==t, "trait"]),
                    "\n(Blue circle is e.median ; vertical blue line is MME 25%-75% interquantile)"))
  }
}

#-------------------
# Boxplots per model 
# and variables
#-------------------

for(m in unique(dt$model))  # m="SQ"
{
  if(m %in% c("q.25", "q.25"))  next
  for(v in 1:length(var.to.plot))  # v=1
  { 
    pdf(paste0(DataAnalysis.wd, "Boxplot_", s, " raw data - all treatments\\", m, "_", var.to.plot[v],".pdf"), paper = "a4", width=9, height=13)
    par(mfrow = c(4,1), mar=c(4,4,4,4))
    for(t in 1:22) # t="1"
    { 
      df       <- dt[dt$model==m,                  c("site", "TRNO", "trait", "rcp", "gcm", var.to.plot[v])]
      if(all(is.na(df[df$TRNO==t, var.to.plot[v]]))) next
      e.median <- dt.30y[dt.30y$model=="e.median", c("site", "TRNO", "trait",               var.to.plot[v])]
      q.25     <- dt.30y[dt.30y$model=="q.25"    , c("site", "TRNO", "trait",               var.to.plot[v])]
      q.75     <- dt.30y[dt.30y$model=="q.75"    , c("site", "TRNO", "trait",               var.to.plot[v])]
      do.boxplot(pc="F", var.to.plot[v], y.lab[v])
    }
    dev.off()
  }
}

rm(m, v, t, df, e.median, q.25, q.75)

#-------------------
# Boxplots per model 
# for baseline only
#------------------
for(m in unique(dt$model))  # m="SQ"
{
  if(m %in% c("q.25", "q.75"))  next
  pdf(paste0(DataAnalysis.wd, "Boxplot_", s, " raw data - baseline\\", m, "_Boxplot Baseline.pdf"), paper = "a4", width=9, height=13)
  par(mfrow = c(3,1), mar=c(4,4,4,4))
  for(v in 1:length(var.to.plot))  # v=1
  { 
    df       <- dt[which(dt$model==m            & dt$TRNO%in%c(1,2)),    c("site", "TRNO", "trait", "rcp", "gcm", var.to.plot[v])]
    e.median <- dt.30y[dt.30y$model=="e.median" & dt.30y$TRNO%in%c(1,2), c("site", "TRNO", "trait",               var.to.plot[v])]
    q.25     <- dt.30y[dt.30y$model=="q.25"     & dt.30y$TRNO%in%c(1,2), c("site", "TRNO", "trait",               var.to.plot[v])]
    q.75     <- dt.30y[dt.30y$model=="q.75"     & dt.30y$TRNO%in%c(1,2), c("site", "TRNO", "trait",               var.to.plot[v])]
    if(all(is.na(df[,var.to.plot[v]])))  { next ; plot.new(); plot.new(); plot.new() }
    for(t in 1:2) # t="1"
    { 
      do.boxplot(pc="F", var.to.plot[v], y.lab[v])
    }
    
    # Calculate and plot % change in response to trait
    if(var.to.plot[v] %in% c("GPC","sa","egf", "gpc.opt", "gpc.opt.gpc", "gpc.opt.N", "gpc.opt.gpc.N"))
    {
      df[df$TRNO==t, var.to.plot[v]]             <- df[df$TRNO==2,             var.to.plot[v]] - df[df$TRNO==1,             var.to.plot[v]]
      e.median[e.median$TRNO==t, var.to.plot[v]] <- e.median[e.median$TRNO==2, var.to.plot[v]] - e.median[e.median$TRNO==1, var.to.plot[v]]
      q.25[q.25$TRNO==t, var.to.plot[v]]         <- q.25[q.25$TRNO==2,         var.to.plot[v]] - q.25[q.25$TRNO==1,         var.to.plot[v]]
      q.75[q.75$TRNO==t, var.to.plot[v]]         <- q.75[q.75$TRNO==2,         var.to.plot[v]] - q.75[q.75$TRNO==1,         var.to.plot[v]]
    }else
    {
      df[df$TRNO==t,             var.to.plot[v]] <- 100 * ( df[df$TRNO==2,             var.to.plot[v]] - df[df$TRNO==1,             var.to.plot[v]] ) / df[df$TRNO==1,         var.to.plot[v]]
      e.median[e.median$TRNO==t, var.to.plot[v]] <- 100 * ( e.median[e.median$TRNO==2, var.to.plot[v]] - e.median[e.median$TRNO==1, var.to.plot[v]] ) / e.median[e.median$TRNO==1, var.to.plot[v]]
      q.25[q.25$TRNO==t,         var.to.plot[v]] <- 100 * ( q.25[q.25$TRNO==2,         var.to.plot[v]] - q.25[q.25$TRNO==1,         var.to.plot[v]] ) / q.25[q.25$TRNO==1,         var.to.plot[v]]
      q.75[q.75$TRNO==t,         var.to.plot[v]] <- 100 * ( q.75[q.75$TRNO==2,         var.to.plot[v]] - q.75[q.75$TRNO==1,         var.to.plot[v]] ) / q.75[q.75$TRNO==1,         var.to.plot[v]]
    }
    df[is.infinite(df[, var.to.plot[v]]), var.to.plot[v]] <- NA
    if(all(is.na(df[df$TRNO==t,var.to.plot[v]]))) { next ; plot.new() }
    do.boxplot(pc="T", var.to.plot[v], y.lab.pc[v])
  }
  dev.off()
}

rm(m, v, t, df, e.median, q.25, q.75)
