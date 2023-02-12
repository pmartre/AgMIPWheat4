# Select models based on the step
s.model <- unique(data.30y[data.30y$step==step.mme.expdataset, "model"])
s.model <- s.model[!s.model%in%c("e.mean","e.median","q.25","q.75")]

#-------------------
# Function to create 
# data frame for 
# barplot
# ------------------

do.df.4.barplot <- function(o, o.q25, o.q75, s, s.q25, s.q75){ # o=nz.o ; o.q25=nz.o.q25 ; o.q75=nz.o.q75 ; s=nz.s ; s.q25=nz.s.q25 ; s.q75 = nz.s.q75
    df <- data.frame(
    genotype = rep(unique(o$Genotype),2),
    name = rep(c("Measured",     "Simulated"),each=length(unique(o$Genotype))),
    mean = c(tapply(o$Yield,o$Genotype,mean, na.rm=T),     tapply(s$Yield,s$Genotype,mean, na.rm=T)),
    q.25 = c(tapply(o.q25$Yield,o$Genotype,mean, na.rm=T), tapply(s.q25$Yield,s$Genotype,mean, na.rm=T)),
    q.75 = c(tapply(o.q75$Yield,o$Genotype,mean, na.rm=T), tapply(s.q75$Yield,s$Genotype,mean, na.rm=T))
  )
    df <- df[order(df$genotype, df$name),]
    return(df)
}

#---------------------
# read New-Zeland data
#---------------------

setwd(paste0(ExpDatasets.wd, "NZ2020//"))
# Read simulations
nz.sim.sum <- read.table("Summary_step1(formated).txt",  header=T, stringsAsFactors=F)
nz.sim.sum <- nz.sim.sum[order(nz.sim.sum$Model, nz.sim.sum$TRNO), ]
# read observations
nz.obs.sum.med  <- read.table("AgMIP_4_NZ2020_Observation_summary_median.txt" , header=T, stringsAsFactors=F)
nz.obs.sum.q.25 <- read.table("AgMIP_4_NZ2020_Observation_summary_q.25.txt"   , header=T, stringsAsFactors=F)
nz.obs.sum.q.75 <- read.table("AgMIP_4_NZ2020_Observation_summary_q.75.txt"   , header=T, stringsAsFactors=F)

nz.obs.sum.med$Year <- nz.obs.sum.med$Year + 1
nz.obs.sum.q.25$Year <- nz.obs.sum.q.25$Year + 1
nz.obs.sum.q.75$Year <- nz.obs.sum.q.75$Year + 1
# Data frame for ploting
mytrno <- c(18, 27, 32, 45, 49, 53, 54, 57, 58, 60, 61)  # Late March (18, 27, 45, 53, 57, 60) and april (27, 49, 54, 58, 61) sowing, sowing density 150 grain/m²
# variable to get
nz.var <- c("TRNO","Year","Yield","Biom.ma","HI","GNumber","GDW","sa","aegf")
nz.m <- data.frame(nz.sim.sum[nz.sim.sum$Model%in%s.model  & nz.sim.sum$TRNO%in%mytrno, c("Model",nz.var)], Location="NZ", Genotype="Wakanui")
nz.s   <- summaryBy(.~TRNO , id=~Year+Genotype+Location, keep.names = T , FUN = function(x){median(x,na.rm=TRUE)},              data = nz.m)
nz.s.q25 <- summaryBy(.~TRNO , id=~Year+Genotype+Location, keep.names = T , FUN = function(x){quantile(x,probs=0.25,na.rm=T)}   , data = nz.m)
nz.s.q75 <- summaryBy(.~TRNO , id=~Year+Genotype+Location, keep.names = T , FUN = function(x){quantile(x,probs=0.75,na.rm=T)}   , data = nz.m)

nz.o     <- data.frame(nz.obs.sum.med[nz.obs.sum.med$TRNO%in%mytrno,    nz.var], Location="NZ", Genotype="Wakanui")
nz.o.q25 <- data.frame(nz.obs.sum.q.25[nz.obs.sum.q.25$TRNO%in%mytrno , nz.var], Location="NZ", Genotype="Wakanui")
nz.o.q75 <- data.frame(nz.obs.sum.q.75[nz.obs.sum.q.75$TRNO%in%mytrno , nz.var], Location="NZ", Genotype="Wakanui")

nz <- do.df.4.barplot(nz.o, nz.o.q25, nz.o.q75, nz.s, nz.s.q25, nz.s.q75)

nz.o <- nz.o[,c("Location","TRNO","Year","Genotype","Yield","Biom.ma","HI","GNumber","GDW","sa", "aegf")]
nz.s <- nz.s[,c("Location","TRNO","Year","Genotype","Yield","Biom.ma","HI","GNumber","GDW","sa", "aegf")]

# --------------
# read ROTS data
# --------------

setwd(paste0(ExpDatasets.wd, "Arvalis//"))
# Read simulations
fr.sim.sum <- read.table(paste0("Summary_step3(formated).txt"),  header=T, stringsAsFactors=FALSE)
fr.sim.sum <- fr.sim.sum[order(fr.sim.sum$Model , fr.sim.sum$TRNO) , ]

# Read observations
fr.obs.sum.med  <- read.table("AgMIP_4_ROTS_Observation_summary_median.txt"  , header=T, sep="\t", stringsAsFactors=F)
fr.obs.sum.q.25 <- read.table("AgMIP_4_ROTS_Observation_summary_q.25.txt"    , header=T, sep="\t", stringsAsFactors=F)
fr.obs.sum.q.75 <- read.table("AgMIP_4_ROTS_Observation_summary_q.75.txt"    , header=T, sep="\t", stringsAsFactors=F)

# Variables to get
fr.var <- c("TRNO","Year","Yield","Biom.ma","HI","GNumber","GDW","sa","aegf")

# Data frame for ploting
fr.m     <- data.frame(fr.sim.sum[fr.sim.sum$Model%in%s.model  & fr.sim.sum$NWunlimited=="UW", c("Model",fr.var)], Location="RO", Genotype="Apache")
fr.s     <- summaryBy(.~TRNO , id=~Year+Genotype+Location, keep.names = T , FUN = function(x){median(x,na.rm=TRUE)},              data = fr.m)
fr.s.q25 <- summaryBy(.~TRNO , id=~Year+Genotype+Location, keep.names = T , FUN = function(x){quantile(x,probs=0.25,na.rm=T)}   , data = fr.m)
fr.s.q75 <- summaryBy(.~TRNO , id=~Year+Genotype+Location, keep.names = T , FUN = function(x){quantile(x,probs=0.75,na.rm=T)}   , data = fr.m)

fr.o     <- data.frame(fr.obs.sum.med[,  fr.var], Location="RO", Genotype="Apache")
fr.o.q25 <- data.frame(fr.obs.sum.q.25[, fr.var], Location="RO", Genotype="Apache")
fr.o.q75 <- data.frame(fr.obs.sum.q.75[, fr.var], Location="RO", Genotype="Apache")

fr <- do.df.4.barplot(fr.o, fr.o.q25, fr.o.q75, fr.s, fr.s.q25, fr.s.q75)

fr.o <- fr.o[,c("Location","TRNO","Year","Genotype","Yield","Biom.ma","HI","GNumber","GDW","sa", "aegf")]
fr.s <- fr.s[,c("Location","TRNO","Year","Genotype","Yield","Biom.ma","HI","GNumber","GDW","sa", "aegf")]

# ----------------
# read CIMMYT data
# ----------------

# Read Simulations
setwd(paste0(ExpDatasets.wd, "BxW//"))
ci.sum <- read.table("Summary_2-DH(formated).txt",  header=T, stringsAsFactors=FALSE)
ci.sum <- ci.sum[order(ci.sum$Model, ci.sum$TRNO), ]
# Read observations
ci.obs.med <- read.table("AgMIP_4_BxW_Observation_summary_median.txt" , header=T, na.strings="na", stringsAsFactors=F)
ci.obs.q25 <- read.table("AgMIP_4_BxW_Observation_summary_q.25.txt"   , header=T, na.strings="na", stringsAsFactors=F)
ci.obs.q75 <- read.table("AgMIP_4_BxW_Observation_summary_q.75.txt"   , header=T, na.strings="na", stringsAsFactors=F)
# Keep only the tretments that were simulated
ci.obs.med <- ci.obs.med[-which(ci.obs.med$TRNO %in% c(64:66,69:71)), ]
ci.obs.q25 <- ci.obs.q25[-which(ci.obs.q25$TRNO %in% c(64:66,69:71)), ]
ci.obs.q75 <- ci.obs.q75[-which(ci.obs.q75$TRNO %in% c(64:66,69:71)), ]
# variabls to get
ci.var <- c("Location","TRNO","Year","Genotype","Yield","Biom.ma","HI","GNumber","GDW","sa","aegf")

# Data frame for ploting
ci.m     <- data.frame(ci.sum[ci.sum$Model%in%s.model, ci.var])
ci.s     <- summaryBy(.~TRNO , id=~Year+Genotype+Location, keep.names = T , FUN = function(x){median(x,na.rm=TRUE)},              data = ci.m)
ci.s.q25 <- summaryBy(.~TRNO , id=~Year+Genotype+Location, keep.names = T , FUN = function(x){quantile(x,probs=0.25,na.rm=T)}   , data = ci.m)
ci.s.q75 <- summaryBy(.~TRNO , id=~Year+Genotype+Location, keep.names = T , FUN = function(x){quantile(x,probs=0.75,na.rm=T)}   , data = ci.m)

ci.o     <- ci.obs.med[, ci.var]
ci.o.q25 <- ci.obs.q25[, ci.var]
ci.o.q75 <- ci.obs.q75[, ci.var]

# Valdivia, Chile
va <- do.df.4.barplot(ci.o[ci.o$TRNO%in%c(62,63,73,74),],
                      ci.o.q25[ci.o.q25$TRNO%in%c(62,63,73,74),],
                      ci.o.q75[ci.o.q75$TRNO%in%c(62,63,73,74),],
                      ci.s[ci.s$TRNO%in%c(62,63,73,74),],
                      ci.s.q25[ci.s.q25$TRNO%in%c(62,63,73,74),],
                      ci.s.q75[ci.s.q75$TRNO%in%c(62,63,73,74),])
# Balcarce, Argentina
ba <- do.df.4.barplot(ci.o[ci.o$TRNO%in%c(67,75),],
                      ci.o.q25[ci.o.q25$TRNO%in%c(67,75),],
                      ci.o.q75[ci.o.q75$TRNO%in%c(67,75),],
                      ci.s[ci.s$TRNO%in%c(67,75),],
                      ci.s.q25[ci.s.q25$TRNO%in%c(67,75),],
                      ci.s.q75[ci.s.q75$TRNO%in%c(67,75),])
# Obregon, Mexico
co <- do.df.4.barplot(ci.o[ci.o$TRNO%in%c(68,76),],
                      ci.o.q25[ci.o.q25$TRNO%in%c(68,76),],
                      ci.o.q75[ci.o.q75$TRNO%in%c(68,76),],
                      ci.s[ci.s$TRNO%in%c(68,76),],
                      ci.s.q25[ci.s.q25$TRNO%in%c(68,76),],
                      ci.s.q75[ci.s.q75$TRNO%in%c(68,76),])

ci.o <- ci.o[,c("Location","TRNO","Year","Genotype","Yield","Biom.ma","HI","GNumber","GDW","sa", "aegf")]
ci.s <- ci.s[,c("Location","TRNO","Year","Genotype","Yield","Biom.ma","HI","GNumber","GDW","sa", "aegf")]


rm(nz.sim.sum, nz.obs.sum.med, nz.obs.sum.q.25, nz.obs.sum.q.75,  nz.s.q25, nz.s.q75, nz.o.q25, nz.o.q75, 
   fr.sim.sum, fr.obs.sum.med, fr.obs.sum.q.25, fr.obs.sum.q.75, fr.s.q25, fr.s.q75, fr.o.q25, fr.o.q75,
   ci.sum, ci.obs.med, ci.obs.q25, ci.obs.q75, ci.s.q25, ci.s.q75,  ci.o.q25, ci.o.q75, mytrno)

# save all rearranged data in a list to plot barplot in the global map
setwd(ExpDatasets.wd)
saveRDS(list(nz=nz, fr=fr, va=va, co=co, ba=ba), file=paste0("MME for Expdataset_Step", step.mme.expdataset, " models.RDS"))


# combine and merge all data sets
o <- rbind(fr.o, nz.o, ci.o)
s <- rbind(fr.s, nz.s, ci.s)
# Reoder according to TRNO
o <- o[order(o$TRNO),]
s <- s[order(s$TRNO),]
# Save all data sets 
setwd(ExpDatasets.wd)
saveRDS(s, file=paste0("MME all datasets for stat calculations", step.mme.expdataset, " models.RDS"))
saveRDS(o, file=paste0("Obs all datasets for stat calculations", step.mme.expdataset, " models.RDS"))

rm(s.model, ba, ci.m, co, fr, fr.m, nz, nz.m, va, fr.o, fr.s, nz.s, nz.o, ci.o, ci.s)
