#-----------------
# Merge simulation
# results
#-----------------

setwd(paste0(main.wd, "Incoming Data\\", Step, "\\"))
data <- read_bulk(subdirectories = T,
                 verbose = F,
                 fun = read.table,
                 skip=7,
                 stringsAsFactors=FALSE,
                 na.strings = c("na", "n/a", "NA", "NaN", "NA-NA-NA", "", "99","-99", "-999", "-9999", "-0.099", "n/a" ))
gc()

# Add column names
setwd(paste0(main.wd, "\\Incoming Data\\", Step, "\\AW\\"))
header <- as.vector(read.table(list.files()[1],skip = 5,stringsAsFactors = FALSE)[1,])
names(data) <- c(header[1,],"WD","file")
rm(header)

#-------------------
# remove models
# with some problems
#-------------------

if(exists('rm.model')==T)
{
  data <- data[-which(data$Model%in%rm.model),]
} 

# Add columns for the sites, rcps, gcms, traits, and planting years
data$site   <- as.numeric(substr(data$file, 3, 4))
data$rcp    <- substr(data$file, 5, 5)
if(Step == "Step01")
{
  data[which(nchar(data$file)==11),"gcm"] <- substr(data[which(nchar(data$file)==11),"file"], 6, 6)
  data[which(nchar(data$file)==10),"trait"] <- substr(data[which(nchar(data$file)==10),"file"], 6, 6)
  data[which(nchar(data$file)==11),"trait"] <- substr(data[which(nchar(data$file)==11),"file"], 7, 7)
  data$Planting.year <- as.numeric(format(as.Date(data$Planting.date),"%Y"))
}
if(Step == "Step02")
{
  data[which(nchar(data$file)==14),"gcm"] <- substr(data[which(nchar(data$file)==14),"file"], 6, 6)
  data[which(nchar(data$file)==13),"trait"] <- substr(data[which(nchar(data$file)==13),"file"], 6, 6)
  data[which(nchar(data$file)==14),"trait"] <- substr(data[which(nchar(data$file)==14),"file"], 7, 7)
  data[which(nchar(data$file)==13),"ferti"]<-as.numeric(substr(data[which(nchar(data$file)==13),"file"], 7, 9))
  data[which(nchar(data$file)==14),"ferti"]<-as.numeric(substr(data[which(nchar(data$file)==14),"file"], 8, 10))
}

# Replace "." by "-" in column names
names(data) <- gsub("-", ".", names(data))

# Rename the RCPs
data[which(data$rcp=="0"),"rcp"] <- "00" ; data[which(data$rcp=="G"),"rcp"] <- "45" ; data[which(data$rcp=="I"),"rcp"] <- "85" 

# For baseline change gcm code to "_"
data[is.na(data$gcm),"gcm"] <- "_"

# Rename column Model
names(data)[grep("Model",names(data))] <- "model"

# Create a list of summary information
summary.data <- list(nb.model = length(unique(data$model)))
summary.data[["model"]]         <- unique(data$model)
summary.data[["nb.site"]]       <- length(unique(data$site))
summary.data[["site"]]          <- unique(data$site)
summary.data[["Planting.year"]] <- unique(data$Planting.year)
summary.data[["trait"]]         <- unique(data$trait)
summary.data[["gcm"]]           <- unique(data$gcm)
summary.data[["rcp"]]           <- unique(data$rcp)
if(Step == "Step02") summary.data[["ferti"]]           <- unique(data$ferti)
print(summary.data)
rm(summary.data)

# Remove the WD and file columns
data <- data[,-which(names(data)%in%c("WD","file"))]

# save the merged data
saveRDS(data, paste0(DataAnalysis.wd, "merged summary_", Step, ".RDS"))
rm(data)
gc()
