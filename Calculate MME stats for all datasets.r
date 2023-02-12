#-----------
# function 
# do.RMSE.EF
#-----------

do.RMSE.EF <- function(S , O , Var , ...)
{
  # Create matrix to store the results
  MSE <- matrix(NA, ncol = dim(S)[2] , nrow = 1)   # mean squared error (MSE)
  colnames(MSE) <- colnames(S) 
  RMSE  <- MSE   # root mean squared error (RMSE)
  RRMSE <- MSE   # relative root mean squared error (RRMSE)
  RMSRE <- MSE   # root mean squared relative error
  
  ## regression of simulations on observations
  r2         <- MSE   # coefficient of determination (r2)
  slope      <- MSE   # slope
  intercept  <- MSE   # intercept
  
  ##  Decomposition of RMSE after Gauch et al. (2003) Agron. J. 95:1442-1446
  SB <- MSE   # squared bias (SB)
  LC <- MSE   # lack of corelation (LC)
  NU <- MSE   # non-unity slope (NU)
  EF <- MSE   # modeling efficiency (EF)
  
  # Mean bias
  MBE <- MSE
  
  for(i in Var)   #  loop over variables
  {#i="sa"
    # Remove the treatment(s) for which Simulation and/or observation is missing
    na <- union(which(is.na(O[,i])) , which(is.na(S[,i])))
    if(length(na) != 0)
    {
      Si <- S[-na , i]
      Oi <- O[-na , i]
    }
    if(length(na) == 0)  
    {
      Si <- S[ , i]
      Oi <- O[ , i]
    }
    if(all(is.na(Si)) == F | length(Oi) != 0 | all(is.na(Oi)) == F | length(Oi) != 0)
    {
      SE        <- (Si - Oi)^2   # squared error (SE)
      MSE[,i]   <- mean(SE)
      RMSE[,i]  <- sqrt(MSE[,i])
      RRMSE[,i] <- 100 * RMSE[,i] / mean(Oi)
      
      # Calculate the root mean squared relative error (RMSRE)
      for(j in 1 : dim(S)[1])   # loop over the treatments
      {
        if(all(is.na(Si[j])) == F)
        {  
          Oij <- unlist(Oi[j])
          Sij <- unlist(Si[j])
          # Remove the treatments for which Simulation and/or observation is missing
          na <- union(which(is.na(Oij)) , which(is.na(Sij)))
          if(length(na) != 0)
          {
            Sij <- Sij[-na]
            Oij <- Oij[-na]
          }     
          re <- ((Oij - Sij) / Oij)^2
          if(is.infinite(re) == T) re <- NA
          if(j == 1) RE <- re
          if(j != 1) RE <- c(RE , re)
        }
      }
      RMSRE[,i] <- 100 * round(sqrt(mean(RE , na.rm = T)),4)
      
      # linear regression of O over S
      lm1     <- lm(Oi ~ Si)
      sum.lm1 <- summary(lm1)
      if(dim(sum.lm1$coef)[1]  != 1)    ### If simulated values are constant, then R does not return the slope, which is then equal to 0
      { 
        slope[ , i] <- sum.lm1$coef[2,1]
      } else
      {
        slope[ , i] <- 0
      }
      intercept[ , i] <- sum.lm1$coef[1,1]
      r2[ , i]        <- sum.lm1$r.squared
      
      # Calculate MSE components
      SB[,i] <- (mean(Oi) - mean(Si))^2
      LC[,i] <- (1 - r2[,i])  * ( sum((Oi - mean(Oi))^2) / length(Oi) )
      NU[,i] <- (1 - slope[,i])^2 * ( sum((Si - mean(Si))^2) / length(Si) )
      
      # Calaculate mean bias error
      MBE[,i] <- sqrt(SB[,i])
    
      # Calculate modelling efficiency
      EF[,i] <- 1 - sum(SE) / sum( (Oi - mean(Oi))^2 )
    }
  }
  return(list(MSE = round(MSE,4) , RMSE = round(RMSE,4) , RRMSE = round(RRMSE,2) , RMSRE = round(RMSRE,2) , SB        = round(SB,4) , LC = round(LC,4) ,
              NU  = round(NU,4)  , MBE  = round(MBE,4)  , EF    = round(EF,4)    , r2    = round(r2,2)    , slope = round(slope,2)  , intercept = round(intercept,4)))
}   ###   END FUNCTION do.RMSE.EF
  

ExpDatasets.wd <- c("C:/1-Pierre/3-Projets/En cours/AgMIP/AgMIP_4/agmip_phase4_globalsites/ExpDatasets/") 

# Read observes and simulated data sets
mme <- readRDS(paste0(ExpDatasets.wd, "MME all datasets for stat calculations2 models.RDS"))
obs <- readRDS(paste0(ExpDatasets.wd, "Obs all datasets for stat calculations2 models.RDS"))
mme$loc.cv <- paste(mme$Location, mme$Genotype, sep=".")
obs$loc.cv <- paste(obs$Location, obs$Genotype, sep=".")

mme$HI <- mme$Yield / mme$Biom.ma
obs$HI <- obs$Yield / obs$Biom.ma

varSum <- c("Yield", "Biom.ma", "HI", "GNumber", "GDW", "sa", "aegf")

## Build a matrix to store the RMSE and EF                     
RMSE <- as.data.frame(matrix(NA , nrow =  length(unique(mme$loc.cv)) , ncol = length(varSum)))                            
rownames(RMSE) <- c("RO.Apache", "NZ.Wakanui", "CO.Bacanora", "CO.DH", "BA.Bacanora", "BA.DH", "VA.Bacanora", "VA.DH") 
colnames(RMSE) <- varSum
RRMSE <- RMSE ; EF <- RMSE ; MBE <- RMSE ; r2 <- RMSE

for(iLoc.cv in unique(mme$loc.cv)) # iLoc.cv = "VA.DH"
{
  S <- as.matrix(mme[(mme$loc.cv == iLoc.cv) , varSum])
  O <- as.matrix(obs[(obs$loc.cv==iLoc.cv)   , varSum])
  
  eval.sum <- do.RMSE.EF(S = S , O = O , Var = varSum) 
  
  RMSE[rownames(RMSE)==iLoc.cv, ]   <- as.vector(unlist(eval.sum$RMSE))
  RRMSE[rownames(RRMSE)==iLoc.cv, ] <- eval.sum$RRMSE
  MBE[rownames(MBE)==iLoc.cv, ]     <- eval.sum$MBE
  r2[rownames(r2)==iLoc.cv, ]       <- eval.sum$r2
  EF[rownames(EF)==iLoc.cv, ]       <- eval.sum$EF
}

# Calculate stats for all the treatments for the standard cultivars
S <- as.matrix(mme[(mme$Genotype %in% c("Apache", "Wakanui", "Bacanora")) , varSum])
O <- as.matrix(obs[(obs$Genotype %in% c("Apache", "Wakanui", "Bacanora")) , varSum])
eval.sum <- do.RMSE.EF(S = S , O = O , Var = varSum) 
RMSE["overall.stdCV",  ] <- as.vector(unlist(eval.sum$RMSE))
RRMSE["overall.stdCV", ] <- eval.sum$RRMSE
MBE["overall.stdCV", ]   <- eval.sum$MBE
r2["overall.stdCV", ]    <- eval.sum$r2
EF["overall.stdCV",    ] <- eval.sum$EF

# Calculate stats for all the treatments for the high-yield DH line
S <- as.matrix(mme[(mme$Genotype == "DH") , varSum])
O <- as.matrix(obs[(obs$Genotype == "DH") , varSum])
eval.sum <- do.RMSE.EF(S = S , O = O , Var = varSum) 
RMSE["overall.DH",  ] <- as.vector(unlist(eval.sum$RMSE))
RRMSE["overall.DH", ] <- eval.sum$RRMSE
MBE["overall.DH", ]   <- eval.sum$MBE
r2["overall.DH", ]    <- eval.sum$r2
EF["overall.DH",    ] <- eval.sum$EF


# Round number
RMSE <- round(RMSE,2)
RMSE[,"GNumber"] <- round(RMSE[,"GNumber"], 0)
RRMSE <- round(RRMSE,2)
MBE   <- round(MBE,2)
r2    <- round(r2,2)
EF    <- round(EF,2)

write.table(RMSE   , file = paste0(ExpDatasets.wd , "RMSE.txt")  , sep = "\t" , row.names = T)                      
write.table(RRMSE  , file = paste0(ExpDatasets.wd , "RRMSE.txt") , sep = "\t" , row.names = T)                      
write.table(MBE    , file = paste0(ExpDatasets.wd , "MBE.txt") , sep = "\t" , row.names = T)                      
write.table(r2     , file = paste0(ExpDatasets.wd , "r2.txt") , sep = "\t" , row.names = T)                      
write.table(EF     , file = paste0(ExpDatasets.wd , "EF.txt")    , sep = "\t" , row.names = T)

                              