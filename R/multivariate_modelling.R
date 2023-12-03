rm(list=ls())

library("readxl")
library(dplyr)
library(reshape2)
library(ggplot2)
library(MTVGARCH)  #ver. 0.8.3

#setwd("D:/GoogleDrive/MyDocs/Annastiina/Topics/MTVGJR_MGARCH/Project16_Rainfall/R")   #Anna's Laptop - GOOGLE DRIVE
#setwd("C:/Users/silvenno/Google Drive/MyDocs/Annastiina/Topics/MTVGJR_MGARCH/Project16_Rainfall/R")   #Anna's Work PC - GOOGLE DRIVE

setwd("C:/Source/Repos/nao") 
#source("clsCCC.r")
#source("manually source code for reference latest/all_common.r")
#source("manually source code for reference latest/clsTVGARCH.r")
#source("clsSTCC_x.r")

my_data <- read_excel("Data/stations.xlsx")

# split data by month - redo only if demeaned data changes
if(FALSE){
  my_e <- read_excel("Data/DeMeanedData_NOV2023.xlsx")
  my_e_JAN <- my_e %>% filter(month==1)
  saveRDS(my_e_JAN,"Data/Rain_JAN_eData.RDS")
  my_e_FEB <- my_e %>% filter(month==2)
  saveRDS(my_e_FEB,"Data/Rain_FEB_eData.RDS")
  my_e_MAR <- my_e %>% filter(month==3)
  saveRDS(my_e_MAR,"Data/Rain_MAR_eData.RDS")
  my_e_APR <- my_e %>% filter(month==4)
  saveRDS(my_e_APR,"Data/Rain_APR_eData.RDS")
  my_e_MAY <- my_e %>% filter(month==5)
  saveRDS(my_e_MAY,"Data/Rain_MAY_eData.RDS")
  my_e_JUN <- my_e %>% filter(month==6)
  saveRDS(my_e_JUN,"Data/Rain_JUN_eData.RDS")
  my_e_JUL <- my_e %>% filter(month==7)
  saveRDS(my_e_JUL,"Data/Rain_JUL_eData.RDS")
  my_e_AUG <- my_e %>% filter(month==8)
  saveRDS(my_e_AUG,"Data/Rain_AUG_eData.RDS")
  my_e_SEP <- my_e %>% filter(month==9)
  saveRDS(my_e_SEP,"Data/Rain_SEP_eData.RDS")
  my_e_OCT <- my_e %>% filter(month==10)
  saveRDS(my_e_OCT,"Data/Rain_OCT_eData.RDS")
  my_e_NOV <- my_e %>% filter(month==11)
  saveRDS(my_e_NOV,"Data/Rain_NOV_eData.RDS")
  my_e_DEC <- my_e %>% filter(month==12)
  saveRDS(my_e_DEC,"Data/Rain_DEC_eData.RDS")
}

# All our CCC & STCC models use a standard form of multivariate-multiplicitive tv-garch object, called a 'ntvgarch' object.
# These ntvgarch objects are constructed from a basic list() of univariate models and a character string of names.
#  So, We want to build a 12 lists of mtvgarch objects, one list for each month JAN..DEC
#  Each list will contain a model for each station, i.e. Each list is 28 elements long.

# 1. Get the names from our dataset: ####
stationNames <- my_data$code

regions <- c("North","Atlantic","CentralEurope","West","SouthMediterranean")

region <- list()
region$North <- c("HEL","SPG","TLL","LUN","CPH","OKE")  # 6 Cities
region$Atlantic <- c("DUB","HOO","DEB","ORK","OXF","UCC")  # 6 Cities
region$CentralEurope <- c("ERF","PRG","VIE","BUD","KLU","SIB","TRS")  # 7 Cities
region$West <- c("TRI","LUX","SXB","ZRH","DIJ","BOD","TLS")  # 7 Cities
region$SouthMediterranean <- c("MRS","PGF","LIS","BEI","LUQ","SEN")  # 6 Cities

regionPairs <- list()
regionPairs$North <- c("None")
regionPairs$Atlantic <- c("None")
regionPairs$CentralEurope <- c("None")
regionPairs$West <- c("None")
regionPairs$SouthMediterranean <- c("None")

for (r in 1:length(regionPairs)){
  pairNames<- NULL
  for (i in 1:(length(region[[r]])-1)){
    for (j in (i+1):length(region[[r]])){
       pairNames <- c(pairNames,paste0(region[[r]][i],"-",region[[r]][j]))
    }
  }
  regionPairs[[r]]<-pairNames
}

## Determine cross-region pairs: ----

## First define our little pairname calculator as a function:
get.pairnames = function(start,end){
    pairNames <- NULL
    for (i in 1:(length(region[[start]]))){
        for (j in 1:length(region[[end]])){
            pairNames <- c(pairNames,paste0(region[[start]][i],"-",region[[end]][j]))
        }
    }
    return(pairNames)
}

## 1.1 North_By_Atlantic ----
regionPairs$North_By_Atlantic <- vector("character")
start = which(!is.na(match(names(regionPairs),"North")))
end = which(!is.na(match(names(regionPairs),"Atlantic")))
regionPairs$North_By_Atlantic <- get.pairnames(start,end)

## 1.2 North_By_CentralEurope ----
regionPairs$North_By_CentralEurope <- vector("character")
start = which(!is.na(match(names(regionPairs),"North")))
end = which(!is.na(match(names(regionPairs),"CentralEurope")))
regionPairs$North_By_CentralEurope <- get.pairnames(start,end)

## 1.3 North_By_West ----
regionPairs$North_By_West <- vector("character")
start = which(!is.na(match(names(regionPairs),"North")))
end = which(!is.na(match(names(regionPairs),"West")))
regionPairs$North_By_West <- get.pairnames(start,end)

## 1.4 North_By_SouthMediterranean ----
regionPairs$North_By_SouthMediterranean <- vector("character")
start = which(!is.na(match(names(regionPairs),"North")))
end = which(!is.na(match(names(regionPairs),"SouthMediterranean")))
regionPairs$North_By_SouthMediterranean <- get.pairnames(start,end)

## 1.5 Atlantic_By_CentralEurope ----
regionPairs$Atlantic_By_CentralEurope <- vector("character")
start = which(!is.na(match(names(regionPairs),"Atlantic")))
end = which(!is.na(match(names(regionPairs),"CentralEurope")))
regionPairs$Atlantic_By_CentralEurope <- get.pairnames(start,end)

## 1.6 Atlantic_By_West ----
regionPairs$Atlantic_By_West <- vector("character")
start = which(!is.na(match(names(regionPairs),"Atlantic")))
end = which(!is.na(match(names(regionPairs),"West")))
regionPairs$Atlantic_By_West <- get.pairnames(start,end)

## 1.7 Atlantic_By_SouthMediterranean ----
regionPairs$Atlantic_By_SouthMediterranean <- vector("character")
start = which(!is.na(match(names(regionPairs),"Atlantic")))
end = which(!is.na(match(names(regionPairs),"SouthMediterranean")))
regionPairs$Atlantic_By_SouthMediterranean <- get.pairnames(start,end)

## 1.8 CentralEurope_By_Weste ----
regionPairs$CentralEurope_By_West <- vector("character")
start = which(!is.na(match(names(regionPairs),"CentralEurope")))
end = which(!is.na(match(names(regionPairs),"West")))
regionPairs$CentralEurope_By_West <- get.pairnames(start,end)

## 1.9 CentralEurope_By_SouthMediterranean ----
regionPairs$CentralEurope_By_SouthMediterranean <- vector("character")
start = which(!is.na(match(names(regionPairs),"CentralEurope")))
end = which(!is.na(match(names(regionPairs),"SouthMediterranean")))
regionPairs$CentralEurope_By_SouthMediterranean <- get.pairnames(start,end)

## 1.10 West_By_SouthMediterranean ----
regionPairs$West_By_SouthMediterranean <- vector("character")
start = which(!is.na(match(names(regionPairs),"West")))
end = which(!is.na(match(names(regionPairs),"SouthMediterranean")))
regionPairs$West_By_SouthMediterranean <- get.pairnames(start,end)

# 2. Truncate data such that it starts 1851m3 for all locations - DONE, just read in data. ####
if (FALSE){

  for (m in  1:12)
  {
    
    # SET UP DATA
    {  
      # get e_mat (incl. all stations) & NAO (current)
      MthName <- stringr::str_to_upper(month.abb[m])
      dataFileName <- paste0("Data/","Rain_",MthName,"_eData.RDS")
      dataFile <- readRDS(dataFileName)
      naoIndex <- as.matrix(dataFile[,35])
      dataFile <- dataFile[,-c(1,2)]
      e_mat <- as.matrix(dataFile)
    } # end: set up data
  } # end: for (m in  1:12)
}

# 3. Test of CCC ####
# Test for CCC: ----

cccmat <- stccmat <- matrix(nrow=12,ncol=5)

m = 1
for (regi in 1:5){
  pccc <- pstcc <- NULL
  for (m in 1:12){
    region_names <- region[[regi]]
    MthName <- stringr::str_to_upper(month.abb[m])
    ntvFileName <- paste0("Data/","Rain_",MthName,"_ntvgarch.RDS")
    ntv_models <- readRDS(ntvFileName)
    ntv_list = ntv_models[region_names]
    ntv <- ntvgarch(ntv_list,region_names)
    e <- matrix()
    for (n in 1:ntv@N){
      if(n==1) e <- ntv[[1]]@e
      else e = cbind(e,ntv[[n]]@e)
    }
    Tobs <- NROW(e)
    
    NullHyp = ccc(length(region_names),ntv)
    NullHyp = estimateCCC(e,NullHyp,estCtrl)

    st = seq(-0.5,0.5,length.out=Tobs)
    testOrd=3
    ccctest <- test.CCCParsim(e,NullHyp,st,testOrd)
    testOrd=3
    stcctest = test.CCCvSTCC1(e,NullHyp,st,testOrd)
    pccc <- c(pccc,ccctest[2])
    pstcc <- c(pstcc,stcctest[2])
  }
  cccmat[,regi] <- pccc
  stccmat[,regi] <- pstcc
}

rownames(cccmat) <- rownames(stccmat) <- month.abb[1:12]

format(scientific=FALSE, round(stccmat,4))
format(scientific=FALSE, round(cccmat,4))


# --- Test for CCC: PAIRWISE for a selection of regions and months ####
# Note: parsimonious and regular tests are identical in bivariate case, do only regular
## pick month and region ----
mo = 1
regi = region$West

source("R/clsCCC.r")
source("R/all_common.r")
source("R/clsTVGARCH.r")

npairs <- length(regi)
stccmat1 <- stccmat2 <- matrix(nrow=npairs,ncol=npairs)
rownames(stccmat1) <- colnames(stccmat1) <- rownames(stccmat2) <- colnames(stccmat2) <- regi
MthName <- stringr::str_to_upper(month.abb[mo])
ntvFileName <- paste0("Data/","Rain_",MthName,"_ntvgarch.RDS")
ntv_models <- readRDS(ntvFileName)
for (i in 1:(npairs-1)){
  for (j in (i+1):npairs){
    pairnames <- c(regi[i],regi[j])
    ntv_list = ntv_models[pairnames]
    ntv <- ntvgarch(ntv_list,pairnames)
    e <- matrix()
    for (n in 1:ntv@N){
      if(n==1) e <- ntv[[1]]@e
      else e = cbind(e,ntv[[n]]@e)
    }
    Tobs <- NROW(e)
    this <- new("ccc_class")
    NullHyp = ccc(2,ntv)
    NullHyp = estimateCCC(e,NullHyp,estCtrl)
    st = seq(-0.5,0.5,length.out=Tobs)
    testOrd=1
    stcctest = test.CCCvSTCC1(e,NullHyp,st,testOrd)
    stccmat1[j,i] <- stcctest[2]
    testOrd=2
    stcctest = test.CCCvSTCC1(e,NullHyp,st,testOrd)
    stccmat2[j,i] <- stcctest[2]
  }
}

format(scientific=FALSE, round(stccmat1,4))
format(scientific=FALSE, round(stccmat2,4))


# Modification of CCC Test to handle cross-region pairs: ----

# --- Test for CCC: PAIRWISE for a selection of cross-regions and months ####
# Note: parsimonious and regular tests are identical in bivariate case, do only regular
## pick month and region ----
mo = 1
region1 = region$North
region2 <- region$CentralEurope
pairNames <- regionPairs$North_By_CentralEurope

source("R/clsCCC.r")
source("R/all_common.r")
source("R/clsTVGARCH.r")

npairs <- length(pairNames)
stccmat1 <- stccmat2 <- matrix(nrow=length(region2),ncol=length(region1))

colnames(stccmat1) <- colnames(stccmat2) <- region1
rownames(stccmat1) <- rownames(stccmat2) <- region2

MthName <- stringr::str_to_upper(month.abb[mo])
ntvFileName <- paste0("Data/","Rain_",MthName,"_ntvgarch.RDS")
ntv_models <- readRDS(ntvFileName)

for (i in 1:length(region1)){
    for (j in 1:length(region2)){
        pairname <- c(region1[i],region2[j])
        ntv_list = ntv_models[pairname]
        ntv <- ntvgarch(ntv_list,pairname)
        e <- matrix()
        for (n in 1:ntv@N){
            if(n==1) e <- ntv[[1]]@e
            else e = cbind(e,ntv[[n]]@e)
        }
        Tobs <- NROW(e)
        this <- new("ccc_class")
        NullHyp = ccc(2,ntv)
        NullHyp = estimateCCC(e,NullHyp,estCtrl)
        st = seq(-0.5,0.5,length.out=Tobs)
        testOrd=1
        stcctest = test.CCCvSTCC1(e,NullHyp,st,testOrd)
        stccmat1[j,i] <- stcctest[2]
        testOrd=2
        stcctest = test.CCCvSTCC1(e,NullHyp,st,testOrd)
        stccmat2[j,i] <- stcctest[2]
    }
}

format(scientific=FALSE, round(stccmat1,4))
format(scientific=FALSE, round(stccmat2,4))


# 4. correlations by distance ----
source("clsCDC.r")
locData <- read_xlsx("Data/LongLatiElev.xlsx")
estCtrl = list(verbose=TRUE,calcSE=TRUE)
results <- list()

for(n in seq_along(month.abb)){
#for(n in 2:2){
  
  modelMonth = toupper(month.abb[n])
  
  fileName_0 = paste0("Data/Rain_",modelMonth,"_ntvgarch.RDS")
  NTV <- readRDS(fileName_0)
  
  rm(CDC)
  # Create the CDC models
  CDC <- cdc(NTV,locData)

  ## Alternate Estimation Methods considered (Not used in paper )
  if (TRUE){
    # # Estimate the CDC model using direct distance
     CDC$pars["alpha"] = NA
     CDC$pars["gammaDir"] = 2
     CDC$pars["kappa"] = 0
     #CDC$optimcontrol$parscale = c(2,3,1)
     CDC$optimcontrol$parscale = c(3,1)
     CDC$optimcontrol$reltol = 1e-3
     #CDC$optimcontrol$ndeps = c(1e-9,1e-9,1e-9)
     CDC$optimcontrol$ndeps = c(1e-9,1e-9)
     #CDC <- estimateCDC(CDC,estCtrl,adjFactor = min(0,min(cor(CDC@z))))
     CDC <- estimateCDC(CDC,estCtrl)
    # fileName_1 = paste0("Results/NAO_",modelMonth,"_EstimatedCDC_Direct.RDS")
    # saveRDS(CDC,fileName_1)
    

  }
  

  # Fine tune the optimiser
  #CDC$optimcontrol$parscale = c(100,100,50,1)
  #CDC$optimcontrol$reltol = 1e-7
  #CDC$optimcontrol$maxit = 1500
  
  #CDC <- estimateCDC(CDC,estCtrl)
  #fileName_3 = paste0("Results/NAO_",modelMonth,"_EstimatedCDC_LatLong_Elev.RDS")
  #saveRDS(CDC,fileName_3)
  
  #
  results[[n]] <- CDC
  
  
}


warnings()
# Warning messages:
# 1: In sqrt(diag(qr.solve(-tmp$hessian, tol = 0.001))) : NaNs produced
# 2: In sqrt(diag(qr.solve(-tmp$hessian, tol = 0.001))) : NaNs produced
# 3: In sqrt(diag(qr.solve(-tmp$hessian, tol = 0.001))) : NaNs produced
# 4: In sqrt(diag(qr.solve(-tmp$hessian, tol = 0.001))) : NaNs produced

saveRDS(results,"Data/ALL_NAO_EstimatedCDC_Direct_new.RDS")
# Std Err needed for n: 2,6,7,8,12 ----

## n=2 ----
n=2
modelMonth = toupper(month.abb[n])
fileName_0 = paste0("Data/Rain_",modelMonth,"_ntvgarch.RDS")
NTV <- readRDS(fileName_0)

rm(CDC)
CDC <- cdc(NTV,locData)

# Estimate the CDC model using direct distance
CDC$pars["alpha"] = 0.98
CDC$pars["gammaDir"] = 0.5
CDC$optimcontrol$parscale = c(2,1)
CDC$optimcontrol$reltol = 1e-3
CDC$optimcontrol$ndeps = c(1e-6,1e-11)
CDC <- estimateCDC(CDC,estCtrl,adjFactor = min(0,min(cor(CDC@z))))
#
results[[n]] <- CDC

## n=6 ----
n=6
modelMonth = toupper(month.abb[n])
fileName_0 = paste0("Data/Rain_",modelMonth,"_ntvgarch.RDS")
NTV <- readRDS(fileName_0)

rm(CDC)
CDC <- cdc(NTV,locData)

# Estimate the CDC model using direct distance
CDC$pars["alpha"] = 0.6
CDC$pars["gammaDir"] = 0.6
CDC$optimcontrol$parscale = c(1,1)
CDC$optimcontrol$reltol = 1e-3
CDC$optimcontrol$ndeps = c(1e-11,1e-11)
CDC <- estimateCDC(CDC,estCtrl,adjFactor = min(0,min(cor(CDC@z))))
#
results[[n]] <- CDC

## n=7 ----
n=7
modelMonth = toupper(month.abb[n])
fileName_0 = paste0("Data/Rain_",modelMonth,"_ntvgarch.RDS")
NTV <- readRDS(fileName_0)

rm(CDC)
CDC <- cdc(NTV,locData)

# Estimate the CDC model using direct distance
CDC$pars["alpha"] = 0.66
CDC$pars["gammaDir"] = 0.81
CDC$optimcontrol$parscale = c(1,1)
CDC$optimcontrol$reltol = 1e-4
CDC$optimcontrol$ndeps = c(1e-9,1e-11)
CDC <- estimateCDC(CDC,estCtrl,adjFactor = min(0,min(cor(CDC@z))))
#
results[[n]] <- CDC

## n=8 ----
n=8
modelMonth = toupper(month.abb[n])
fileName_0 = paste0("Data/Rain_",modelMonth,"_ntvgarch.RDS")
NTV <- readRDS(fileName_0)

rm(CDC)
CDC <- cdc(NTV,locData)

# Estimate the CDC model using direct distance
CDC$pars["alpha"] = 0.79
CDC$pars["gammaDir"] = 0.66
CDC$optimcontrol$parscale = c(1,1)
CDC$optimcontrol$reltol = 1e-3
CDC$optimcontrol$ndeps = c(1e-10,1e-11)
CDC <- estimateCDC(CDC,estCtrl,adjFactor = min(0,min(cor(CDC@z))))
#
results[[n]] <- CDC

## n=12 ----
n=12
modelMonth = toupper(month.abb[n])
fileName_0 = paste0("Data/Rain_",modelMonth,"_ntvgarch.RDS")
NTV <- readRDS(fileName_0)

rm(CDC)
CDC <- cdc(NTV,locData)

# Estimate the CDC model using direct distance
CDC$pars["alpha"] = 0.9
CDC$pars["gammaDir"] = 0.5
CDC$optimcontrol$parscale = c(2,1)
CDC$optimcontrol$reltol = 1e-3
CDC$optimcontrol$ndeps = c(1e-10,1e-7)
CDC <- estimateCDC(CDC,estCtrl,adjFactor = min(0,min(cor(CDC@z))))
#
results[[n]] <- CDC

saveRDS(results,"ALL_NAO_EstimatedCDC_Direct.RDS")

CorrResults <- readRDS("Data/ALL_NAO_EstimatedCDC_Direct.RDS")



# 5. Estimate TVCC  ####
# By region: TVCC estimation ####
## ------ pick region and month --------
regi = 1
mo = 4

region_names <- region[[regi]]
MthName <- stringr::str_to_upper(month.abb[mo])
ntvFileName <- paste0("Data/","Rain_",MthName,"_ntvgarch.RDS")
ntv_models <- readRDS(ntvFileName)
ntv_list = ntv_models[region_names]
ntv <- ntvgarch(ntv_list,region_names)
e <- matrix()
for (n in 1:ntv@N){
  if(n==1) e <- ntv[[1]]@e
  else e = cbind(e,ntv[[n]]@e)
}
Tobs <- NROW(e)
N <- length(region_names)
g <- matrix(1,Tobs,N)
for (n in 1:N) {
  g[,n] <- ntv[[n]]$Estimated$tv$g
}
z <- e/sqrt(g)
stccmodel = stcc1(z,ntv)
stccmodel = estimateSTCC1(z,stccmodel)
saveRDS(stccmodel,paste0("Data/STCC_",regions[regi],"_",MthName,".RDS"))

## ---- plot correlations ----
corr <- stccmodel$Estimated$Pt
colnames(corr) = regionPairs[[regi]]
corr1 <- melt(corr, varnames = c("Time","Pair"), value.name = "value")
plot_Dates <- seq(as.Date("1850/1/1"), by = "year", length.out = NROW(corr))
plot_Dates <- rep(plot_Dates,NCOL(corr))
df <-data.frame(plot_Dates,corr1[,2],as.numeric(corr1[,3]))  
colnames(df) <- c("Time","Pair","value") 
title_text <- month.abb[mo]
titleFontSize <- 11
title_margin = margin(t=30,r=0,b=0,l=0)
axisFontSize <- 9  
# Line Thickness
lineSize <- 1.0
corrPlots <- ggplot(df) + ggtitle(title_text) 
corrPlots <- corrPlots  + geom_line(aes(x=as.Date(Time), y=as.numeric(value), color = Pair ), linewidth =lineSize, show.legend = TRUE) 
corrPlots

# 6. By region bivariate: Estimate TVCC and CCC ####
## pick region and month, list pairs that have TVCC, others have CCC ----
source("clsSTCC_x.r")

# NORTH ----
{
  regidx = 1
  regi = region$North
  regipairs = regionPairs$North
  
  mo = 1
  stccpairs = c("HEL-TLL","SPG-TLL","TLL-OKE")
  stccxpairs = c(" ")
  mo = 2
  stccpairs = c(" ")
  stccxpairs = c("HEL-TLL","LUN-OKE")
  mo = 3
  stccpairs = c("HEL-TLL")
  stccxpairs = c("TLL-OKE")
  mo = 4
  stccpairs = c("HEL-TLL","SPG-TLL","TLL-LUN","TLL-OKE")
  stccxpairs = c(" ")
  mo = 5
  stccpairs = c(" ")
  stccxpairs = c("SPG-CPH")
  mo = 6
  stccpairs = c(" ")
  stccxpairs = c("HEL-TLL")
  mo = 7
  stccpairs = c(" ")
  stccxpairs = c("SPG-OKE")
  mo = 8
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 9
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 10
  stccpairs = c(" ")
  stccxpairs = c("HEL-TLL")
  mo = 11
  stccpairs = c(" ")
  stccxpairs = c("LUN-CPH")
  mo = 12
  stccpairs = c(" ")
  stccxpairs = c("SPG-LUN","LUN-CPH")
}

# ATLANTIC ----
{
  regidx = 2
  regi = region$Atlantic
  regipairs = regionPairs$Atlantic
  
  mo = 1
  stccpairs = c(" ")
  stccxpairs = c("HOO-DEB")
  mo = 2
  stccpairs = c("DUB-ORK")
  stccxpairs = c("HOO-DEB")
  mo = 3
  stccpairs = c(" ")
  stccxpairs = c("HOO-DEB")
  mo = 4
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 5
  stccpairs = c("DUB-OXF")
  stccxpairs = c("DUB-DEB")
  mo = 6
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 7
  stccpairs = c(" ")
  stccxpairs = c("HOO-DEB")
  mo = 8
  stccpairs = c("DUB-HOO")
  stccxpairs = c(" ")
  mo = 9
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 10
  stccpairs = c(" ")
  stccxpairs = c("HOO-DEB")
  mo = 11
  stccpairs = c("HOO-UCC")
  stccxpairs = c("HOO-DEB")
  mo = 12
  stccpairs = c("DUB-OXF")
  stccxpairs = c(" ")
}

# CENTRAL EUROPE ----
{
  regidx = 3
  regi = region$CentralEurope
  regipairs = regionPairs$CentralEurope
  
  mo = 1
  stccpairs = c("KLU-TRS")
  stccxpairs = c("VIE-KLU","BUD-TRS")
  mo = 2
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 3
  stccpairs = c(" ")
  stccxpairs = c("VIE-BUD")
  mo = 4
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 5
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 6
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 7
  stccpairs = c(" ")
  stccxpairs = c(" ")
  mo = 8
  stccpairs = c("KLU-TRS")
  stccxpairs = c(" ")
  mo = 9
  stccpairs = c(" ")
  stccxpairs = c("KLU-TRS")
  mo = 10
  stccpairs = c("VIE-TRS","BUD-TRS")
  stccxpairs = c(" ")
  mo = 11
  stccpairs = c(" ")
  stccxpairs = c("VIE-KLU")
  mo = 12
  stccpairs = c("PRG-KLU","KLU-TRS")
  stccxpairs = c(" ")
}



# WEST ----
{
  regidx = 4
  regi = region$West
  regipairs = regionPairs$West

  mo = 1
  stccpairs = c("TRI-LUX","LUX-ZRH","SXB-ZRH","ZRH-DIJ","ZRH-BOD")
  stccxpairs = c("TRI-ZRH","ZRH-TLS","BOD-TLS")
  mo = 2
  stccpairs = c("TRI-LUX","TRI-DIJ")
  mo = 4
  stccpairs = c("TRI-LUX","SXB-DIJ","DIJ-TLS")
  mo = 5
  stccpairs = c("TRI-LUX")
  mo = 7
  stccpairs = c("LUX-ZRH")
  mo = 10
  stccpairs = c("TRI-LUX")
  mo = 12
  stccpairs = c("TRI-LUX","LUX-DIJ")
  
  
}




estCtrl = list(verbose=TRUE,calcSE=TRUE)
nlocs = length(regi)
npairs <- length(regipairs)
corrmat <- matrix(nrow=npairs,ncol=npairs)
rownames(corrmat) <- colnames(corrmat) <- regipairs
MthName <- stringr::str_to_upper(month.abb[mo])
ntvFileName <- paste0("Data/","Rain_",MthName,"_ntvgarch.RDS")
ntv_models <- readRDS(ntvFileName)
bivmodel <- list()
corr <- NULL
k=1
for (i in 1:(nlocs-1)){
  print(paste0("i = ",i))
  for (j in (i+1):nlocs){
    print(paste0("j = ",j))
    pairnames <- c(regi[i],regi[j])
    pairlabel <- paste0(pairnames[1],"-",pairnames[2])
    ntv_list = ntv_models[pairnames]
    ntv <- ntvgarch(ntv_list,pairnames)
    e <- matrix()
    for (n in 1:ntv@N){
      if(n==1) e <- ntv[[1]]@e
      else e = cbind(e,ntv[[n]]@e)
    }
    if (pairlabel %in% stccpairs){
      Tobs <- NROW(e)
      N <- length(pairnames)
      g <- matrix(1,Tobs,N)
      for (n in 1:N) {
        g[,n] <- ntv[[n]]$Estimated$tv$g
      }
      z <- e/sqrt(g)
      stccmodel = stcc1(z,ntv)
      stccmodel = estimateSTCC1(z,stccmodel,estCtrl)
      bivmodel[[k]] <- stccmodel
      corr <- cbind(corr,stccmodel$Estimated$Pt)
    } else if (pairlabel %in% stccxpairs){
      Tobs <- NROW(e)
      N <- length(pairnames)
      g <- matrix(1,Tobs,N)
      for (n in 1:N) {
        g[,n] <- ntv[[n]]$Estimated$tv$g
      }
      z <- e/sqrt(g)
      stccmodel = stcc1(z,ntv)
      zDiv3 <- round(Tobs/3)
      z.start <- zDiv3
      z.end <- 2*zDiv3
      stccmodel$P1 <- cor(z[(z.start:z.end),])
      zDiv6 <- round(Tobs/6)
      z.start <- (1:zDiv6)
      z.end <- ((Tobs-zDiv6):Tobs)
      stccmodel$P2 <- cor(z[c(z.start,z.end),])
      stccmodel = estimateSTCC1_x(z,stccmodel,estCtrl)
      bivmodel[[k]] <- stccmodel
      corr <- cbind(corr,stccmodel$Estimated$Pt)
    } else {
      cccmodel = ccc(2,ntv)
      cccmodel = estimateCCC(e,cccmodel,estCtrl)
      bivmodel[[k]] <- cccmodel
      rhovec <- rep(cccmodel$Estimated$P[2,1],NROW(e))
      corr <- cbind(corr,rhovec)
    }
    k <- k+1
  }
}
colnames(corr) = regipairs
saveRDS(corr,file=paste0("Data/corr_",regions[regidx],"_",MthName,".RDS"))


## plot correlations ----
corr1 <- melt(corr, varnames = c("Time","Pair"), value.name = "value")
plot_Dates <- seq(as.Date("1850/1/1"), by = "year", length.out = NROW(corr))
plot_Dates <- rep(plot_Dates,NCOL(corr))
df <-data.frame(plot_Dates,corr1[,2],as.numeric(corr1[,3]))  
colnames(df) <- c("Time","Pair","value") 
title_text <- month.abb[mo]
titleFontSize <- 11
title_margin = margin(t=30,r=0,b=0,l=0)
axisFontSize <- 9  
# Line Thickness
lineSize <- 1.0
corrPlots <- ggplot(df) + ggtitle(title_text) 
corrPlots <- corrPlots  + geom_line(aes(x=as.Date(Time), y=as.numeric(value), color = Pair ), linewidth =lineSize, show.legend = TRUE) 
corrPlots



P1 <- cbind(c(1,0.3),c(0.3,1))
P2 <- cbind(c(1,0.7),c(0.7,1))
T <- 100
st <- seq(1:T)/T
speed <- 20
loc <- 0.5
G <- 1-exp(-speed/sd(st) *(st-loc)^2)
plot(G)
Pt <- P2[2,1]*(1-G)+P1[2,1]*G
plot(Pt)
