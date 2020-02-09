##################################################################################################
#### LOAD PACKAGES AND HELP FUNCTION
##################################################################################################
source("utils/estimateVar.R")
library(gfpop)
source("utils/crops.R")
library(changepoint)
library(jointseg)
library(FDRSeg)
library(breakfast)
library(wbs)
source("utils/WBS2_SDLL_github_v3.R")
library(IDetect)
library(mosum)
library(capushe)


MLK_ <- function(y, Kstar){
  n <- length(y)
  Kmax <- Kstar
  res <- Fpsn(y, Kmax=Kmax+1)
  K.seg <- 1:Kmax
  cost  <- res$J.est
  pen.shape <- 2*K.seg*log(n/K.seg) + 5*(K.seg)
  Khat <- Kstar
  return(res$t.est[Khat, 1:Khat])                         
}
MLK <- function(y, Kstar){
  runtime <- system.time(cpt <- MLK_(y, Kstar))[3];
  return(list(cpt=cpt, runtime=runtime))
  
}
getWbsK <- function(x, Kgoal){
  res <- wbs.K.cpt(x, K=Kgoal-1)
  return(c(res$cpt, length(x)))
}
