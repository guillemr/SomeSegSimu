library(aricode) ## used for scoring
###### define class
DatasetDesc <- setClass("DatasetDesc", representation(
  Name   = "character",
  bkp    = "integer",
  Lg     = "integer",
  mu     = "numeric", 
  sigma  = "numeric", 
  signal = "numeric",
  segNb  = "integer",
  K      = "integer",
  noise  = "character",
  fileDt = "character",
  fileRs = "character"
),
  prototype(
    Name   = NA_character_, bkp    = NA_integer_,
    Lg     = NA_integer_  , mu     = NA_real_,
    sigma  = NA_real_     , signal = NA_real_, 
    segNb  = NA_integer_  , K      = NA_integer_,
    noise  = "Gauss"      , fileDt = NA_character_,
    fileRs = NA_character_
  )
)

##################################################################################################
#### COMPLETE DATASET-DESC INFO
##################################################################################################
setGeneric(name="complete", def=function(object){ standardGeneric("complete") })
setMethod("complete", signature="DatasetDesc", 
  function(object){
    object@Lg     <- object@bkp[length(object@bkp)]
    object@signal <- rep(object@mu, diff(object@bkp)) 
    object@K      <- as.integer(length(object@bkp)-1)
    object@segNb  <- rep(1:object@K, diff(object@bkp))
    return(object)
  }
)

##################################################################################################
#### NOISE & SCALE & EXPAND DATASET
##################################################################################################

setGeneric(name="noiseDesc", def=function(object, noise) {standardGeneric("noiseDesc")})
setMethod(f="noiseDesc",
          signature="DatasetDesc",
          definition=function(object, noise)
          {
            if( noise %in% c("Gauss", "Stud", "ARMA")){
               object@noise  <- noise
            }
            return(object)
          }
)

setGeneric(name="scaleDesc", def=function(object, scaleBy) {standardGeneric("scaleDesc")})
setMethod(f="scaleDesc",
          signature="DatasetDesc",
          definition=function(object, scaleBy)
          {
            object@Name   <- paste0(object@Name, "_s_", scaleBy)
            object@sigma  <- object@sigma*scaleBy
            return(object)
          }
)

setGeneric(name="expandDesc", def=function(object, expandBy) {standardGeneric("expandDesc")})
setMethod(f="expandDesc",
          signature="DatasetDesc",
          definition=function(object, expandBy)
          {
            object@Name   <- paste0(object@Name, "_x_", expandBy)
            object@bkp    <- as.integer(object@bkp*expandBy)
            object@sigma  <- object@sigma*sqrt(expandBy)
            object@mu     <- object@mu
            object        <- complete(object)
            return(object)
            }
)


##################################################################################################
#### SIMULATE DATASET
##################################################################################################

setGeneric(name="simulation", def=function(object) {standardGeneric("simulation") })
setMethod(f="simulation",
          signature="DatasetDesc",
          definition=function(object)
          {
            ##############################################################################
            if(object@noise == "Gauss"){
              y <- object@signal + rnorm(object@Lg, sd=object@sigma)
              return(y)
            }
            ##############################################################################
            if(object@noise == "Stud"){
              dfree <- 10
              y <- object@signal + rt(n=object@Lg, df=dfree)*object@sigma
              return(y)
            }
            ##############################################################################
            if(object@noise == "ARMA"){
              arima.model <- list(order=c(1,0,0), ar=0.3)
              y <- object@signal + arima.sim(arima.model, n=object@Lg)*object@sigma
              return(as.numeric(y))
            }
            ##############################################################################
          }
)

setGeneric(name="simulateMany", def=function(object, dir="simulated_dataset", nrepeat) {standardGeneric("simulateMany") })
setMethod(f="simulateMany",
          signature="DatasetDesc",
          definition=function(object, dir, nrepeat)
          {
            
            object@fileDt <- paste0(dir, "/Data/Name=", object@Name, "_Noise=", object@noise, 
                                    "_Sigma=", object@sigma, "_K=", object@K, "_Lg=", object@Lg,
                                    ".Rdata")
            
            object@fileRs <- paste0(dir, "/Res/Name=", object@Name, "_Noise=", object@noise, 
                                    "_Sigma=", object@sigma, "_K=", object@K, "_Lg=", object@Lg,
                                    ".Rdata")
            
            ##
            getSdDiff <- function(data){
              n = length(data)
              return(mad(diff(data)/sqrt(2)))	
            }
            ## if necessary create dataset
            if(!file.exists(object@fileDt) | !file.exists(object@fileRs)){
              allDatasets <- replicate(nrepeat, simulation(object), simplify = F)
              allDatasets <- lapply(allDatasets, FUN=function(y) list(data=y, sd.est = getSdDiff(y)))
              allMethods  <- list()
            
            
              toSaveDt <- list(description=object, allDatasets=allDatasets)
              toSaveRs <- list(description=object, allMethods=allMethods)
              save(toSaveDt, file=object@fileDt)
              save(toSaveRs, file=object@fileRs)
            }
            return(object)
             
          }
)
##################################################################################################
#### SCORE DATASET
##################################################################################################

setGeneric(name="score", def=function(object, metOutput, y) {standardGeneric("score") })
setMethod(f="score",
          signature="DatasetDesc",
          definition=function(object, metOutput, y)
          {
            cpt <- metOutput$cpt
            ## prepare
            lengthSeg <- diff(c(0, cpt))
            Khat     <- length(lengthSeg)
            segmentNb  <- rep(1:Khat, lengthSeg)
            smt       <- rep(by(y, segmentNb, FUN=mean), lengthSeg)
            
            ## ARI
            ari.score <- NA ; if(object@K > 1) ari.score <- ARI(object@segNb, segmentNb)
                
            
            ## MSE
            lineScore <- data.frame(
              K   = abs(Khat - object@K),
              mse = mean( (object@signal - smt)^2 / object@sigma),
              ari = 1- ari.score,
              time=metOutput$runtime
            )
            
            return(lineScore)
            
          }
)

##################################################################################################
#### CLEAN FUNCTIONS
##################################################################################################

remove.simulated.datasets <- function(dir){
  allfiles <- list.files(paste0(dir, "/Data/"), full.names = T)
  lapply(allfiles, FUN=file.remove)
  allfiles <- list.files(paste0(dir, "/Res/"), full.names = T)
  lapply(allfiles, FUN=file.remove)
}

reset.results.datasets <- function(dir, metName){
  allfiles <- list.files(paste0(dir, "/Res/"), full.names = T)
  lapply(allfiles, FUN=function(file_){
    load(file_)
    toSaveRs$allMethods[[metName]]
    toSaveRs$allMethods[[metName]] <- NULL
    save(toSaveRs, file=file_)
  })
  
}