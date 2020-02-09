###### define class
CptMethod <- setClass("CptMethod", representation(
  Name   = "character",
  Fun    = "function"
))

##################################################################################################
#### RUN
##################################################################################################
setGeneric(name="runMethod", def=function(object, y){ standardGeneric("runMethod") })
setMethod("runMethod", signature="CptMethod", 
          function(object, y){
            runtime <- system.time(cpt <- object@Fun(y))[3];
            return(list(cpt=cpt, runtime=runtime))
          }
)

##################################################################################################
#
#
#
#
##################################################################################################
###### define class for known K
CptKnownKMethod <- setClass("CptKnownKMethod", representation(
  Name   = "character",
  Fun    = "function"
))

##################################################################################################
#### RUN
##################################################################################################
setGeneric(name="runMethodK", def=function(object, y, K){ standardGeneric("runMethodK") })
setMethod("runMethodK", signature="CptKnownKMethod", 
          function(object, y, K){
            runtime <- system.time(cpt <- object@Fun(y, K))[3];
            return(list(cpt=cpt, runtime=runtime))
          }
)
