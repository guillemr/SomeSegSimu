
##################################################################################################
## Yao's penalty 1989
listMethod[[i]] <- new("CptMethod", 
                       Name = "Fpop-Yao", 
                       Fun = function(y){
                         n <- length(y)
                         myGraphStd <- graph(penalty = 2*log(n), type = "std")
                         res <- gfpop(data = y,  weights = rep(1, n), 
                               mygraph = myGraphStd, type = "mean")
                         return(res$changepoints)
                       })
i <- i+1


##################################################################################################
## Yao's penalty 1989
listMethod[[i]] <- new("CptMethod", 
                       Name = "Fpop-Yao-Ha", 
                       Fun = function(y){
                         n <- length(y)
                         sd_y <- sdDiff(y, method="HALL")
                         y_ <- y/sd_y
                         myGraphStd <- graph(penalty = 2*log(n), type = "std")
                         res <- gfpop(data = y_,  weights = rep(1, n), 
                                      mygraph = myGraphStd, type = "mean")
                         return(res$changepoints)
                       })
i <- i+1


##################################################################################################
## Yao's penalty 1989 and minimum length on segments
listMethod[[i]] <- new("CptMethod", 
                       Name = "Fpop-Yao-L4", 
                       Fun = function(y){
                         n <- length(y)
                         myGraph <- getWaitGraph(4, penalty=2*log(n), K=Inf)
                         
                         res <- gfpop(data = y,  weights = rep(1, n), 
                                      mygraph = myGraph, type = "mean")
                         return(res$changepoints)
                       })
i <- i+1

##################################################################################################
## Lebarbier's 2005
listMethod[[i]] <- new("CptMethod", 
                       Name = "Fpop-Crops-Lb", 
                       Fun = function(y){
                         n   <- length(y); pen.init <- 2*log(n); Kmax <- trunc(min(n/2-1, 3*n/log(n)))
                         res <- CROPS.FPOP(y, weights=rep(1, length(y)),
                                           min_pen=pen.init - 2*log(Kmax)+3, max_pen=pen.init+3,
                                           THRS_CROPS=1e-3, min.len=1, K=Inf)
                         K.seg <- res[[1]][3,] + 1
                         cost  <- res[[1]][4,]
                         pen.shape <- 2*K.seg*log(n/K.seg) + 5*(K.seg)
                         Khat <- K.seg[which.min(cost+pen.shape)]
                         That <- which.min(cost+pen.shape)
                         return(res[[2]][[ That ]])
                       })
i <- i+1


listMethod[[i]] <- new("CptMethod", 
                       Name = "Fpop-Crops-Lb-L4", 
                       Fun = function(y){
                         n   <- length(y); pen.init <- 2*log(n); Kmax <- trunc(min(n/4-1, 3*n/log(n)))
                        
                         res <- CROPS.FPOP(y, weights=rep(1, length(y)),
                                           min_pen=pen.init - 2*log(Kmax)+3, max_pen=pen.init+3,
                                           THRS_CROPS=1e-3, min.len=4, K=Inf)
                         K.seg <- res[[1]][3,] + 1
                         cost  <- res[[1]][4,]
                         pen.shape <- 2*K.seg*log(n/K.seg) + 5*(K.seg)
                         Khat <- K.seg[which.min(cost+pen.shape)]
                         That <- which.min(cost+pen.shape)
                         return(res[[2]][[ That ]])
                       })
i <- i+1




##################################################################################################
## Lebarbier's 2005 Hall
listMethod[[i]] <- new("CptMethod", 
                       Name = "Fpop-Crops-Lb-Ha", 
                       Fun = function(y){
                         n   <- length(y); pen.init <- 2*log(n); Kmax <- trunc(min(n/2-1, 3*n/log(n)))
                         sd_y <- sdDiff(y, method="HALL")
                         y_ <- y/sd_y
                         res <- CROPS.FPOP(y_, weights=rep(1, length(y)),
                                           min_pen=pen.init - 2*log(Kmax)+3, max_pen=pen.init+3,
                                           THRS_CROPS=1e-3, min.len=1, K=Inf)
                         K.seg <- res[[1]][3,] + 1
                         cost  <- res[[1]][4,]
                         pen.shape <- 2*K.seg*log(n/K.seg) + 5*(K.seg)
                         Khat <- K.seg[which.min(cost+pen.shape)]
                         That <- which.min(cost+pen.shape)
                         return(res[[2]][[ That ]])
                       })
i <- i+1

##################################################################################################
## Lebarbier's 2005 Hall
listMethod[[i]] <- new("CptMethod", 
                       Name = "Fpop-Crops-Lb-Ha-L4", 
                       Fun = function(y){
                         n   <- length(y); pen.init <- 2*log(n); Kmax <- trunc(min(n/4-1, 3*n/log(n)))
                         sd_y <- sdDiff(y, method="HALL")
                         y_ <- y/sd_y
                         res <- CROPS.FPOP(y_, weights=rep(1, length(y)),
                                           min_pen=pen.init - 2*log(Kmax)+3, max_pen=pen.init+3,
                                           THRS_CROPS=1e-3, min.len=4, K=Inf)
                         K.seg <- res[[1]][3,] + 1
                         cost  <- res[[1]][4,]
                         pen.shape <- 2*K.seg*log(n/K.seg) + 5*(K.seg)
                         Khat <- K.seg[which.min(cost+pen.shape)]
                         That <- which.min(cost+pen.shape)
                         return(res[[2]][[ That ]])
                       })
i <- i+1

listMethod[[i]] <- new("CptMethod",
                       Name = "Fpop-Crops-Lb-Rob",
                       Fun = function(y){
                         n   <- length(y); pen.init <- 2*log(n); Kmax <- trunc(min(n/2-1, 3*n/log(n)))
                         res <- CROPS.FPOP(y, weights=rep(1, length(y)),
                                           min_pen=pen.init - 2*log(Kmax)+3, max_pen=pen.init+3,
                                           THRS_CROPS=1e-3, min.len=1, K=3^2)
                         K.seg <- res[[1]][3,] + 1
                         cost  <- res[[1]][4,]
                         pen.shape <- 2*K.seg*log(n/K.seg) + 5*(K.seg)
                         Khat <- K.seg[which.min(cost+pen.shape)]
                         That <- which.min(cost+pen.shape)
                         return(res[[2]][[ That ]])
                       })
i <- i+1

#####################
listMethod[[i]] <- new("CptMethod",
                       Name = "Fpsn-Lb",
                       Fun = function(y){
                         n <- length(y)
                         Kmax <- trunc(min(n/2-1, 3*n/log(n)))
                         res <- Fpsn(y, Kmax=Kmax)
                         K.seg <- 1:Kmax
                         cost  <- res$J.est
                         pen.shape <- 2*K.seg*log(n/K.seg) + 5*(K.seg)
                         Khat <- K.seg[which.min(cost+pen.shape)]
                         return(res$t.est[Khat, 1:Khat])
                       })
i <- i+1


# ##################################################################################################
# ## FDRseg

listMethod[[i]] <- new("CptMethod",
                       Name = "FDRseg-.1",
                       Fun = function(y){
                         n <- length(y)
                         alpha <- 0.1
                         ## compute quantile if not available
                         file.quantile <- paste("Simu_FDRseg/fdrseg_qtl", n, "alpha=", alpha, ".RData", sep="")
                         load(file.quantile)
                         ## segmentation
                         res <- fdrseg(y, q=qtl.fdrseg)
                         cpt <- c(res$left[-1]-1, length(y))
                         return(cpt)
                       })
i <- i+1





##################################################################################################
## BREAKFAST
## tguh
listMethod[[i]] <- new("CptMethod",
                       Name = "Bft-Tguh",
                       Fun = function(y){
                         w <- tguh.cpt(y)
                         w.cpt <- c(sort(w$cpt), length(y))
                         return(w.cpt)
                       })
i <- i+1
## hybrid
listMethod[[i]] <- new("CptMethod",
                       Name = "Bft-Hyb",
                       Fun = function(y){
                         w <- hybrid.cpt(y)
                         w.cpt <- c(sort(w$cpt), length(y))
                         return(w.cpt)
                       })
i <- i+1



##################################################################################################
## WBS
## Sic
listMethod[[i]] <- new("CptMethod",
                       Name = "Wbs-Sic",
                       Fun = function(y){
                         w <- wbs(y)
                         w.cpt <- changepoints(w, penalty="ssic.penalty")
                         w.cpt <- w.cpt$cpt.ic$ssic.penalty
                         if(is.na(w.cpt[1])) w.cpt <- c() else { w.cpt <- sort(w.cpt)}
                         return( c(w.cpt, length(y))  )
                       })
i <- i+1

# ## threshold 1
listMethod[[i]] <- new("CptMethod",
                       Name = "Wbs-Th1",
                       Fun = function(y){
                         w <- wbs(y)
                         w.cpt <- changepoints(w)
                         w.cpt <- w.cpt$cpt.th[[1]]
                         if(is.na(w.cpt[1])) w.cpt <- c() else { w.cpt <- sort(w.cpt)}
                         return( c(w.cpt, length(y)) )
                       })
i <- i+1


##################################################################################################
## WBS2
## threshold 0.9
listMethod[[i]] <- new("CptMethod", 
                       Name = "Wbs2-.9", 
                       Fun = function(y){
                         res <- wbs.sdll.cpt(y, lambda=0.9)
                         return( c(res$cpt, length(y)) )  
                       })
i <- i+1

listMethod[[i]] <- new("CptMethod", 
                       Name = "Wbs2-.95", 
                       Fun = function(y){
                         res <- wbs.sdll.cpt(y, lambda=0.95)
                         return( c(res$cpt, length(y)) )  
                       })
i <- i+1
##################################################################################################
## IDetect
listMethod[[i]] <- new("CptMethod", 
                       Name = "IDetect", 
                       Fun = function(y){
                         res <- ID(y)
                         if (res$no_cpt == 0) cpt <- length(y) else cpt <- c(res$cpt, length(y))
                         return( cpt )  
                       })
i <- i+1

##################################################################################################
## mosum
listMethod[[i]] <- new("CptMethod", 
                       Name = "mosum", 
                       Fun = function(y){
                         res <- multiscale.localPrune(y)
                         if (length(res$cpts) == 0) cpt <- length(y) else cpt <- c(res$cpts, length(y))
                         return( cpt )  
                       })
i <- i+1



##################################################################################################
## Yao's penalty 1989 + RFPOP with Th=3
listMethod[[i]] <- new("CptMethod", 
                       Name = "Rpop-Yao", 
                       Fun = function(y){
                         n <- length(y)
                         myGraphStd <- graph(penalty = 2*log(n), type = "std", K=3^2)
                         res <- gfpop(data = y,  weights = rep(1, n), 
                                      mygraph = myGraphStd, type = "mean")
                         return(res$changepoints)
                       })
i <- i+1


