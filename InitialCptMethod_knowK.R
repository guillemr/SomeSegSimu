listMethod_knownK <- list()
## correction for mBIC
i <- 1
listMethod_knownK[[i]] <- new("CptKnownKMethod", 
                       Name = "MaxLik-K*", 
                       Fun = function(y, K){
                         MLK_(y, K)
                       })
i <- i+1
listMethod_knownK[[i]] <- new("CptKnownKMethod", 
                              Name = "Bft-Wbs-K*", 
                              Fun = function(y, K){
                                res <- wbs.K.cpt(y, K=K-1)
                                c(res$cpt, length(y))
                              })
i <- i+1

listMethod_knownK[[i]] <- new("CptKnownKMethod",
                              Name = "BinSeg-K*",
                              Fun = function(y, K){
                                w <- sbs(y)
                                if(K > 1){
                                w.cpt <- sort(changepoints(w, Kmax=K)$cpt.th[[1]][1:(K-1)])
                                } else {
                                w.cpt <- c()  
                                }
                                
                                c(w.cpt, length(y))
                              })
i <- i+1


listMethod_knownK[[i]] <- new("CptKnownKMethod",
                              Name = "Rpop-K*",
                              Fun = function(y, K){
                                ## prepare graph
                                if(K > 1 & K <= 101){
                                  K <- K-1
                                  emptyGraph <- graph()
                                  for(i in 1:K){
                                    emptyGraph <- graph(emptyGraph, Edge((i-1), i, "std", K=3^2))
                                  }
                                  for(i in 0:K){
                                    emptyGraph <- graph(emptyGraph, Edge(i, i, "null", K=3^2))
                                  }
                                  emptyGraph <- graph(emptyGraph, StartEnd(start = 0, end = K))

                                  ## run
                                  res <- gfpop(data =  y, mygraph = emptyGraph, type = "mean")
                                  cpt <- res$changepoints
                                } else{ ## no change
                                  cpt <- length(y)
                                }
                                c(cpt)
                              })
i <- i+1
