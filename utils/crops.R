##CROPS for FPOP

getWaitGraph <- function(min.len, penalty, K=Inf){
  emptyGraph <- graph()
  if(min.len == 1){
    return(graph(penalty = penalty, type = "std", K=K)) ## classic graph
  }
  
  if(min.len >= 2){
    emptyGraph <- graph()
    emptyGraph <- graph(emptyGraph, Edge(1, 1, "null", K=K))
    emptyGraph <- graph(emptyGraph, Edge(1, 2, penalty=penalty, "std", K=K))
    if(min.len >=3){
    for(i in 3:(min.len)){
      emptyGraph <- graph(emptyGraph, Edge((i-1), i, "null", K=K))
    }}
    emptyGraph <- graph(emptyGraph, Edge(min.len, 1, "null", K=K))
    return(emptyGraph)
  }
  
}

CROPS.FPOP <- function(data, weights, min_pen=5, max_pen=20, THRS_CROPS=1e-3, min.len=1, K=Inf) {
  NCALC=0
  pen_interval <- c(min_pen,max_pen)
  n <- length(data)
  
  test_penalties <- NULL
  numberofchangepoints <- NULL
  penal <- NULL
  overall_cost <- array()
  segmentations <- NULL
  #thetas=NULL
  b_between <- array()  

  count <- 0 
  
  while (length(pen_interval) > 0){
    
    new_numcpts <- array()
    new_penalty <- array()
    new_cpts <- array()
    #new.theta<-array()
    
    for (b in 1:length(pen_interval)) {
      #ans <- Rob_seg.std(data, loss="L2", lambda=pen_interval[b])
      # UNIQUE CALL TO GFPOP
      ans <- gfpop(data = data,  
                   mygraph = getWaitGraph(min.len=min.len, penalty = pen_interval[b], K=K), 
                   type = "mean")
      
      if(min.len >= 2){ ## adjust globalCost temporary gfpop
        ans$globalCost <- (ans$globalCost - (length(ans$changepoints)-1)*pen_interval[b])
      }
      resultingcpts <- ans$changepoints 
      ans$J.est <- ans$globalCost #- pen_interval[b]*(length(resultingcpts)-1)
      new_numcpts[b] <- length(resultingcpts)-1
      cost.test <- array()
      new_cpts[b] <- list(resultingcpts)
     # new.theta[b]=list(ans[[5]])
      new_penalty[b] <- ans$J.est
    }
    
    if (count == 0){
    print(paste("Maximum number of runs of algorithm = ", new_numcpts[1] - new_numcpts[2] + 2, sep = ""))
    count <- count + length(new_numcpts)
    print(paste("Completed runs = ", count, sep = ""))
    }else{
      count <- count + length(new_numcpts)
      print(paste("Completed runs = ", count, sep = ""))
    }
    
    ## Add the values calculated to the already stored values
    test_penalties <- unique((sort(c(test_penalties,pen_interval))))
    new_numcpts <- c(numberofchangepoints,new_numcpts)
    new_penalty <- c(penal,new_penalty)
    
    new_cpts <- c(segmentations,new_cpts)
    #new.theta <- c(thetas,new.theta)
    numberofchangepoints <- -sort(-new_numcpts) ##can use sort to re-order
    penal <- sort(new_penalty)
    
    ls <- array()
    
    for (l in 1:length(new_cpts)){
      ls[l] <- length(new_cpts[[l]])
    }
    
    
    ls1 <- sort(ls,index.return = T, decreasing = T)
    ls1 <- ls1$ix
    
    
    segmentations <- new_cpts[c(ls1)]
    #thetas=new.theta[c(ls1)]
    
    pen_interval <- NULL
    tmppen_interval <- NULL
     
    for (i in 1:(length(test_penalties)-1)){
      if(abs(numberofchangepoints[i]-numberofchangepoints[i+1])>1){ ##only need to add a beta if difference in cpts>1
        j <- i+1
        tmppen_interval <- (penal[j] - penal[i]) * ((numberofchangepoints[i] - numberofchangepoints[j])^-1)
        pen_interval <- c(pen_interval, tmppen_interval )
       }
     }
      
      
      if(length(pen_interval)>0){
        for(k in length(pen_interval):1){
          if(min(abs(pen_interval[k]-test_penalties))< THRS_CROPS) {
             pen_interval=pen_interval[-k]
          }
        }
      }
    }
        
  ##PRUNE VALUES WITH SAME num_cp
  for(j in length(test_penalties):2){
    if(numberofchangepoints[j]==numberofchangepoints[j-1]){
      numberofchangepoints=numberofchangepoints[-j]
      test_penalties=test_penalties[-j]
      penal=penal[-j]
      segmentations = segmentations[-j]
      #thetas=thetas[-j]
        }
  }
  
  
  
  ###calculate beta intervals
  nb=length(test_penalties)
  beta.int=rep(0,nb)
  beta.e=rep(0,nb)
  for(k in 1:nb){
    if(k==1){
      beta.int[1]=test_penalties[1]
    }else{
      beta.int[k]=beta.e[k-1]
    }
    if(k==nb){
      beta.e[k]=test_penalties[k]
    }else{
      beta.e[k]=(penal[k]-penal[k+1])/(numberofchangepoints[k+1]-numberofchangepoints[k])
    }
    
  }
   
  return(list(rbind(test_penalties,beta.int,numberofchangepoints,penal),segmentations))
}
    


