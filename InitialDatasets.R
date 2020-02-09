
########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt2", 
                               bkp   = as.integer(c(0, 138, 225, 242, 299, 308, 332, 497)),
                               mu    = c(-0.18, 0.08, 1.07, -0.53, 0.16, -0.69,-0.16),
                               sigma = 0.3
)
i <- i+1

########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt3", 
                               bkp   = as.integer(c(0, 10, 20, 40, 60, 90, 120, 160, 200, 250, 300, 360, 420, 490, 560)),
                               mu    = c(7, -7, 6, -6, 5, -5, 4, -4, 3, -3, 2, -2, 1, -1),
                               sigma = 4
)
i <- i+1

########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt4", 
                               bkp   = as.integer(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140)),
                               mu    = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
                               sigma = 0.4
)
i <- i+1
# 
# initialDatasetDesc[[i]] <- new("DatasetDesc", 
#                                Name  = "Dt4plus2", 
#                                bkp   = as.integer(c(0, c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130)+2, 140)),
#                                mu    = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
#                                sigma = 0.4
# )
# i <- i+1


########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt5a", 
                               bkp   = as.integer(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)),
                               mu    = 1:15,
                               sigma = 0.3
)
i <- i+1

# initialDatasetDesc[[i]] <- new("DatasetDesc", 
#                                Name  = "Dt5aplus2", 
#                                bkp   = as.integer(c(0, c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140)+2, 150)),
#                                mu    = 1:15,
#                                sigma = 0.3
# )
# i <- i+1
########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt5b", 
                               bkp   = as.integer(seq(0, 500, by=10)),
                               mu    = 1:50,
                               sigma = 0.45 # approx 1/sqrt(5)
)
i <- i+1
########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt6a", 
                               bkp   = as.integer(seq(0, 1000, by=5)),
                               mu    = rep(c(0, 1), 100),
                               sigma = 0.2
)
i <- i+1

########################
initialDatasetDesc[[i]] <- new("DatasetDesc",
                               Name  = "Dt6a2",
                               bkp   = as.integer(c(0, seq(105, 200, by=5), seq(605, 700, by=5), 1000)),
                               mu    = c(rep(c(0, 1), 20), 0),
                               sigma = 0.2
)
i <- i+1

########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt6b", 
                               bkp   = as.integer(seq(0, 1000, by=10)),
                               mu    = rep(c(0, 1), 50),
                               sigma = 0.35
)
i <- i+1

########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt6c", 
                               bkp   = as.integer(seq(0, 1000, by=20)),
                               mu    = rep(c(0, 1), 25),
                               sigma = 0.5
)
i <- i+1
########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt6d", 
                               bkp   = as.integer(seq(0, 1000, by=100)),
                               mu    = rep(c(0, 1), 5),
                               sigma = 1
)
i <- i+1
########################
initialDatasetDesc[[i]] <- new("DatasetDesc", 
                               Name  = "Dt7", 
                               bkp   = as.integer(c(0, 1000)),
                               mu    = 0,
                               sigma = 1
)
