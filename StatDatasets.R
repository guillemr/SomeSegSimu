
######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-2a, K=7, n=497"
Simu[[isimu]]$bkp <-c(0, c(139, 226, 243, 300, 309, 333)-1, 497)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- c(-0.18, 0.08, 1.07, -0.53, 0.16, -0.69,-0.16)
Simu[[isimu]]$sigma=0.3; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1

######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-3, K=14, n=560"
Simu[[isimu]]$bkp <-c(0, c(11, 21, 41, 61, 91, 121, 161, 201, 251, 301, 361, 421, 491)-1, 560)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- c(7, -7, 6, -6, 5, -5, 4, -4, 3, -3, 2, -2, 1, -1)
Simu[[isimu]]$sigma=4; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1

######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-4a, K=14, n=140"
Simu[[isimu]]$bkp <- c(0, c(11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111, 121, 131)-1, 140)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
Simu[[isimu]]$sigma=0.4; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1

######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-4b, K=14, n=140"
Simu[[isimu]]$bkp <- c(0, c(11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111, 121, 131)-1, 140)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
Simu[[isimu]]$sigma=0.5; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1


######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-5a, K=15, n=150"
Simu[[isimu]]$bkp <-c(0, c(11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111, 121, 131, 141)-1, 150)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- c(1:15)
Simu[[isimu]]$sigma=0.3; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1

######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-5b, K=50, n=500"
Simu[[isimu]]$bkp <- seq(0, 500, by=10)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- 1:50
Simu[[isimu]]$sigma <- 1/sqrt(5); 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1

######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-6a, K=200, n=1000"
Simu[[isimu]]$bkp <- seq(0, 1000, by=5)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- rep(c(0, 1), 100)
Simu[[isimu]]$sigma <- 0.2; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1

######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-6b, K=100, n=1000"
Simu[[isimu]]$bkp <- seq(0, 1000, by=10)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- rep(c(0, 1), 50)
Simu[[isimu]]$sigma <- 0.35; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1

######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-6c, K=50, n=1000"
Simu[[isimu]]$bkp <- seq(0, 1000, by=20)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- rep(c(0, 1), 25)
Simu[[isimu]]$sigma <- 0.5; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1

######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-7, K=10, n=2000"
Simu[[isimu]]$bkp <- seq(0, 2000, by=200)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- rep(c(0, 1), 5) ## n=10000 mu=0.5
Simu[[isimu]]$sigma <- 1; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1

######################################################################################################
isimu <- isimu+1
Simu[[isimu]] <- list()
Simu[[isimu]]$Name <- "Dataset-8, K=1, n=1000"
Simu[[isimu]]$bkp <- c(0, 1000)
Simu[[isimu]]$Lg <- diff(Simu[[isimu]]$bkp)
Simu[[isimu]]$mu <- 
Simu[[isimu]]$sigma <- 1; 
Simu[[isimu]]$signal <- rep(Simu[[isimu]]$mu, Simu[[isimu]]$Lg); 
Simu[[isimu]]$Ktrue <- sum(diff(Simu[[isimu]]$signal)!=0)+1
