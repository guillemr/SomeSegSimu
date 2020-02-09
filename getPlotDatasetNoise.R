getPlotDatasetNoise <- function(noise, append_, dataset, mName_){
  allfiles <- list.files("simulated_dataset/Res", pattern = paste0(dataset@Name, "_", append_, ".*_Noise=", noise),
                         full.names = T)
  #############################################################
  ## we load one data set of factor 1 in 
  load(allfiles[grep("_1_Noise=", allfiles)])
  load(toSaveRs$description@fileDt)
  dataY <- data.frame(x=1:length(toSaveDt$allDatasets[[1]]$data), y=toSaveDt$allDatasets[[1]]$data, smt=toSaveDt$description@signal)
  
  if(append_ == "s") scenario <- "increasing variance"
  if(append_ == "x") scenario <- "increasing length"
  plot_data <- ggplot(dataY, aes(x=x, y=y)) + geom_point(size=0.2) + geom_line(aes(x=x, y=smt, color="red")) + 
    geom_vline(xintercept = toSaveDt$description@bkp, color="blue") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste0("Dataset: ", dataset@Name, ", Scenario: ",scenario))
  #############################################################
  allscores <- do.call(rbind, lapply(allfiles, function(file_){
    load(file_)
    dat <- data.frame(rbindlist(toSaveRs$allMethods))
    dat$n <- toSaveRs$description@Lg
    dat$sd <- toSaveRs$description@sigma
    dat
  }))
  
  allscores <- allscores[allscores$method %in% mName_, ]
  
  mean_dat <- aggregate(allscores[, 1:4], by=list(allscores$method, allscores$n, allscores$sd), FUN=mean)
  mean_dat <- mean_dat[order(mean_dat[, 1], mean_dat[, 2], mean_dat[, 3]), ]
  colnames(mean_dat)[1:3] <- c("met", "n", "sd")

  
  scaleFUN <- function(x) sprintf("%.2f", x)
  
  if(append_ == "x"){
    p1 <- ggplot(mean_dat, aes(x=n, y=mse, color=met)) + geom_line()+geom_point(aes(shape=met)) + scale_shape_manual(values=c(1, 13, 2, 3, 4, 15, 19)) +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+ggtitle(noise) + scale_y_continuous(trans='log2', labels = function(x) format(x, scientific = TRUE, digits=2))
     
    p2 <- ggplot(mean_dat, aes(x=n, y=K, color=met)) + geom_line()+geom_point(aes(shape=met))  + scale_shape_manual(values=c(1, 13, 2, 3, 4, 15, 19)) +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+ggtitle(noise)
    
    p3 <- ggplot(mean_dat, aes(x=n, y=ari, color=met)) + geom_line()+geom_point(aes(shape=met)) + scale_shape_manual(values=c(1, 13, 2, 3, 4, 15, 19)) +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+ggtitle(noise)
    p4 <- ggplot(mean_dat, aes(x=n, y=time, color=met)) + geom_line()+geom_point(aes(shape=met)) + scale_shape_manual(values=c(1, 13, 2, 3, 4, 15, 19)) +
      ggtitle(noise)+ scale_y_continuous(trans='log2', labels = function(x) format(x, scientific = TRUE, digits=2))+ 
      guides(col = guide_legend(ncol=2)) + theme(plot.title = element_text(hjust = 0.5))
  }
  
  if(append_ == "s"){
    p1 <- ggplot(mean_dat, aes(x=sd, y=mse, color=met)) + geom_line()+geom_point(aes(shape=met)) + scale_shape_manual(values=c(1, 13, 2, 3, 4, 15, 19)) + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +ggtitle(noise) + 
      scale_y_continuous(trans='log2', labels = function(x) format(x, scientific = TRUE, digits=2))
    p2 <- ggplot(mean_dat, aes(x=sd, y=K, color=met)) + geom_line()+geom_point(aes(shape=met)) + scale_shape_manual(values=c(1, 13, 2, 3, 4, 15, 19)) + 
      theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+ggtitle(noise)
    p3 <- ggplot(mean_dat, aes(x=sd, y=ari, color=met)) + geom_line()+geom_point(aes(shape=met)) + scale_shape_manual(values=c(1, 13, 2, 3, 4, 15, 19)) + 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+ggtitle(noise)
    p4 <- ggplot(mean_dat, aes(x=sd, y=time, color=met)) + geom_line()+geom_point(aes(shape=met)) + scale_shape_manual(values=c(1, 13, 2, 3, 4, 15, 19)) +
      ggtitle(noise)+ scale_y_continuous(trans='log2', labels = function(x) format(x, scientific = TRUE, digits=2))+ 
      guides(col = guide_legend(ncol=2))+theme(plot.title = element_text(hjust = 0.5))
  }
  
  legend <- cowplot::get_legend(p4)
  p4 <- p4 + theme(legend.position = "none")
  
  
  return(list(data.ex=plot_data, mse=p1, k=p2, ari=p3, time=p4, legend=legend))
}