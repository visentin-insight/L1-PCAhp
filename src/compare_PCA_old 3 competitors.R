compPCA <- function(ds, shuffN = 10, maxfeature, minfeature = 1,startseed = 12345)
{
  #import functions
  source("~/l1pcahp/src/class_data.R") #dataset manager
  source("~/l1pcahp/src/l1pcahp.R")    #l1pcahp function
  source("~/l1pcahp/src/classifier.R")    #10cvknn algorithm
  library("pcaL1")
  library("MASS")
  
  result_matrix <- rep(0, 11)
  
  #shuffle the dataset
  
  exp_data <- classDataset(ds)
  data_set <- exp_data$attributes
  data_class <- exp_data$class
  for (k in 1:shuffN)
  {
    nseed <-sample(10^5,1,replace=TRUE)
    set.seed(nseed)
    gp <- runif(nrow(data_set))
    data_set <- data_set[order(gp),]
    data_class <- data_class[order(gp)]
    
    data_set <- scale(data_set, center = TRUE, scale = TRUE)
    
    for (projDim in minfeature:maxfeature){
      ##### PCA #####
      timepcaL1 <- system.time(data_pcal1 <- pcal1(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE, initialize = "l2pca"))
      timel1pca <- system.time(data_l1pca <- l1pca(data_set,projDim = projDim,center = TRUE, projPoints = FALSE, initialize = "l2pca", tolerance = 0.0001, iterations = 10))
      timestar <- system.time(data_l1pcastar <- l1pcastar(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE))
      timehp <- system.time(data_l1plcahp <- l1pcahp(data_set,projDim = projDim, center = TRUE, scores = TRUE,  initialize = "l2pca", threshold = 0.001))
      
      data_r <- list(data_pcal1$scores,data_l1pca$scores,data_l1pcastar$scores,data_l1plcahp$scores)
      data_time <- c(timepcaL1[3], timel1pca[3], timestar[3],timehp[3])
      
      ##### KNN CLASSIFICATION #####  
      # apply the classificator to the dataset reduced
      resultC <- sapply (data_r, knn10cv, data_class = data_class) 
      result_matrix <- rbind(result_matrix, c(nseed,resultC, data_time,exp_data$id,projDim))
      
      write( c(nseed,resultC, data_time,exp_data$id,projDim),file = "~/fileout/streamoutput.txt", ncolumns = 11, append = TRUE, sep = ";") 
    }
  }
  result_matrix <- result_matrix[2:nrow(result_matrix),]
  
  write.table(result_matrix, file = paste("~/fileout/res_",ds,"_",shuffN,".txt"), col.names = c("seed","Acc. pcal1","Acc. l1pca","Acc. l1pcastar","Acc. l1pcahp","Time pcal1","Time l1pca","Time l1pcastar","Time l1pcahp", "Id dataset", "ProjDim"),sep = ";")
  
  
  
  
}