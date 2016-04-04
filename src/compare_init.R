compPCA <- function(ds, shuffN = 10, maxfeature, minfeature = 1,startseed = 12345, init = "l2pca")
{
  #import functions
  source("~/l1pcahp/src/class_data.R") #dataset manager
  source("~/l1pcahp/src/l1pcahp.R")    #l1pcahp function
  source("~/l1pcahp/src/classifier.R")    #10cvknn algorithm
  library("pcaL1")
  library("MASS")
  
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
      timel2pca <- system.time(data_l1plcahp <- l1pcahp(data_set,projDim = projDim, center = TRUE, scores = TRUE,  initialize = "l2pca", threshold = 0.001))
      timerand <- system.time(data_l1plcahp <- l1pcahp(data_set,projDim = projDim, center = TRUE, scores = TRUE,  initialize = "l2pca", threshold = 0.001))
      
      data_r <- list(data_pcal1$scores,data_l1pca$scores,data_l1pcastar$scores,data_l1plcahp$scores)
      data_time <- c(timepcaL1[3], timel1pca[3], timestar[3],timehp[3])
      
      ##### KNN CLASSIFICATION #####  
      # apply the classificator to the dataset reduced
      resultC <- sapply (data_r, knn10cv, data_class = data_class) 
      
      write( c(nseed,resultC, data_time,exp_data$id,projDim),file = "~/l1pcahp/fileout/streamoutput.txt", ncolumns = 11, append = TRUE, sep = ";") 
    }
  }
  
  
  
}