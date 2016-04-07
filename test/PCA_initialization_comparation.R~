
source("~/l1pcahp/src/class_data.R")
source("~/l1pcahp/src/l1pcahp.R")
source("~/l1pcahp/src/classifier.R")    #10cvknn algorithm
source("~/l1pcahp/src/l2pca.R")    #10cvknn algorithm

excecution_times <- 1

library("pcaL1")
library("pcaPP")
library("MASS")
library("pdist")
require(class) # required for knn



datasetnames <- c("balance","breastcancer","dermatology","heart","ionosphere","iris","liver","sonar","vehicle","yeast")
totFeatures <- c(3,8,10,10,10,4,6,10,10,8)

set.seed(12345)

result_matrix <- rep(0, 19)
count <- 0;
#shuffle the dataset
for (ds in datasetnames)
{
  cat(ds,"\n")
  exp_data <- classDataset(ds)
  data_set <- exp_data$attributes
  data_class <- exp_data$class
  count <- 1 + count;
  
  for (k in 1:excecution_times)
  {
    nseed <-sample(10^5,1,replace=TRUE)
    set.seed(nseed)
    gp <- runif(nrow(data_set))
    data_set <- data_set[order(gp),]
    data_class <- data_class[order(gp)]
    
    data_set <- scale(data_set, center = TRUE, scale = TRUE)
    
    for (projDim in 1:totFeatures[count]){
      ##### PCA #####
      cat("\n\n#####################\n",projDim," ",totFeatures[count],"\n")
      data_l2pca <- PCAproj(data_set,projDim)
      
      timepcaL1rd     <- system.time(data_pcal1rd <- pcal1(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE, initialize = "random"))
      timel1pcard     <- system.time(data_l1pcard <- l1pca(data_set,projDim = projDim,center = TRUE, projPoints = FALSE, initialize = "random", tolerance = 0.01, iterations = 10))
      
      timepcaL1pca     <- system.time(data_pcal1pca <- pcal1(data_set,projDim = projDim,center = TRUE, scores = TRUE, projPoints = FALSE, initialize = "l2pca"))
      timel1pcapca     <- system.time(data_l1pcapca <- l1pca(data_set,projDim = projDim,center = TRUE, projPoints = FALSE, initialize = "l2pca", tolerance = 0.01, iterations = 10))
      
      cat("\n random \n")
      timerd <- system.time(datard <- l1pcahp(data_set,projDim = projDim, center = TRUE, scores = TRUE,  initialize = "l2pca", threshold = 0.001))
      
      cat("\n pca \n")
      timepca <- system.time(datapca <- l1pcahp(data_set,projDim = projDim, center = TRUE, scores = TRUE,  initialize = "random", threshold = 0.001))
      
      
      differences <- c(sum(diag(matrix(pdist(datapca$loadings,datard$loadings)@dist,nrow = totFeatures[count]))),sum(diag(matrix(pdist(data_pcal1pca$loadings,data_pcal1rd$loadings)@dist,nrow = totFeatures[count]))),sum(diag(matrix(pdist(data_l1pcapca$loadings,data_l1pcard$loadings)@dist,nrow = totFeatures[count]))))
      tot <- c(sum(abs(datapca$loadings)),sum(abs(data_pcal1pca$loadings)),sum(abs(data_l1pcapca$loadings)))
      
      data_r <- list(datard$scores,datapca$scores)
      
      data_time <- c(timerd[3], timepca[3],timepcaL1rd[3], timepcaL1pca[3],timel1pcard[3], timel1pcapca[3])
      
      data_hyperplanes <-  c(datard$planes,datapca$planes)
      
      ##### KNN CLASSIFICATION #####  
      # apply the classificator to the dataset reduced
      resultC <- sapply (data_r, knn10cv, data_class = data_class) 
      result_matrix <- rbind(result_matrix, c(nseed,resultC, data_time,data_hyperplanes,differences,exp_data$id,projDim))
      write( c(nseed,resultC, data_time,data_hyperplanes,differences,tot,exp_data$id,projDim),file = "~/l1pcahp/fileout/streamoutput_threshold.txt", ncolumns = 19, append = TRUE, sep = ";") 
    }
  }
  result_matrix <- result_matrix[2:nrow(result_matrix),]
}





